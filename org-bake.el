;;; org-bake.el --- Bake Org projects into stable JSON artifacts  -*- lexical-binding: t; -*-

;; Author: bitbloxhub <https://github.com/bitbloxhub>
;; Maintainer: bitbloxhub <https://github.com/bitbloxhub>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (async "1.9.9") (ox-json "1"))
;; Keywords: outlines, tools, data
;; URL: https://github.com/bitbloxhub/org-bake
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: pi:gpt-5.4
;; Assisted-by: pi:gpt-5.3-codex

;;; Commentary:

;; org-bake keeps configured Org workspaces incrementally exported to stable JSON
;; documents, then builds async materialized views from those documents
;; (for example ids, agenda rows, and other derived indexes).
;;
;; Runtime behavior is controlled by `org-bake-mode': enabling mode installs
;; watchers/advice/startup scheduling; disabling mode removes them and stops
;; background workers.

;;; Code:

(require 'org-bake-process)
(require 'org-bake-store)
(require 'org-bake-project)
(require 'org-bake-export)
(require 'org-bake-materialize)
(require 'filenotify)
(require 'seq)
(defgroup org-bake nil
  "Bake Org projects into stable JSON artifacts."
  :group 'tools
  :prefix "org-bake-")

(defcustom org-bake-store-root nil
  "Base directory used for org-bake workspace stores.

When nil, org-bake stores each workspace under
`$XDG_DATA_HOME/org-bake/' or `~/.local/share/org-bake/' when
`XDG_DATA_HOME' is unset.  When non-nil, each workspace store lives under
this directory as NAME/."
  :type '(choice (const :tag "Use XDG data home" nil) directory)
  :group 'org-bake)

(defcustom org-bake-workspaces nil
  "Workspace definitions for org-bake.

Each entry is of the form:

  (NAME :roots (DIR...) [:store-dir DIR] ...)

where NAME is a symbol, :roots is a list of root directories, `:store-dir'
optionally overrides that workspace's full store directory, and the default
workspace store location is computed from `org-bake-store-root'."
  :type '(alist :key-type symbol :value-type (plist :value-type sexp))
  :group 'org-bake)

(defcustom org-bake-auto-index-on-startup t
  "When non-nil, start org-bake indexing in background after startup.

This schedules a small startup coordinator that scans each configured
workspace, enqueues stale export jobs, and starts async exports."
  :type 'boolean
  :group 'org-bake)

(defcustom org-bake-index-interval nil
  "Deprecated polling interval for org-bake.

Directory watches handle ongoing indexing.  Keep nil."
  :type '(choice (const :tag "Use directory watches" nil) number)
  :group 'org-bake)

(defcustom org-bake-max-export-jobs 8
  "Maximum concurrent async export jobs per workspace.

Lower this to avoid hitting file descriptor limits on large workspaces."
  :type 'integer
  :group 'org-bake)

(defcustom org-bake-export-batch-size 32
  "Baseline documents each async export worker processes per batch.

Used as initial per-worker batch size when dynamic sizing is enabled."
  :type 'integer
  :group 'org-bake)

(defcustom org-bake-export-dynamic-batch-size t
  "When non-nil, adapt per-worker export batch size using observed throughput."
  :type 'boolean
  :group 'org-bake)

(defcustom org-bake-export-batch-size-min 8
  "Minimum per-worker export batch size when dynamic sizing is enabled."
  :type 'integer
  :group 'org-bake)

(defcustom org-bake-export-batch-size-max 256
  "Maximum per-worker export batch size when dynamic sizing is enabled."
  :type 'integer
  :group 'org-bake)

(defcustom org-bake-export-batch-target-seconds 1.5
  "Target wall-clock seconds for each export batch when dynamic sizing is enabled."
  :type 'number
  :group 'org-bake)

(defcustom org-bake-export-batch-throughput-alpha 0.3
  "EWMA smoothing factor for dynamic export throughput estimates."
  :type 'number
  :group 'org-bake)

(defcustom org-bake-export-resolve-id-links nil
  "When non-nil, resolve `id:' links through `org-id' during export.

When nil, org-bake skips org-id link lookups in export prepass.
Unresolvable id links are emitted as broken-link markers."
  :type 'boolean
  :group 'org-bake)

(defcustom org-bake-max-materialize-jobs 4
  "Maximum materializer jobs per child materialization process.

Materializers still run in a single child Emacs process at a time per
workspace."
  :type 'integer
  :group 'org-bake)

(defcustom org-bake-materialize-debounce-seconds 0.5
  "Seconds to debounce workspace materialization scheduling.

Lower this to rebuild materializations sooner after export queues drain."
  :type 'number
  :group 'org-bake)

(defvar org-bake--startup-indexer-scheduled nil
  "Non-nil when org-bake startup indexer has already been scheduled.")

(defvar org-bake--index-timer nil
  "Optional fallback polling timer for org-bake.")

(defvar org-bake--workspace-watch-descriptors nil
  "Alist mapping workspace symbols to file notification descriptors.")
(defvar org-bake--workspace-refresh-timers nil
  "Alist mapping workspace symbols to pending refresh timers.")

(defcustom org-bake-org-id-use-materialized-ids t
  "When non-nil, resolve Org `id:' links through org-bake ids materialization."
  :type 'boolean
  :group 'org-bake)

(defcustom org-bake-agenda-generated-file-name "agenda-view.org"
  "Generated Org file name for workspace agenda views.

This file is written under each workspace store directory."
  :type 'string
  :group 'org-bake)

(defcustom org-bake-agenda-remap-ret t
  "When non-nil, bind RET in `org-agenda-mode' to source jump command."
  :type 'boolean
  :group 'org-bake)

(defvar org-bake--org-id-materialized-advice-enabled nil
  "Non-nil when org-bake advice for `org-id-find-id-file' is enabled.")

(defun org-bake--workspace-for-path (path)
  "Return first org-bake workspace containing PATH, or nil."
  (seq-find
   (lambda (workspace)
     (org-bake-project-contains-p workspace path))
   (org-bake-workspace-names)))

(defun org-bake--preferred-workspaces-for-id-resolution ()
  "Return workspace search order for materialized id resolution."
  (let* ((current-workspace
          (when buffer-file-name
            (org-bake--workspace-for-path buffer-file-name)))
         (workspaces (org-bake-workspace-names)))
    (if current-workspace
        (cons
         current-workspace
         (delq current-workspace (copy-sequence workspaces)))
      workspaces)))

(defun org-bake--materializer-version (name)
  "Return registered version string for materializer NAME, or nil."
  (condition-case nil
      (let* ((entry (cdr (org-bake-process-materializer-entry name)))
             (version (plist-get entry :version)))
        (when (stringp version)
          version))
    (error
     nil)))

(defun org-bake--ids-materializer-version ()
  "Return registered version string for ids materializer, or nil."
  (org-bake--materializer-version 'ids))

(defun org-bake--workspace-materialized-document-id-for-id
    (workspace id)
  "Return materialized document id for ID in WORKSPACE, or nil."
  (let ((version (org-bake--ids-materializer-version)))
    (when version
      (let* ((path
              (org-bake-project-materialization-path
               workspace "ids" version))
             (materialization
              (org-bake-store-read-materialization path))
             (data (org-bake-store--json-get materialization "data"))
             (document-id (org-bake-store--json-get data id)))
        (when (stringp document-id)
          document-id)))))

(defun org-bake--workspace-materialized-source-path-for-id
    (workspace id)
  "Return source Org file path for ID in WORKSPACE, or nil."
  (let ((document-id
         (org-bake--workspace-materialized-document-id-for-id
          workspace id)))
    (when document-id
      (let* ((document-path
              (expand-file-name
               (format "documents/%s.json" document-id)
               (org-bake-project-store-dir workspace)))
             (document (org-bake-store-read-json-file document-path))
             (source (org-bake-store--json-get document "source"))
             (path (org-bake-store--json-get source "path")))
        (when (stringp path)
          path)))))

(defun org-bake--materialized-source-path-for-id (id)
  "Return source Org file path for ID from org-bake materializations, or nil."
  (seq-some
   (lambda (workspace)
     (org-bake--workspace-materialized-source-path-for-id
      workspace id))
   (org-bake--preferred-workspaces-for-id-resolution)))

(defun org-bake--agenda-items-materializer-version ()
  "Return registered version string for agenda-items materializer, or nil."
  (org-bake--materializer-version 'agenda-items))

(defun org-bake--workspace-materialized-agenda-items (workspace)
  "Return agenda item rows for WORKSPACE from materialized data."
  (let ((version (org-bake--agenda-items-materializer-version)))
    (when version
      (let* ((path
              (org-bake-project-materialization-path
               workspace "agenda-items" version))
             (materialization
              (org-bake-store-read-materialization path))
             (data (org-bake-store--json-get materialization "data")))
        (cond
         ((vectorp data)
          (append data nil))
         ((listp data)
          data)
         (t
          nil))))))

(defun org-bake--agenda-item-tags-suffix (item)
  "Return Org headline tag suffix string for ITEM."
  (let ((tags (org-bake-store--json-get item "tags")))
    (when tags
      (let* ((tag-list
              (cond
               ((vectorp tags)
                (append tags nil))
               ((listp tags)
                tags)
               (t
                nil)))
             (clean-tags
              (delq
               nil
               (mapcar
                (lambda (tag)
                  (when (and (stringp tag) (> (length tag) 0))
                    tag))
                tag-list))))
        (when clean-tags
          (format " :%s:" (string-join clean-tags ":")))))))

(defun org-bake--agenda-item-priority-cookie (item)
  "Return Org priority cookie for ITEM, or empty string."
  (let ((priority (org-bake-store--json-get item "priority"))
        priority-char)
    (cond
     ((numberp priority)
      (setq priority-char (string priority)))
     ((and (stringp priority) (= (length priority) 1))
      (setq priority-char priority))
     ((and (stringp priority)
           (string-match-p "\\`[0-9]+\\'" priority))
      (setq priority-char (string (string-to-number priority)))))
    (if (and (stringp priority-char) (> (length priority-char) 0))
        (format " [#%s]" priority-char)
      "")))

(defun org-bake--agenda-item-heading-line (item)
  "Return generated Org headline line for ITEM."
  (let* ((todo (org-bake-store--json-get item "todo"))
         (title
          (or (org-bake-store--json-get item "title") "Untitled"))
         (priority (org-bake--agenda-item-priority-cookie item))
         (tags-suffix
          (or (org-bake--agenda-item-tags-suffix item) ""))
         (todo-prefix
          (if (and (stringp todo) (> (length todo) 0))
              (concat todo " ")
            ""))
         (clean-title
          (replace-regexp-in-string "[\n\r\t]+" " " title)))
    (format "* %s%s%s%s"
            todo-prefix
            priority
            clean-title
            tags-suffix)))

(defun org-bake--agenda-item-category (item)
  "Return agenda category string for ITEM source path."
  (let ((source-path (org-bake-store--json-get item "source_path")))
    (when (and (stringp source-path) (> (length source-path) 0))
      (file-name-base source-path))))

(defun org-bake-write-workspace-agenda-file
    (workspace &optional file-path)
  "Write generated agenda Org file for WORKSPACE and return its path.

FILE-PATH overrides the default workspace agenda view path."
  (interactive (list (org-bake-read-workspace)))
  (let* ((items
          (or (org-bake--workspace-materialized-agenda-items
               workspace)
              nil))
         (path
          (or file-path
              (expand-file-name
               org-bake-agenda-generated-file-name
               (org-bake-project-store-dir workspace)))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert "#+title: Org Bake Agenda View\n")
      (insert "\n")
      (insert
       (format "#+date: [%s]\n\n" (format-time-string "%F %a %R")))
      (dolist (item items)
        (let ((scheduled (org-bake-store--json-get item "scheduled"))
              (deadline (org-bake-store--json-get item "deadline"))
              (source-path
               (org-bake-store--json-get item "source_path"))
              (source-begin (org-bake-store--json-get item "begin"))
              (source-id (org-bake-store--json-get item "source_id"))
              (source-title (org-bake-store--json-get item "title"))
              (source-todo (org-bake-store--json-get item "todo"))
              (category (org-bake--agenda-item-category item)))
          (insert (org-bake--agenda-item-heading-line item) "\n")
          (when (and (stringp scheduled) (> (length scheduled) 0))
            (insert "SCHEDULED: " scheduled "\n"))
          (when (and (stringp deadline) (> (length deadline) 0))
            (insert "DEADLINE: " deadline "\n"))
          (insert ":PROPERTIES:\n")
          (when (and (stringp source-path) (> (length source-path) 0))
            (insert ":ORG_BAKE_SOURCE_PATH: " source-path "\n"))
          (when (numberp source-begin)
            (insert
             ":ORG_BAKE_SOURCE_BEGIN: "
             (number-to-string source-begin)
             "\n"))
          (when (and (stringp source-id) (> (length source-id) 0))
            (insert ":ORG_BAKE_SOURCE_ID: " source-id "\n"))
          (when (and (stringp source-title)
                     (> (length source-title) 0))
            (insert ":ORG_BAKE_SOURCE_TITLE: " source-title "\n"))
          (when (and (stringp source-todo) (> (length source-todo) 0))
            (insert ":ORG_BAKE_SOURCE_TODO: " source-todo "\n"))
          (when (and (stringp category) (> (length category) 0))
            (insert ":CATEGORY: " category "\n"))
          (insert ":END:\n\n"))))
    (when (called-interactively-p 'interactive)
      (message "org-bake wrote agenda view for %s: %s"
               workspace
               path))
    (let ((buffer (get-file-buffer path)))
      (when buffer
        (with-current-buffer buffer
          (revert-buffer t t t))))
    path))

(defun org-bake-use-workspace-agenda-file (workspace)
  "Generate and use WORKSPACE agenda view as sole entry in
variable `org-agenda-files'."
  (interactive (list (org-bake-read-workspace)))
  (let ((path (org-bake-write-workspace-agenda-file workspace)))
    (setq org-agenda-files (list path))
    (message
     "org-bake set org-agenda-files to generated agenda view: %s"
     path)
    path))

(defun org-bake-refresh-generated-agenda-files ()
  "Regenerate org-bake agenda files currently present in
variable `org-agenda-files'."
  (interactive)
  (let ((agenda-files
         (mapcar #'expand-file-name (org-agenda-files nil 'ifmode))))
    (dolist (workspace (org-bake-workspace-names))
      (let ((generated-path
             (expand-file-name
              org-bake-agenda-generated-file-name
              (org-bake-project-store-dir workspace))))
        (when (member (expand-file-name generated-path) agenda-files)
          (org-bake-write-workspace-agenda-file workspace))))))

(defun org-bake-agenda-redo ()
  "Regenerate org-bake agenda files then redo current Org agenda view."
  (interactive)
  (org-bake-refresh-generated-agenda-files)
  (org-agenda-redo))

(defun org-bake--workspace-materialization-idle-p (workspace)
  "Return non-nil when WORKSPACE has no queued or running materialize jobs."
  (not
   (seq-some
    (lambda (job)
      (and (eq (plist-get job :workspace) workspace)
           (eq (plist-get job :kind) 'materialize)
           (memq (plist-get job :status) '(queued running))))
    org-bake-process-queue)))

(defun org-bake--on-materializer-job-complete (job)
  "Handle completed materializer JOB by refreshing generated agenda view."
  (let ((workspace (plist-get job :workspace)))
    (when (symbolp workspace)
      (run-with-timer
       0 nil
       (lambda ()
         (when (org-bake--workspace-materialization-idle-p workspace)
           (ignore-errors
             (org-bake-write-workspace-agenda-file workspace))))))))

(defvar org-bake-process-materializer-job-complete-function
  #'org-bake--on-materializer-job-complete
  "Callback run when a materializer job completes.")

(defun org-bake--goto-source-heading-by-title
    (title &optional todo-keyword)
  "Jump to headline matching TITLE and optional TODO-KEYWORD in current buffer.

Return non-nil when a matching headline is found."
  (let (title-match-pos
        exact-match-pos)
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (let* ((components (org-heading-components))
             (todo-at-point (nth 2 components))
             (heading-title (nth 4 components))
             (point-at-heading (line-beginning-position)))
        (when (and (stringp heading-title)
                   (stringp title)
                   (string= heading-title title))
          (unless title-match-pos
            (setq title-match-pos point-at-heading))
          (when (and (stringp todo-keyword)
                     (stringp todo-at-point)
                     (string= todo-keyword todo-at-point))
            (setq exact-match-pos point-at-heading)
            (goto-char (point-max))))))
    (when (or exact-match-pos title-match-pos)
      (goto-char (or exact-match-pos title-match-pos))
      t)))


(defun org-bake-agenda-open-source ()
  "Jump from generated org-bake agenda entry to source heading."
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Not in org-agenda buffer"))
  (let ((marker
         (or (org-get-at-bol 'org-hd-marker)
             (org-get-at-bol 'org-marker))))
    (unless (markerp marker)
      (user-error "No agenda entry at point"))
    (let (source-path
          source-begin
          source-id
          source-title
          source-todo)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (setq source-path
                (org-entry-get nil "ORG_BAKE_SOURCE_PATH" t))
          (setq source-begin
                (org-entry-get nil "ORG_BAKE_SOURCE_BEGIN" t))
          (setq source-id (org-entry-get nil "ORG_BAKE_SOURCE_ID" t))
          (setq source-title
                (org-entry-get nil "ORG_BAKE_SOURCE_TITLE" t))
          (setq source-todo
                (org-entry-get nil "ORG_BAKE_SOURCE_TODO" t))))
      (cond
       ((and (stringp source-path)
             (> (length source-path) 0)
             (file-exists-p source-path))
        (find-file source-path)
        (cond
         ((and (stringp source-begin)
               (string-match-p "\\`[0-9]+\\'" source-begin))
          (goto-char
           (max 1 (min (point-max) (string-to-number source-begin)))))
         ((and (stringp source-id) (> (length source-id) 0))
          (require 'org-id)
          (org-id-goto source-id))
         ((and (stringp source-title) (> (length source-title) 0))
          (unless (org-bake--goto-source-heading-by-title source-title
                                                          source-todo)
            (goto-char (point-min)))))
        (org-fold-show-context 'org-link-search))
       ((and (stringp source-id) (> (length source-id) 0))
        (require 'org-id)
        (org-id-goto source-id))
       (t
        (user-error
         "No source metadata on generated agenda entry"))))))

(with-eval-after-load 'org-agenda
  (define-key
   org-agenda-mode-map (kbd "o") #'org-bake-agenda-open-source)
  (when org-bake-agenda-remap-ret
    (define-key
     org-agenda-mode-map (kbd "RET") #'org-bake-agenda-open-source))
  (define-key org-agenda-mode-map (kbd "g") #'org-bake-agenda-redo))

(defun org-bake--org-id-find-id-file-advice (orig-fn id)
  "Advice for `org-id-find-id-file' using ORIG-FN and materialized ID fallback."
  (or (funcall orig-fn id)
      (when org-bake-org-id-use-materialized-ids
        (org-bake--materialized-source-path-for-id id))))

(defun org-bake--org-id-find-advice (orig-fn id &optional markerp)
  "Advice for `org-id-find' using ORIG-FN, ID, and optional MARKERP.

Falls back to materialized ID lookup."
  (or (funcall orig-fn id markerp)
      (when org-bake-org-id-use-materialized-ids
        (let ((path (org-bake--materialized-source-path-for-id id)))
          (when (and (stringp path) (file-exists-p path))
            (if markerp
                (with-current-buffer (find-file-noselect path)
                  (copy-marker 1))
              (cons path 1)))))))

(defun org-bake-enable-org-id-materialized-resolution ()
  "Enable org-bake fallback for `org-id-find-id-file'."
  (interactive)
  (require 'org-id)
  (unless (advice-member-p
           #'org-bake--org-id-find-id-file-advice
           #'org-id-find-id-file)
    (advice-add
     #'org-id-find-id-file
     :around #'org-bake--org-id-find-id-file-advice))
  (unless (advice-member-p
           #'org-bake--org-id-find-advice #'org-id-find)
    (advice-add #'org-id-find :around #'org-bake--org-id-find-advice))
  (setq org-bake--org-id-materialized-advice-enabled t))

(defun org-bake-disable-org-id-materialized-resolution ()
  "Disable org-bake fallback for `org-id-find-id-file'."
  (interactive)
  (when (fboundp 'org-id-find-id-file)
    (when (advice-member-p
           #'org-bake--org-id-find-id-file-advice
           #'org-id-find-id-file)
      (advice-remove
       #'org-id-find-id-file #'org-bake--org-id-find-id-file-advice)))
  (when (fboundp 'org-id-find)
    (when (advice-member-p
           #'org-bake--org-id-find-advice #'org-id-find)
      (advice-remove #'org-id-find #'org-bake--org-id-find-advice)))
  (setq org-bake--org-id-materialized-advice-enabled nil))

(defvar org-bake--mode-after-init-hook-added nil
  "Non-nil when `org-bake-schedule-startup-indexer' is in `after-init-hook'.")

(defun org-bake--enable-runtime ()
  "Enable org-bake runtime side effects for `org-bake-mode'."
  (add-variable-watcher
   'org-bake-workspaces #'org-bake--workspaces-watcher)
  (unless org-bake--mode-after-init-hook-added
    (add-hook 'after-init-hook #'org-bake-schedule-startup-indexer)
    (setq org-bake--mode-after-init-hook-added t))
  (when (and org-bake-org-id-use-materialized-ids
             (not org-bake--org-id-materialized-advice-enabled))
    (org-bake-enable-org-id-materialized-resolution))
  (if (bound-and-true-p after-init-time)
      (org-bake-schedule-startup-indexer)))

(defun org-bake--disable-runtime ()
  "Disable org-bake runtime side effects for `org-bake-mode'."
  (when org-bake--mode-after-init-hook-added
    (remove-hook 'after-init-hook #'org-bake-schedule-startup-indexer)
    (setq org-bake--mode-after-init-hook-added nil))
  (remove-variable-watcher
   'org-bake-workspaces #'org-bake--workspaces-watcher)
  (org-bake-disable-org-id-materialized-resolution)
  (org-bake-stop-indexer))

(define-minor-mode org-bake-mode
  "Toggle org-bake background indexing runtime.

When enabled, org-bake installs runtime hooks/advice/watchers and can
schedule startup indexing for configured workspaces.  When disabled,
runtime side effects are removed and background workers stop."
  :global t
  :group
  'org-bake
  (if org-bake-mode
      (org-bake--enable-runtime)
    (org-bake--disable-runtime)))


(defun org-bake--workspaces-watcher
    (_symbol new-value operation _where)
  "Schedule startup indexing after workspace config change.

NEW-VALUE and OPERATION come from `add-variable-watcher'."
  (when (and (not (memq operation '(let unlet))) new-value)
    (org-bake-schedule-startup-indexer)))

(defun org-bake--workspace-directories (workspace)
  "Return all directories under WORKSPACE roots, recursively."
  (let (dirs)
    (dolist (root (org-bake-project-roots workspace))
      (when (file-directory-p root)
        (push root dirs)
        (dolist (path (directory-files-recursively root ".*" t nil))
          (when (file-directory-p path)
            (push path dirs)))))
    (delete-dups dirs)))

(defun org-bake--workspace-refresh-timer (workspace)
  "Return pending refresh timer for WORKSPACE, or nil."
  (alist-get workspace org-bake--workspace-refresh-timers))

(defun org-bake--clear-workspace-refresh-timer (workspace)
  "Clear pending refresh timer for WORKSPACE."
  (setq org-bake--workspace-refresh-timers
        (assq-delete-all
         workspace org-bake--workspace-refresh-timers)))

(defun org-bake--clear-workspace-watchers (workspace)
  "Remove all file notification watches for WORKSPACE."
  (dolist (descriptor
           (alist-get
            workspace org-bake--workspace-watch-descriptors))
    (ignore-errors
      (when (file-notify-valid-p descriptor)
        (file-notify-rm-watch descriptor))))
  (setq org-bake--workspace-watch-descriptors
        (assq-delete-all
         workspace org-bake--workspace-watch-descriptors)))

(defun org-bake--clear-all-watchers ()
  "Remove all org-bake file watches and pending refresh timers."
  (dolist (entry org-bake--workspace-watch-descriptors)
    (org-bake--clear-workspace-watchers (car entry)))
  (dolist (entry org-bake--workspace-refresh-timers)
    (when (timerp (cdr entry))
      (cancel-timer (cdr entry))))
  (setq
   org-bake--workspace-watch-descriptors nil
   org-bake--workspace-refresh-timers nil)
  (org-bake-process-clear-materialize-timers))

(defun org-bake--refresh-workspace-watchers (workspace)
  "Rebuild recursive file notification watches for WORKSPACE."
  (org-bake--clear-workspace-watchers workspace)
  (let ((dirs (org-bake--workspace-directories workspace))
        descriptors)
    (dolist (dir dirs)
      (push (file-notify-add-watch
             dir '(change attribute-change)
             (lambda (event)
               (org-bake--handle-watch-event workspace event)))
            descriptors))
    (push (cons workspace descriptors)
          org-bake--workspace-watch-descriptors)))

(defun org-bake--schedule-workspace-refresh (workspace)
  "Debounce refresh for WORKSPACE."
  (unless (timerp (org-bake--workspace-refresh-timer workspace))
    (let ((timer
           (run-with-timer
            0.2 nil
            (lambda ()
              (org-bake--clear-workspace-refresh-timer workspace)
              (org-bake--refresh-workspace-watchers workspace)
              (org-bake-refresh-workspace workspace)))))
      (push
       (cons workspace timer) org-bake--workspace-refresh-timers))))


(defun org-bake--handle-watch-event (workspace _event)
  "Handle file notification event for WORKSPACE."
  (org-bake--schedule-workspace-refresh workspace))

(defun org-bake-workspace-names ()
  "Return configured org-bake workspace names."
  (mapcar #'car org-bake-workspaces))

(defun org-bake-read-workspace ()
  "Prompt for an org-bake workspace name."
  (intern
   (completing-read "Org-bake workspace: "
                    (mapcar #'symbol-name (org-bake-workspace-names))
                    nil t)))

(defun org-bake-scan-workspace (workspace)
  "Scan WORKSPACE and enqueue stale export jobs."
  (interactive (list (org-bake-read-workspace)))
  (let* ((result (org-bake-process-scan-workspace workspace))
         (queued-jobs (plist-get result :queued))
         (deleted-documents (plist-get result :deleted))
         (missing-materializers
          (org-bake-process-missing-materializers workspace)))
    (cond
     (deleted-documents
      (org-bake-process-schedule-workspace-materialization
       workspace t))
     ((and (null queued-jobs) missing-materializers)
      (org-bake-process-schedule-workspace-materialization
       workspace nil)))
    (when (called-interactively-p 'interactive)
      (message
       "org-bake queued %d export job(s), deleted %d stale document(s) for %s"
       (length queued-jobs) (length deleted-documents) workspace))
    result))

(defun org-bake-export-workspace (workspace)
  "Start queued export jobs for WORKSPACE asynchronously."
  (interactive (list (org-bake-read-workspace)))
  (let ((jobs (org-bake-export-queued-jobs workspace)))
    (when (called-interactively-p 'interactive)
      (message "org-bake started %d export job(s) for %s"
               (length jobs)
               workspace))
    jobs))

(defun org-bake-materialize-workspace (workspace)
  "Enqueue and run registered materializers for WORKSPACE asynchronously."
  (interactive (list (org-bake-read-workspace)))
  (let ((jobs (org-bake-process-enqueue-materializers workspace t)))
    (org-bake-process-run-materializers workspace)
    (when (called-interactively-p 'interactive)
      (message "org-bake queued %d materializer job(s) for %s"
               (length jobs)
               workspace))
    jobs))

(defun org-bake-rematerialize-workspace (workspace)
  "Force rerun all registered materializers for WORKSPACE asynchronously."
  (interactive (list (org-bake-read-workspace)))
  (let ((jobs (org-bake-process-enqueue-materializers workspace t)))
    (org-bake-process-run-materializers workspace)
    (when (called-interactively-p 'interactive)
      (message "org-bake force-queued %d materializer job(s) for %s"
               (length jobs)
               workspace))
    jobs))

(defun org-bake-refresh-workspace (workspace)
  "Scan WORKSPACE, enqueue stale jobs, and start async exports."
  (interactive (list (org-bake-read-workspace)))
  (org-bake-scan-workspace workspace)
  (org-bake-export-workspace workspace))

(defun org-bake-rebake-workspace (workspace)
  "Force re-export all documents in WORKSPACE asynchronously."
  (interactive (list (org-bake-read-workspace)))
  (let* ((result (org-bake-process-rebake-workspace workspace))
         (queued-jobs (plist-get result :queued))
         (deleted-documents (plist-get result :deleted)))
    (org-bake-export-workspace workspace)
    (when (called-interactively-p 'interactive)
      (message
       "org-bake rebake queued %d export job(s), deleted %d stale document(s) for %s"
       (length queued-jobs) (length deleted-documents) workspace))
    result))

(defun org-bake-startup-indexer ()
  "Scan and export all configured workspaces."
  (interactive)
  (mapc #'org-bake-refresh-workspace (org-bake-workspace-names)))

(defun org-bake-stop-indexer ()
  "Stop background org-bake indexer."
  (interactive)
  (when (timerp org-bake--index-timer)
    (cancel-timer org-bake--index-timer))
  (setq
   org-bake--index-timer nil
   org-bake--startup-indexer-scheduled nil)
  (org-bake--clear-all-watchers))
(org-bake-export-stop-workers)

(defun org-bake-start-indexer ()
  "Start background org-bake indexer."
  (interactive)
  (org-bake-stop-indexer)
  (setq org-bake--startup-indexer-scheduled nil)
  (dolist (workspace (org-bake-workspace-names))
    (org-bake--refresh-workspace-watchers workspace)
    (org-bake-process-schedule-workspace-materialization
     workspace nil))
  (org-bake-startup-indexer)
  (when org-bake-index-interval
    (setq org-bake--index-timer
          (run-at-time
           org-bake-index-interval
           org-bake-index-interval
           #'org-bake-startup-indexer))))

(defun org-bake-schedule-startup-indexer ()
  "Schedule org-bake startup indexer to run in background."
  (when (and org-bake-auto-index-on-startup
             org-bake-workspaces
             (not org-bake--startup-indexer-scheduled))
    (setq org-bake--startup-indexer-scheduled t)
    (run-with-timer 0 nil (lambda () (org-bake-start-indexer)))))


(provide 'org-bake)
;;; org-bake.el ends here
