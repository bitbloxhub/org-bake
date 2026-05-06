;;; org-bake-project.el --- Project discovery and configuration for org-bake  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (async "1.9.9") (ox-json "1"))
;; URL: https://github.com/bitbloxhub/org-bake
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Project-level logic for org-bake.
;;
;; This includes project root discovery, reading project configuration,
;; path layout decisions, and document enumeration.

;;; Code:

(require 'org)

(require 'subr-x)
(require 'seq)

(defvar org-bake-store-root)
(defvar org-bake-workspaces)

(defun org-bake-project--workspace-entry (name)
  "Return the raw workspace entry for NAME.
Signal an error if NAME is unknown."
  (or (assq name org-bake-workspaces)
      (error "Unknown org-bake workspace: %S" name)))

(defun org-bake-project-get (name)
  "Return the plist for workspace NAME."
  (cdr (org-bake-project--workspace-entry name)))

(defun org-bake-project-roots (name)
  "Return normalized root directories for workspace NAME."
  (let* ((workspace (org-bake-project-get name))
         (roots (plist-get workspace :roots)))
    (unless roots
      (error "Workspace %S has no :roots" name))
    (mapcar #'org-bake-project--normalize-dir roots)))

(defun org-bake-project--xdg-data-home ()
  "Return XDG data home for org-bake."
  (expand-file-name (or (getenv "XDG_DATA_HOME") "~/.local/share/")))

(defun org-bake-project--default-store-dir (name)
  "Return default store directory for workspace NAME."
  (let ((base-dir
         (if org-bake-store-root
             (org-bake-project--normalize-dir org-bake-store-root)
           (expand-file-name "org-bake/"
                             (org-bake-project--xdg-data-home)))))
    (expand-file-name (format "%s/" (symbol-name name)) base-dir)))

(defun org-bake-project-store-dir (name)
  "Return normalized store directory for workspace NAME."
  (let* ((workspace (org-bake-project-get name))
         (dir
          (or (plist-get workspace :store-dir)
              (org-bake-project--default-store-dir name))))
    (org-bake-project--normalize-dir dir)))

(defun org-bake-project--normalize-dir (path)
  "Expand PATH to an absolute directory name with trailing slash."
  (file-name-as-directory (expand-file-name path)))

(defun org-bake-project--org-file-p (path)
  "Return non-nil if PATH is an indexable Org file."
  (when (stringp path)
    (let ((name (file-name-nondirectory path)))
      (and (string-match-p "\\.org\\'" path)
           (not (string-prefix-p ".#" name))
           (not
            (and (string-prefix-p "#" name)
                 (string-suffix-p "#" name)))
           (not (string-suffix-p "~" name))))))

(defun org-bake-project-files (name)
  "Return all indexable Org files for workspace NAME."
  (seq-filter
   #'org-bake-project--org-file-p
   (seq-mapcat
    (lambda (root)
      (directory-files-recursively root "\\.org\\'"))
    (org-bake-project-roots name))))

(defun org-bake-project-contains-p (name path)
  "Return non-nil if PATH is inside any root of workspace NAME."
  (let ((abs-path (expand-file-name path)))
    (seq-some
     (lambda (root) (file-in-directory-p abs-path root))
     (org-bake-project-roots name))))

(defun org-bake-project-root-for-path (name path)
  "Return workspace root in NAME that can contain PATH, or nil."
  (let ((abs-path (expand-file-name path)))
    (seq-find
     (lambda (root) (file-in-directory-p abs-path root))
     (org-bake-project-roots name))))

(defun org-bake-project-relative-path (name path)
  "Return PATH relative to its containing root in workspace NAME.
Signal an error if PATH is not inside any root."
  (let* ((abs-path (expand-file-name path))
         (root (org-bake-project-root-for-path name abs-path)))
    (unless root
      (error "Path %S is not in workspace %S" path name))
    (file-relative-name abs-path root)))

(defun org-bake-project-document-id (workspace path)
  "Return a stable document id for PATH in WORKSPACE."
  (let ((relative-path
         (org-bake-project-relative-path workspace path)))
    (substring (secure-hash
                'sha256
                (encode-coding-string relative-path 'utf-8))
               0 20)))

(defun org-bake-project-document-path (name path)
  "Return document JSON path for PATH in workspace NAME."
  (expand-file-name (format "documents/%s.json"
                            (org-bake-project-document-id name path))
                    (org-bake-project-store-dir name)))

(defun org-bake-project-meta-path (name)
  "Return metadata file path for workspace NAME."
  (expand-file-name "meta.json" (org-bake-project-store-dir name)))

(defun org-bake-project-documents-dir (name)
  "Return documents directory for workspace NAME."
  (expand-file-name "documents/" (org-bake-project-store-dir name)))

(defun org-bake-project-materializations-dir (name)
  "Return materializations directory for workspace NAME."
  (expand-file-name "materializations/"
                    (org-bake-project-store-dir name)))

(defun org-bake-project-materialization-path
    (name materialization version)
  "Return materialization path for NAME, MATERIALIZATION, and VERSION."
  (expand-file-name (format "materializations/%s-%s.json"
                            materialization
                            version)
                    (org-bake-project-store-dir name)))

(defun org-bake-project-describe (name)
  "Return a plist describing workspace NAME."
  (list
   :name name
   :roots (org-bake-project-roots name)
   :store-dir (org-bake-project-store-dir name)
   :files (org-bake-project-files name)))

(defun org-bake-project-jobs (name)
  "Return export job plists for all indexable Org files in workspace NAME."
  (mapcar
   (lambda (path)
     (list
      :workspace name
      :source-path path
      :document-id (org-bake-project-document-id name path)
      :relative-path (org-bake-project-relative-path name path)
      :document-path (org-bake-project-document-path name path)))
   (org-bake-project-files name)))

(provide 'org-bake-project)
;;; org-bake-project.el ends here
