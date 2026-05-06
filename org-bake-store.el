;;; org-bake-store.el --- Derived document store for org-bake  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (async "1.9.9") (ox-json "1"))
;; URL: https://github.com/bitbloxhub/org-bake
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Store logic for org-bake.
;;
;; This file manages the persisted derived view of Org sources used by
;; org-bake.  The store records source-state snapshots, output metadata,
;; and schema/version fingerprints so org-bake can determine which jobs
;; are out of date and need to be rebuilt.
;;
;; The store is treated as a durable derived data source, not merely as
;; an implementation cache.

;;; Code:

(require 'json)
(require 'org-bake-project)
(require 'subr-x)

(defvar org-bake-store-schema-version 1
  "Current on-disk schema version for org-bake.")

(defvar org-bake-export-format-version)

(defun org-bake-store--json-get (object key)
  "Return KEY from parsed JSON OBJECT.

KEY may be a string or symbol.  OBJECT should be an alist parsed from JSON."
  (when (listp object)
    (let ((string-key
           (if (stringp key)
               key
             (symbol-name key)))
          (symbol-key
           (if (symbolp key)
               key
             (intern key))))
      (or (alist-get key object nil nil #'equal)
          (alist-get string-key object nil nil #'equal)
          (alist-get symbol-key object nil nil #'equal)))))

(defun org-bake-store--mtime-seconds (mtime)
  "Normalize MTIME into integer seconds, or nil."
  (when mtime
    (floor (float-time mtime))))

(defun org-bake-store-source-state (path)
  "Return current source-state plist for PATH."
  (let* ((attrs (file-attributes path))
         (mtime (file-attribute-modification-time attrs))
         (size (file-attribute-size attrs)))
    (list
     :path (expand-file-name path)
     :mtime (org-bake-store--mtime-seconds mtime)
     :size size)))

(defun org-bake-store--source-state (path)
  "Return current source-state plist for PATH."
  (org-bake-store-source-state path))

(defun org-bake-store-read-json-file (path)
  "Return parsed JSON object from PATH, or nil when PATH does not exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (json-parse-buffer
       :object-type 'alist
       :array-type 'list
       :null-object nil
       :false-object
       :false))))

(defun org-bake-store-write-json-file (path object)
  "Write OBJECT as JSON to PATH.

Parent directories are created automatically."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert
     (json-serialize object :null-object nil :false-object :false))
    (insert "\n")))

(defun org-bake-store-ensure-workspace (name)
  "Ensure store layout exists for workspace NAME."
  (dolist (dir
           (list
            (org-bake-project-store-dir name)
            (org-bake-project-documents-dir name)
            (org-bake-project-materializations-dir name)))
    (make-directory dir t))
  (org-bake-project-store-dir name))

(defun org-bake-store-read-meta (name)
  "Return parsed `meta.json' for workspace NAME, or nil."
  (org-bake-store-read-json-file (org-bake-project-meta-path name)))

(defun org-bake-store-write-meta (name &rest properties)
  "Write `meta.json' for workspace NAME using PROPERTIES.

Supported PROPERTIES keys are `:created-at', `:last-scan-at', and
`document-count'."
  (org-bake-store-ensure-workspace name)
  (let* ((now (floor (float-time)))
         (existing (or (org-bake-store-read-meta name) '()))
         (created-at
          (or (plist-get properties :created-at)
              (org-bake-store--json-get existing "created_at")
              now))
         (last-scan-at
          (or (plist-get properties :last-scan-at)
              (org-bake-store--json-get existing "last_scan_at")))
         (document-count
          (or (plist-get properties :document-count)
              (org-bake-store--json-get existing "document_count")
              0))
         (meta
          `((schema_version . ,org-bake-store-schema-version)
            (workspace . ,(symbol-name name))
            (roots . ,(vconcat (org-bake-project-roots name)))
            (store_root . ,(org-bake-project-store-dir name))
            (created_at . ,created-at)
            (last_scan_at . ,last-scan-at)
            (document_count . ,document-count))))
    (org-bake-store-write-json-file
     (org-bake-project-meta-path name) meta)
    meta))

(defun org-bake-store-source-stale-p (stored-source path)
  "Return non-nil when STORED-SOURCE metadata is stale for PATH."
  (let* ((current-state (org-bake-store-source-state path))
         (stored-mtime
          (org-bake-store--mtime-seconds
           (org-bake-store--json-get stored-source "mtime")))
         (stored-size
          (org-bake-store--json-get stored-source "size")))
    (or (null stored-source)
        (not (equal stored-mtime (plist-get current-state :mtime)))
        (not (equal stored-size (plist-get current-state :size))))))

(defun org-bake-store-job-outdated-p (job)
  "Return non-nil when JOB should be re-exported."
  (let* ((document
          (org-bake-store-read-json-file
           (plist-get job :document-path)))
         (stored-source (org-bake-store--json-get document "source"))
         (stored-export (org-bake-store--json-get document "export"))
         (stored-schema-version
          (org-bake-store--json-get document "schema_version"))
         (stored-format-version
          (org-bake-store--json-get stored-export "format_version")))
    (or (null document)
        (org-bake-store-source-stale-p
         stored-source (plist-get job :source-path))
        (not
         (equal stored-schema-version org-bake-store-schema-version))
        (not
         (equal
          stored-format-version org-bake-export-format-version)))))

(defun org-bake-store--entry-outdated-p
    (entry current-state fingerprint)
  "Return non-nil when store ENTRY does not match CURRENT-STATE.

FINGERPRINT should capture schema/config/exporter inputs relevant to the job."
  (or (null entry)
      (not
       (equal
        (plist-get entry :source-mtime)
        (plist-get current-state :mtime)))
      (not
       (= (plist-get entry :source-size)
          (plist-get current-state :size)))
      (not (equal (plist-get entry :fingerprint) fingerprint))
      (not (file-exists-p (plist-get entry :output-path)))))

(defun org-bake-store--entry-for-job (store job)
  "Return stored entry for JOB from STORE."
  (alist-get (plist-get job :document-id) store nil nil #'equal))

(defun org-bake-store-filter-jobs (store jobs fingerprint-fn)
  "Partition JOBS into outdated and current jobs using STORE.

FINGERPRINT-FN is called with a job plist and must return a string or plist
that represents job's derivation fingerprint.

Return plist of form:
  (:outdated (JOB...)
   :current  (JOB...))."
  (let (outdated
        current)
    (dolist (job jobs)
      (let* ((entry (org-bake-store--entry-for-job store job))
             (state
              (org-bake-store--source-state
               (plist-get job :source-path)))
             (fingerprint (funcall fingerprint-fn job)))
        (if (org-bake-store--entry-outdated-p entry state fingerprint)
            (push job outdated)
          (push job current))))
    (list :outdated (nreverse outdated) :current (nreverse current))))

(defun org-bake-store-make-entry (job fingerprint)
  "Return fresh store entry plist for JOB and FINGERPRINT."
  (let* ((source-path (plist-get job :source-path))
         (state (org-bake-store--source-state source-path)))
    (list
     :document-id (plist-get job :document-id)
     :source-path (plist-get state :path)
     :source-mtime (plist-get state :mtime)
     :source-size (plist-get state :size)
     :fingerprint fingerprint
     :output-path (plist-get job :output-path))))

(defun org-bake-store-document-paths (name)
  "Return stored document JSON paths for workspace NAME."
  (if (file-directory-p (org-bake-project-documents-dir name))
      (directory-files (org-bake-project-documents-dir name)
                       t
                       "\\.json\\'")
    nil))

(defun org-bake-store-read-documents (name)
  "Return parsed stored documents for workspace NAME."
  (delq
   nil
   (mapcar
    #'org-bake-store-read-json-file
    (org-bake-store-document-paths name))))

(defun org-bake-store-prune-missing-documents (name jobs)
  "Delete stored documents for workspace NAME missing from JOBS.

JOBS should be current project jobs for workspace NAME.  Return deleted
document paths."
  (let* ((live-document-ids
          (mapcar (lambda (job) (plist-get job :document-id)) jobs))
         deleted-paths)
    (dolist (path (org-bake-store-document-paths name))
      (let* ((document (org-bake-store-read-json-file path))
             (document-id (org-bake-store--json-get document "id")))
        (unless (member document-id live-document-ids)
          (delete-file path)
          (push path deleted-paths))))
    (nreverse deleted-paths)))

(defun org-bake-store-read-documents-from-dir (documents-dir)
  "Return parsed stored documents from DOCUMENTS-DIR."
  (if (file-directory-p documents-dir)
      (delq
       nil
       (mapcar
        #'org-bake-store-read-json-file
        (directory-files documents-dir t "\\.json\\'")))
    nil))

(defun org-bake-store-read-materialization (path)
  "Return parsed materialization JSON from PATH, or nil."
  (org-bake-store-read-json-file path))

(defun org-bake-store-write-materialization (path object)
  "Write materialization OBJECT to PATH as JSON."
  (org-bake-store-write-json-file path object))


(provide 'org-bake-store)
;;; org-bake-store.el ends here
