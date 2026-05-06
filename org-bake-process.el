;;; org-bake-process.el --- Async process support for org-bake  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (async "1.9.9") (ox-json "1"))
;; URL: https://github.com/bitbloxhub/org-bake
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Async and worker-process support for org-bake.
;;
;; This file isolates async execution, worker bootstrap, and communication
;; details so the rest of the package can stay mostly synchronous.

;;; Code:


(require 'async)
(require 'cl-lib)
(require 'org-bake-project)
(require 'org-bake-store)
(require 'seq)

(defvar org-bake-process-queue nil
  "Queued org-bake jobs.")

(defvar org-bake-process--job-counter 0
  "Monotonic job counter for lightweight in-memory job ids.")

(defvar org-bake-process-materializers nil
  "Registered org-bake materializers.

Each entry form:
  (NAME . PROPERTIES).")

(defvar org-bake-process--workspace-materialize-timers nil)

(defvar org-bake-store-schema-version)

(defun org-bake-process--next-job-id ()
  "Return fresh in-memory job id."
  (format "job-%d" (cl-incf org-bake-process--job-counter)))

(defun org-bake-process-enqueue (job)
  "Append JOB to in-memory org-bake queue and return JOB."
  (setq org-bake-process-queue
        (append org-bake-process-queue (list job)))
  job)

(defun org-bake-process-enqueue-many (jobs)
  "Append JOBS to in-memory org-bake queue and return JOBS."
  (setq org-bake-process-queue (append org-bake-process-queue jobs))
  jobs)

(defun org-bake-process-clear-queue ()
  "Clear in-memory org-bake queue."
  (setq org-bake-process-queue nil))

(defun org-bake-process-delete-job (job-id)
  "Delete queued job with JOB-ID from in-memory queue."
  (setq org-bake-process-queue
        (seq-remove
         (lambda (job) (equal job-id (plist-get job :id)))
         org-bake-process-queue)))

(defun org-bake-process-update-job-status (job-id status)
  "Set queued job with JOB-ID to STATUS and return updated job."
  (let (updated-job)
    (setq org-bake-process-queue
          (mapcar
           (lambda (job)
             (if (equal job-id (plist-get job :id))
                 (setq updated-job
                       (plist-put (copy-sequence job) :status status))
               job))
           org-bake-process-queue))
    updated-job))

(defun org-bake-process-queued-jobs (&optional workspace kind)
  "Return queued jobs filtered by WORKSPACE and KIND.

When WORKSPACE or KIND is nil, that filter is skipped."
  (seq-filter
   (lambda (job)
     (and (or (null workspace)
              (eq workspace (plist-get job :workspace)))
          (or (null kind) (eq kind (plist-get job :kind)))))
   org-bake-process-queue))

(defun org-bake-process-job-active-p (workspace kind document-id)
  "Return non-nil when matching WORKSPACE KIND DOCUMENT-ID job exists.

Both queued and running jobs count as existing."
  (seq-some
   (lambda (job)
     (and (eq workspace (plist-get job :workspace))
          (eq kind (plist-get job :kind))
          (equal document-id (plist-get job :document-id))
          (memq (plist-get job :status) '(queued running))))
   org-bake-process-queue))

(defun org-bake-process--normalize-materializer-name (name)
  "Return canonical symbol form for materializer NAME."
  (cond
   ((symbolp name)
    name)
   ((stringp name)
    (intern name))
   (t
    (error "Invalid materializer name: %S" name))))


(defun org-bake-process-register-materializer (name &rest properties)
  "Register materializer NAME with PROPERTIES.

Required PROPERTIES keys are `:version' and `:builder'.  Optional keys are
`:feature' and `:description'.  Builder should be a function symbol that
accepts WORKSPACE and DOCUMENTS, returning JSON-serializable data."
  (setq name (org-bake-process--normalize-materializer-name name))
  (unless (plist-get properties :version)
    (error "Materializer %S missing :version" name))
  (unless (plist-get properties :builder)
    (error "Materializer %S missing :builder" name))
  (setq org-bake-process-materializers
        (assq-delete-all name org-bake-process-materializers))
  (push (cons name properties) org-bake-process-materializers)
  name)

(defun org-bake-process-materializer-entry (name)
  "Return registered materializer entry for NAME."
  (setq name (org-bake-process--normalize-materializer-name name))
  (or (assq name org-bake-process-materializers)
      (error "Unknown org-bake materializer: %S" name)))

(defun org-bake-process-materializer-names ()
  "Return registered materializer names."
  (mapcar #'car org-bake-process-materializers))

(defun org-bake-process-materializer-active-p
    (workspace materializer version)
  "Return non-nil when WORKSPACE MATERIALIZER VERSION job is active."
  (seq-some
   (lambda (job)
     (and (eq workspace (plist-get job :workspace))
          (eq 'materialize (plist-get job :kind))
          (eq materializer (plist-get job :materializer))
          (equal version (plist-get job :version))
          (memq (plist-get job :status) '(queued running))))
   org-bake-process-queue))

(defun org-bake-process--make-export-job (workspace job)
  "Return queued export job plist for WORKSPACE from JOB."
  (list
   :id (org-bake-process--next-job-id)
   :kind 'export-document
   :status 'queued
   :workspace workspace
   :document-id (plist-get job :document-id)
   :source-path (plist-get job :source-path)
   :relative-path (plist-get job :relative-path)
   :document-path (plist-get job :document-path)))

(defun org-bake-process--make-materialize-job (workspace materializer)
  "Return queued materialize job plist for WORKSPACE and MATERIALIZER."
  (setq materializer
        (org-bake-process--normalize-materializer-name materializer))
  (let* ((entry
          (cdr (org-bake-process-materializer-entry materializer)))
         (version (plist-get entry :version)))
    (list
     :id (org-bake-process--next-job-id)
     :kind 'materialize
     :status 'queued
     :workspace workspace
     :materializer materializer
     :version version
     :builder (plist-get entry :builder)
     :feature (plist-get entry :feature)
     :documents-dir (org-bake-project-documents-dir workspace)
     :materialization-path
     (org-bake-project-materialization-path
      workspace (symbol-name materializer) version))))

(defun org-bake-process-scan-workspace (name)
  "Scan workspace NAME for stale documents and enqueue export jobs."
  (let* ((jobs (org-bake-project-jobs name))
         (deleted-documents
          (org-bake-store-prune-missing-documents name jobs))
         (queued-jobs
          (seq-mapcat
           (lambda (job)
             (when (and (org-bake-store-job-outdated-p job)
                        (not
                         (org-bake-process-job-active-p
                          name
                          'export-document
                          (plist-get job :document-id))))
               (list (org-bake-process--make-export-job name job))))
           jobs))
         (scan-at (floor (float-time))))
    (org-bake-store-write-meta
     name
     :last-scan-at scan-at
     :document-count (length jobs))
    (org-bake-process-enqueue-many queued-jobs)
    (list :queued queued-jobs :deleted deleted-documents)))

(defun org-bake-process-rebake-workspace (name)
  "Enqueue export jobs for all documents in workspace NAME.

Unlike `org-bake-process-scan-workspace', this ignores staleness checks and
queues every current document not already queued or running."
  (let* ((jobs (org-bake-project-jobs name))
         (deleted-documents
          (org-bake-store-prune-missing-documents name jobs))
         (queued-jobs
          (seq-mapcat
           (lambda (job)
             (unless (org-bake-process-job-active-p
                      name 'export-document
                      (plist-get job :document-id))
               (list (org-bake-process--make-export-job name job))))
           jobs))
         (scan-at (floor (float-time))))
    (org-bake-store-write-meta
     name
     :last-scan-at scan-at
     :document-count (length jobs))
    (org-bake-process-enqueue-many queued-jobs)
    (list :queued queued-jobs :deleted deleted-documents)))

(defun org-bake-process-materialization-path
    (workspace materializer version)
  "Return materialization output path for WORKSPACE, MATERIALIZER, and VERSION."
  (org-bake-project-materialization-path
   workspace (symbol-name materializer) version))

(defun org-bake-process-materialization-exists-p
    (workspace materializer version)
  "Return non-nil when WORKSPACE MATERIALIZER VERSION output exists."
  (file-exists-p
   (org-bake-process-materialization-path
    workspace materializer version)))

(defun org-bake-process-missing-materializers (workspace)
  "Return registered materializer names missing output for WORKSPACE."
  (seq-filter
   (lambda (materializer)
     (let* ((entry
             (cdr (org-bake-process-materializer-entry materializer)))
            (version (plist-get entry :version)))
       (not
        (org-bake-process-materialization-exists-p
         workspace materializer version))))
   (org-bake-process-materializer-names)))

(defun org-bake-process-enqueue-materializers
    (workspace &optional force)
  "Enqueue materializer jobs for WORKSPACE.

When FORCE is non-nil, enqueue every registered materializer not already
running.  Otherwise enqueue only materializers whose output file is missing."
  (let ((jobs
         (seq-mapcat
          (lambda (materializer)
            (let* ((entry
                    (cdr
                     (org-bake-process-materializer-entry
                      materializer)))
                   (version (plist-get entry :version)))
              (when (and (not
                          (org-bake-process-materializer-active-p
                           workspace materializer version))
                         (or
                          force
                          (not
                           (org-bake-process-materialization-exists-p
                            workspace materializer version))))
                (list
                 (org-bake-process--make-materialize-job
                  workspace materializer)))))
          (org-bake-process-materializer-names))))
    (org-bake-process-enqueue-many jobs)
    jobs))

(defun org-bake-process-materialize-batch-async-backend
    (workspace-name job-specs documents-dir)
  "Worker-side backend for async batch materialization.

WORKSPACE-NAME names workspace.  JOB-SPECS is list of plists with keys
`:materializer', `:version', `:builder', and `:feature'.
DOCUMENTS-DIR points to exported documents."
  (require 'org-bake-store)
  (dolist (job-spec job-specs)
    (let ((feature (plist-get job-spec :feature)))
      (when feature
        (require feature))))
  (let* ((workspace (intern workspace-name))
         (documents
          (org-bake-store-read-documents-from-dir documents-dir)))
    (mapcar
     (lambda (job-spec)
       (let* ((materializer
               (org-bake-process--normalize-materializer-name
                (plist-get job-spec :materializer)))
              (version (plist-get job-spec :version))
              (builder (plist-get job-spec :builder))
              (data (funcall builder workspace documents)))
         `((schema_version . ,org-bake-store-schema-version)
           (type . "materialization")
           (name . ,(symbol-name materializer))
           (version . ,version)
           (workspace . ,workspace-name)
           (generated_at . ,(floor (float-time)))
           (data . ,data))))
     job-specs)))

(defun org-bake-process-materialize-jobs-async
    (jobs &optional callback)
  "Run materialize JOBS in one child Emacs process.

CALLBACK is called as (CALLBACK JOB MATERIALIZATION) for each completed
materialization in JOBS order."
  (let* ((workspace-name
          (symbol-name (plist-get (car jobs) :workspace)))
         (documents-dir (plist-get (car jobs) :documents-dir))
         (job-specs
          (mapcar
           (lambda (job)
             (list
              :materializer (plist-get job :materializer)
              :version (plist-get job :version)
              :builder (plist-get job :builder)
              :feature (plist-get job :feature)))
           jobs))
         (parent-load-path load-path))
    (async-start
     `(lambda ()
        (setq load-path ',parent-load-path)
        (setq load-prefer-newer t)
        (require 'org-bake-process)
        (org-bake-process-materialize-batch-async-backend
         ,workspace-name ',job-specs ,documents-dir))
     (lambda (materializations)
       (cl-mapc
        (lambda (job materialization)
          (org-bake-store-write-materialization
           (plist-get job :materialization-path) materialization)
          (when callback
            (funcall callback job materialization)))
        jobs materializations)))))

(defvar org-bake-max-materialize-jobs)
(defvar org-bake-materialize-debounce-seconds)
(defvar org-bake-process-materializer-job-complete-function nil
  "Optional function called with one completed materializer JOB.")

(defun org-bake-process--running-materializer-count
    (&optional workspace)
  "Return count of running materializer jobs for WORKSPACE."
  (seq-count
   (lambda (job) (eq (plist-get job :status) 'running))
   (org-bake-process-queued-jobs workspace 'materialize)))

(defun org-bake-process--dispatch-materializer-jobs
    (&optional workspace callback)
  "Start queued materializer jobs for WORKSPACE in one child process.

When CALLBACK is non-nil, call it for each completed job.
When a materialize job is already running for WORKSPACE, this is a no-op.
Return jobs started by this dispatch call."
  (let* ((running
          (org-bake-process--running-materializer-count workspace))
         (limit (max 1 (or org-bake-max-materialize-jobs 1)))
         (queued
          (seq-filter
           (lambda (job) (eq (plist-get job :status) 'queued))
           (org-bake-process-queued-jobs workspace 'materialize)))
         (to-start (seq-take queued limit))
         started-jobs)
    (when (and (= running 0) to-start)
      (setq started-jobs to-start)
      (dolist (job to-start)
        (org-bake-process-update-job-status
         (plist-get job :id) 'running))
      (org-bake-process-materialize-jobs-async
       to-start
       (lambda (job _materialization)
         (unwind-protect
             (when callback
               (funcall callback job))
           (org-bake-process-delete-job (plist-get job :id))
           (when org-bake-process-materializer-job-complete-function
             (funcall
              org-bake-process-materializer-job-complete-function
              job))
           (org-bake-process--dispatch-materializer-jobs
            workspace callback)))))
    started-jobs))

(defun org-bake-process-run-materializers
    (&optional workspace callback)
  "Start queued materializer jobs for WORKSPACE and return newly started jobs.

Optional CALLBACK runs for each completed job."
  (org-bake-process--dispatch-materializer-jobs workspace callback))

(defun org-bake-process--workspace-materialize-timer (workspace)
  "Return pending materialize timer for WORKSPACE, or nil."
  (alist-get
   workspace org-bake-process--workspace-materialize-timers))

(defun org-bake-process-clear-materialize-timers ()
  "Cancel all pending materialize timers."
  (dolist (entry org-bake-process--workspace-materialize-timers)
    (when (timerp (cdr entry))
      (cancel-timer (cdr entry))))
  (setq org-bake-process--workspace-materialize-timers nil))

(defun org-bake-process-schedule-workspace-materialization
    (workspace &optional force)
  "Debounce materialization rebuild for WORKSPACE.

When FORCE is non-nil, rebuild all registered materializers."
  (unless (timerp
           (org-bake-process--workspace-materialize-timer workspace))
    (let ((timer
           (run-with-timer
            (max 0.0 (or org-bake-materialize-debounce-seconds 0.5))
            nil
            (lambda ()
              (setq org-bake-process--workspace-materialize-timers
                    (assq-delete-all
                     workspace
                     org-bake-process--workspace-materialize-timers))
              (org-bake-process-enqueue-materializers workspace force)
              (org-bake-process-run-materializers workspace)))))
      (push (cons workspace timer)
            org-bake-process--workspace-materialize-timers))))

(provide 'org-bake-process)
;;; org-bake-process.el ends here
