;;; org-bake-export.el --- Org export pipeline for org-bake  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; URL: https://github.com/bitbloxhub/org-bake
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Export logic for org-bake.
;;
;; This file is responsible for turning Org documents into JSON-oriented
;; intermediate or final forms, using Org and ox-json.


;;; Code:

(require 'async)
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'ox-json)
(require 'org-bake-process)
(require 'org-bake-store)
(require 'subr-x)
(require 'seq)


(defconst org-bake-export-format-version 1
  "Version for org-bake export wrapper metadata.")

(defvar org-bake-store-schema-version)

(defun org-bake-export--source-metadata (source-path)
  "Return source metadata plist for SOURCE-PATH."
  (org-bake-store-source-state source-path))

(defun org-bake-export--normalized-document
    (job exported &optional exported-at)
  "Return normalized stored document for JOB from EXPORTED ox-json tree.

Optional EXPORTED-AT overrides export timestamp."
  (let* ((source-path (plist-get job :source-path))
         (workspace (plist-get job :workspace))
         (state (org-bake-export--source-metadata source-path)))
    `((schema_version . ,org-bake-store-schema-version)
      (type . "document")
      (id . ,(plist-get job :document-id))
      (workspace . ,(symbol-name workspace))
      (source
       .
       ((path . ,(plist-get state :path))
        (mtime . ,(plist-get state :mtime))
        (size . ,(plist-get state :size))))
      (export
       .
       ((exported_at . ,(or exported-at (floor (float-time))))
        (format_version . ,org-bake-export-format-version)))
      (document . ,exported))))


(defvar org-bake-export-resolve-id-links nil)

(defun org-bake-export--collect-tree-properties-without-id-resolution
    (data info)
  "Collect export tree properties from DATA and INFO without org-id lookups.

This avoids `org-id' lookups for `id:' links."
  (setq info (plist-put info :parse-tree data))
  (setq
   info
   (plist-put
    info
    :headline-offset (- 1 (org-export--get-min-level data info))))
  (org-combine-plists
   info
   (list
    :headline-numbering
    (org-export--collect-headline-numbering data info)
    :id-alist nil)))

(defun org-bake-export--export-as-json ()
  "Export current Org buffer to ox-json with org-bake defaults."
  (let ((options '(:with-broken-links mark :json-postprocess nil)))
    (if org-bake-export-resolve-id-links
        (org-export-as 'json nil nil nil options)
      (cl-letf
          (((symbol-function 'org-export--collect-tree-properties)
            #'org-bake-export--collect-tree-properties-without-id-resolution))
        (org-export-as 'json nil nil nil options)))))
(defun org-bake-export-document (job)
  "Export JOB synchronously and write normalized document JSON."
  (let* ((source-path (plist-get job :source-path))
         (exported
          (with-temp-buffer
            (insert-file-contents source-path)
            (setq-local buffer-file-name source-path)
            (org-mode)
            (json-parse-string (org-bake-export--export-as-json)
                               :object-type 'hash-table
                               :array-type 'array
                               :null-object nil
                               :false-object
                               :false)))
         (document
          (org-bake-export--normalized-document
           job exported
           (floor (float-time)))))
    (org-bake-store-write-json-file
     (plist-get job :document-path) document)
    document))

(defun org-bake-export-document-async-backend
    (source-path
     workspace-name document-id schema-version format-version)
  "Worker-side backend for async Org export.

SOURCE-PATH is exported for WORKSPACE-NAME into DOCUMENT-ID using
SCHEMA-VERSION and FORMAT-VERSION metadata."
  (require 'org)
  (require 'ox-json)
  (require 'json)

  (let* ((source-attrs (file-attributes source-path))
         (source-mtime
          (floor
           (float-time
            (file-attribute-modification-time source-attrs))))
         (source-size (file-attribute-size source-attrs))
         (exported-at (floor (float-time)))
         (exported
          (with-temp-buffer
            (insert-file-contents source-path)
            (setq-local buffer-file-name source-path)
            (org-mode)
            (json-parse-string (org-bake-export--export-as-json)
                               :object-type 'hash-table
                               :array-type 'array
                               :null-object nil
                               :false-object
                               :false))))
    `((schema_version . ,schema-version)
      (type . "document")
      (id . ,document-id)
      (workspace . ,workspace-name)
      (source
       .
       ((path . ,source-path)
        (mtime . ,source-mtime)
        (size . ,source-size)))
      (export
       .
       ((exported_at . ,exported-at)
        (format_version . ,format-version)))
      (document . ,exported))))

(defun org-bake-export-document-async (job &optional callback)
  "Export JOB in child Emacs process.

CALLBACK is called with normalized document after it is written."
  (let* ((source-path (plist-get job :source-path))
         (document-path (plist-get job :document-path))
         (workspace-name (symbol-name (plist-get job :workspace)))
         (document-id (plist-get job :document-id))
         (schema-version org-bake-store-schema-version)
         (format-version org-bake-export-format-version)
         (parent-load-path load-path))
    (async-start
     `(lambda ()
        (setq load-path ',parent-load-path)
        (setq load-prefer-newer t)
        (require 'org-bake-export)
        (setq org-id-track-globally org-bake-export-resolve-id-links)
        (when org-bake-export-resolve-id-links
          (setq org-id-locations-file
                (make-temp-file "org-bake-org-id-locations-")))
        (org-bake-export-document-async-backend
         ,source-path
         ,workspace-name
         ,document-id
         ,schema-version
         ,format-version))
     (lambda (document)
       (org-bake-store-write-json-file document-path document)
       (when callback
         (funcall callback document))))))

(defun org-bake-export-queued-job (job &optional callback)
  "Start async export for queued JOB.

Optional CALLBACK receives exported document."
  (org-bake-export-document-async job callback))
(defvar org-bake-max-export-jobs)
(defvar org-bake-export-batch-size)
(defvar org-bake-export-dynamic-batch-size)
(defvar org-bake-export-batch-size-min)
(defvar org-bake-export-batch-size-max)
(defvar org-bake-export-batch-target-seconds)
(defvar org-bake-export-batch-throughput-alpha)
(defvar org-bake-export--workspace-workers nil
  "Alist mapping workspace symbols to export worker process lists.")

(defvar org-bake-export--workspace-callbacks nil
  "Alist mapping workspace symbols to export completion callbacks.")

(defun org-bake-export--workspace-callback (workspace)
  "Return registered export callback for WORKSPACE, or nil."
  (alist-get workspace org-bake-export--workspace-callbacks))

(defun org-bake-export--set-workspace-callback (workspace callback)
  "Set export CALLBACK for WORKSPACE."
  (setq org-bake-export--workspace-callbacks
        (assq-delete-all
         workspace org-bake-export--workspace-callbacks))
  (when callback
    (push
     (cons workspace callback) org-bake-export--workspace-callbacks))
  callback)

(defun org-bake-export--workspace-workers (workspace)
  "Return live export worker processes for WORKSPACE."
  (let* ((workers
          (alist-get workspace org-bake-export--workspace-workers))
         (live-workers (seq-filter #'process-live-p workers)))
    (setq org-bake-export--workspace-workers
          (assq-delete-all
           workspace org-bake-export--workspace-workers))
    (when live-workers
      (push (cons workspace live-workers)
            org-bake-export--workspace-workers))
    live-workers))

(defun org-bake-export--set-workspace-workers (workspace workers)
  "Set WORKERS process list for WORKSPACE."
  (setq org-bake-export--workspace-workers
        (assq-delete-all
         workspace org-bake-export--workspace-workers))
  (when workers
    (push
     (cons workspace workers) org-bake-export--workspace-workers))
  workers)

(defun org-bake-export--workspace-idle-p (workspace)
  "Return non-nil when WORKSPACE has no queued or running export jobs."
  (not
   (seq-some
    (lambda (job)
      (and (eq (plist-get job :workspace) workspace)
           (eq (plist-get job :kind) 'export-document)
           (memq (plist-get job :status) '(queued running))))
    org-bake-process-queue)))

(defun org-bake-export--batch-size-limits ()
  "Return normalized (MIN . MAX) limits for dynamic batch sizing."
  (let* ((minimum (max 1 (or org-bake-export-batch-size-min 1)))
         (maximum
          (max minimum (or org-bake-export-batch-size-max minimum))))
    (cons minimum maximum)))

(defun org-bake-export--worker-batch-size (worker)
  "Return current export batch size for WORKER."
  (let* ((limits (org-bake-export--batch-size-limits))
         (minimum (car limits))
         (maximum (cdr limits))
         (fallback
          (max minimum (or org-bake-export-batch-size minimum))))
    (max minimum
         (min maximum
              (or (process-get
                   worker 'org-bake-export-worker-batch-size)
                  fallback)))))

(defun org-bake-export--worker-next-batch-size (worker jobs elapsed)
  "Update and return next batch size for WORKER from JOBS and ELAPSED seconds."
  (let* ((limits (org-bake-export--batch-size-limits))
         (minimum (car limits))
         (maximum (cdr limits))
         (current (org-bake-export--worker-batch-size worker)))
    (if (not org-bake-export-dynamic-batch-size)
        current
      (let* ((doc-count (max 1 (length jobs)))
             (duration (max 0.001 elapsed))
             (sample-throughput (/ doc-count duration))
             (alpha
              (max 0.0
                   (min 1.0
                        (or org-bake-export-batch-throughput-alpha
                            0.3))))
             (prior-throughput
              (process-get worker 'org-bake-export-worker-throughput))
             (throughput
              (if prior-throughput
                  (+ (* alpha sample-throughput)
                     (* (- 1.0 alpha) prior-throughput))
                sample-throughput))
             (target-seconds
              (max 0.1 (or org-bake-export-batch-target-seconds 1.5)))
             (next
              (max minimum
                   (min maximum
                        (round (* throughput target-seconds))))))
        (process-put
         worker 'org-bake-export-worker-throughput throughput)
        (process-put worker 'org-bake-export-worker-batch-size next)
        next))))

(defun org-bake-export--worker-backoff-batch-size (worker)
  "Back off WORKER batch size after an export batch error."
  (if (not org-bake-export-dynamic-batch-size)
      (org-bake-export--worker-batch-size worker)
    (let* ((limits (org-bake-export--batch-size-limits))
           (minimum (car limits))
           (current (org-bake-export--worker-batch-size worker))
           (next (max minimum (/ current 2))))
      (process-put worker 'org-bake-export-worker-batch-size next)
      next)))

(defun org-bake-export-document-batch-async-backend
    (jobs schema-version format-version)
  "Worker-side backend exporting JOBS in one child process.

SCHEMA-VERSION and FORMAT-VERSION are written into each document."
  (mapcar
   (lambda (job)
     (org-bake-export-document-async-backend
      (plist-get job :source-path)
      (symbol-name (plist-get job :workspace))
      (plist-get job :document-id)
      schema-version
      format-version))
   jobs))

(defun org-bake-export--on-batch-complete (workspace worker message)
  "Handle completed export batch MESSAGE from WORKER in WORKSPACE."
  (let* ((jobs (plist-get message :jobs))
         (documents (plist-get message :documents))
         (callback (org-bake-export--workspace-callback workspace))
         (started-at
          (process-get worker 'org-bake-export-batch-started-at))
         (elapsed
          (if started-at
              (- (float-time) started-at)
            0.0))
         (job-tail jobs)
         (document-tail documents))
    (while (and job-tail document-tail)
      (let ((job (car job-tail))
            (document (car document-tail)))
        (org-bake-store-write-json-file
         (plist-get job :document-path) document)
        (when callback
          (funcall callback job)))
      (setq
       job-tail (cdr job-tail)
       document-tail (cdr document-tail)))
    (dolist (job jobs)
      (org-bake-process-delete-job (plist-get job :id)))
    (org-bake-export--worker-next-batch-size worker jobs elapsed)
    (process-put worker 'org-bake-export-batch-started-at nil)
    (process-put worker 'org-bake-export-worker-busy nil)
    (process-put worker 'org-bake-export-current-jobs nil)
    (org-bake-export--dispatch-queued-jobs workspace callback)
    (when (org-bake-export--workspace-idle-p workspace)
      (org-bake-process-schedule-workspace-materialization
       workspace t))))

(defun org-bake-export--on-batch-error (workspace worker message)
  "Handle failed export batch MESSAGE from WORKER in WORKSPACE."
  (let ((jobs (plist-get message :jobs))
        (error-text (plist-get message :error))
        (callback (org-bake-export--workspace-callback workspace)))
    (message "org-bake export worker error in %s: %s"
             workspace
             error-text)
    (dolist (job jobs)
      (org-bake-process-delete-job (plist-get job :id))
      (when callback
        (funcall callback job)))
    (org-bake-export--worker-backoff-batch-size worker)
    (process-put worker 'org-bake-export-batch-started-at nil)
    (process-put worker 'org-bake-export-worker-busy nil)
    (process-put worker 'org-bake-export-current-jobs nil)
    (org-bake-export--dispatch-queued-jobs workspace callback)))

(defun org-bake-export--remove-worker (workspace worker)
  "Remove WORKER from WORKSPACE worker list."
  (org-bake-export--set-workspace-workers
   workspace
   (delq worker (org-bake-export--workspace-workers workspace))))

(defun org-bake-export--handle-worker-exit (workspace worker _result)
  "Handle WORKER exit for WORKSPACE."
  (let ((in-flight (process-get worker 'org-bake-export-current-jobs))
        (shutting-down
         (process-get worker 'org-bake-export-worker-shutdown))
        (callback (org-bake-export--workspace-callback workspace)))
    (org-bake-export--remove-worker workspace worker)
    (unless shutting-down
      (dolist (job in-flight)
        (org-bake-process-update-job-status
         (plist-get job :id) 'queued))
      (org-bake-export--dispatch-queued-jobs workspace callback))))

(defun org-bake-export--start-worker (workspace callback)
  "Start and return one persistent export worker for WORKSPACE.

CALLBACK runs after each completed job."
  (let ((parent-load-path load-path)
        worker)
    (setq
     worker
     (async-start
      `(lambda ()
         (setq load-path ',parent-load-path)
         (setq load-prefer-newer t)
         (require 'org-bake-export)
         (setq org-id-track-globally org-bake-export-resolve-id-links)
         (when org-bake-export-resolve-id-links
           (setq org-id-locations-file
                 (make-temp-file "org-bake-org-id-locations-")))
         (let ((running t)
               (worker-profile-enabled
                (string=
                 (or (getenv "ORG_BAKE_BENCH_WORKER_PROFILE_ENABLED")
                     "0")
                 "1"))
               (worker-profile-dir
                (getenv "ORG_BAKE_BENCH_WORKER_PROFILE_DIR"))
               (worker-profile-counter 0))
           (while running
             (let ((message (async-receive)))
               (pcase (plist-get message :op)
                 (:export-batch
                  (let ((jobs (plist-get message :jobs))
                        (schema-version
                         (plist-get message :schema-version))
                        (format-version
                         (plist-get message :format-version)))
                    (condition-case err
                        (let (documents)
                          (if (and worker-profile-enabled
                                   worker-profile-dir
                                   (> (length worker-profile-dir) 0))
                              (progn
                                (make-directory worker-profile-dir t)
                                (profiler-start 'cpu+mem)
                                (unwind-protect
                                    (setq
                                     documents
                                     (org-bake-export-document-batch-async-backend
                                      jobs
                                      schema-version
                                      format-version))
                                  (profiler-stop)
                                  (setq worker-profile-counter
                                        (1+ worker-profile-counter))
                                  (let* ((prefix
                                          (format
                                           "worker-p%s-n%06d-j%d"
                                           (emacs-pid)
                                           worker-profile-counter
                                           (length jobs)))
                                         (cpu-path
                                          (expand-file-name
                                           (concat prefix "-cpu.prof")
                                           worker-profile-dir))
                                         (mem-path
                                          (expand-file-name
                                           (concat prefix "-mem.prof")
                                           worker-profile-dir)))
                                    (profiler-write-profile
                                     (profiler-cpu-profile) cpu-path)
                                    (profiler-write-profile
                                     (profiler-memory-profile)
                                     mem-path)
                                    (when (fboundp 'profiler-reset)
                                      (profiler-reset)))))
                            (setq
                             documents
                             (org-bake-export-document-batch-async-backend
                              jobs schema-version format-version)))
                          (async-send
                           :op
                           :batch-complete
                           :jobs jobs
                           :documents documents))
                      (error
                       (async-send
                        :op
                        :batch-error
                        :jobs jobs
                        :error (prin1-to-string err))))))
                 (:shutdown (setq running nil))
                 (_
                  (async-send
                   :op
                   :batch-error
                   :jobs nil
                   :error
                   (format "Unknown worker message op: %S"
                           (plist-get message :op)))))))
           :stopped))
      (lambda (result)
        (if (async-message-p result)
            (pcase (plist-get result :op)
              (:batch-complete
               (org-bake-export--on-batch-complete
                workspace worker result))
              (:batch-error
               (org-bake-export--on-batch-error
                workspace worker result))
              (_
               (message "org-bake export worker unknown message: %S"
                        result)))
          (org-bake-export--handle-worker-exit
           workspace worker result)))))
    (process-put worker 'org-bake-export-worker-busy nil)
    (process-put worker 'org-bake-export-worker-shutdown nil)
    (process-put worker 'org-bake-export-current-jobs nil)
    (process-put
     worker
     'org-bake-export-worker-batch-size
     (max 1 (or org-bake-export-batch-size 1)))
    (process-put worker 'org-bake-export-worker-throughput nil)
    (process-put worker 'org-bake-export-batch-started-at nil)
    (org-bake-export--set-workspace-workers
     workspace
     (append
      (org-bake-export--workspace-workers workspace) (list worker)))
    (org-bake-export--set-workspace-callback workspace callback)
    worker))

(defun org-bake-export--ensure-worker-pool (workspace callback)
  "Ensure WORKSPACE has enough export workers for configured limit.

When non-nil, CALLBACK is registered as workspace export callback."
  (let ((workers (org-bake-export--workspace-workers workspace))
        (limit (max 1 (or org-bake-max-export-jobs 1))))
    (when callback
      (org-bake-export--set-workspace-callback workspace callback))
    (while (< (length workers) limit)
      (org-bake-export--start-worker
       workspace (org-bake-export--workspace-callback workspace))
      (setq workers (org-bake-export--workspace-workers workspace)))
    workers))

(defun org-bake-export--assign-worker-batch
    (workspace worker schema-version format-version)
  "Assign one export batch to WORKER in WORKSPACE.

Return started jobs list."
  (let* ((batch-size (org-bake-export--worker-batch-size worker))
         (queued
          (seq-filter
           (lambda (job) (eq (plist-get job :status) 'queued))
           (org-bake-process-queued-jobs workspace 'export-document)))
         (batch (seq-take queued batch-size)))
    (when batch
      (dolist (job batch)
        (org-bake-process-update-job-status
         (plist-get job :id) 'running))
      (process-put worker 'org-bake-export-worker-busy t)
      (process-put worker 'org-bake-export-current-jobs batch)
      (process-put
       worker 'org-bake-export-batch-started-at (float-time))
      (async-send
       worker
       :op
       :export-batch
       :jobs batch
       :schema-version schema-version
       :format-version format-version)
      batch)))

(defun org-bake-export--dispatch-queued-jobs
    (&optional workspace callback)
  "Start queued export jobs for WORKSPACE up to configured concurrency limit.

When non-nil, CALLBACK is registered and used for started jobs.
Return jobs started by this dispatch call."
  (if (null workspace)
      (seq-mapcat
       (lambda (ws)
         (org-bake-export--dispatch-queued-jobs ws callback))
       (delete-dups
        (mapcar
         (lambda (job) (plist-get job :workspace))
         (org-bake-process-queued-jobs nil 'export-document))))
    (let* ((queued
            (seq-filter
             (lambda (job) (eq (plist-get job :status) 'queued))
             (org-bake-process-queued-jobs
              workspace 'export-document))))
      (when queued
        (let* ((schema-version org-bake-store-schema-version)
               (format-version org-bake-export-format-version)
               (workers
                (org-bake-export--ensure-worker-pool
                 workspace callback))
               (idle-workers
                (seq-filter
                 (lambda (worker)
                   (not
                    (process-get
                     worker 'org-bake-export-worker-busy)))
                 workers))
               started-jobs)
          (dolist (worker idle-workers)
            (let ((started
                   (org-bake-export--assign-worker-batch
                    workspace worker schema-version format-version)))
              (when started
                (setq started-jobs (append started-jobs started)))))
          (when (org-bake-export--workspace-idle-p workspace)
            (org-bake-process-schedule-workspace-materialization
             workspace t))
          started-jobs)))))

(defun org-bake-export-stop-workers (&optional workspace)
  "Stop persistent export workers.

When WORKSPACE is non-nil, stop only workers for that workspace."
  (if workspace
      (let ((workers (org-bake-export--workspace-workers workspace)))
        (dolist (worker workers)
          (process-put worker 'org-bake-export-worker-shutdown t)
          (when (process-live-p worker)
            (async-send worker :op :shutdown)))
        (org-bake-export--set-workspace-workers workspace nil)
        (org-bake-export--set-workspace-callback workspace nil))
    (dolist (entry (copy-sequence org-bake-export--workspace-workers))
      (org-bake-export-stop-workers (car entry)))))

(defun org-bake-export-queued-jobs (&optional workspace callback)
  "Start queued export jobs for WORKSPACE and return newly started jobs.

When WORKSPACE is nil, export queued document jobs from all workspaces.
Optional CALLBACK receives each completed job."
  (org-bake-export--dispatch-queued-jobs workspace callback))

(provide 'org-bake-export)
;;; org-bake-export.el ends here
