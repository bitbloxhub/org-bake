(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(let* ((repo-root
        (or (getenv "ORG_BAKE_BENCH_REPO")
            (error "ORG_BAKE_BENCH_REPO is not set")))
       (workspace-root
        (or (getenv "ORG_BAKE_BENCH_ROOT")
            (error "ORG_BAKE_BENCH_ROOT is not set")))
       (user-dir (expand-file-name ".bench-emacs/" workspace-root))
       (xdg-data-home
        (expand-file-name ".bench-xdg-data/" workspace-root)))
  (add-to-list 'load-path repo-root)
  (setq user-emacs-directory user-dir)
  (setq org-id-track-globally nil)
  (make-directory user-emacs-directory t)
  (setenv "XDG_DATA_HOME" xdg-data-home)
  (setq org-bake-auto-index-on-startup nil)
  (require 'org-bake)
  (let* ((max-export-jobs (getenv "ORG_BAKE_BENCH_MAX_EXPORT_JOBS"))
         (default-export-jobs
          (max 1
               (or (and (fboundp 'num-processors) (num-processors))
                   1)))
         (export-batch-size
          (getenv "ORG_BAKE_BENCH_EXPORT_BATCH_SIZE"))
         (max-materialize-jobs
          (getenv "ORG_BAKE_BENCH_MAX_MATERIALIZE_JOBS"))
         (materialize-debounce
          (getenv "ORG_BAKE_BENCH_MATERIALIZE_DEBOUNCE")))
    (setq org-bake-max-export-jobs
          (if (and max-export-jobs (> (length max-export-jobs) 0))
              (string-to-number max-export-jobs)
            default-export-jobs))
    (when export-batch-size
      (setq org-bake-export-batch-size
            (string-to-number export-batch-size)))
    (when max-materialize-jobs
      (setq org-bake-max-materialize-jobs
            (string-to-number max-materialize-jobs)))
    (setq org-bake-materialize-debounce-seconds
          (if materialize-debounce
              (string-to-number materialize-debounce)
            0.0)))
  (setq org-bake-store-root
        (expand-file-name ".bench-store/" workspace-root))
  (setq org-bake-workspaces `((bench :roots (,workspace-root))))
  (org-bake-mode 1))
