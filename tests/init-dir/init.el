(setq load-prefer-newer t)

(setq package-enable-at-startup nil)

(let* ((init-dir
        (file-name-directory (or load-file-name buffer-file-name)))
       (repo-root (expand-file-name "../.." init-dir))
       (xdg-data-home (expand-file-name "xdg-data" init-dir)))
  (add-to-list 'load-path repo-root)
  (setenv "XDG_DATA_HOME" xdg-data-home)
  (setq user-emacs-directory init-dir)
  (setq org-id-locations-file
        (expand-file-name ".org-id-locations" user-emacs-directory))
  (make-directory (file-name-directory org-id-locations-file) t)
  (require 'org-bake)
  (setq org-bake-store-root nil)
  (setq org-bake-workspaces
        `((test :roots (,(expand-file-name "../org" init-dir))))))
(org-bake-mode 1)
(org-bake-use-workspace-agenda-file 'test)
