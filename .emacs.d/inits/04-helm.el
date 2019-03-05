(use-package helm
  :bind
  (("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-for-files)
   ("C-x I" . helm-semantic-or-imenu)
   ("C-x r l" . helm-bookmarks)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-<f6>" . helm-ls-git-ls)
   ()
   ("C-x C-d" . helm-browse-project))
  :config
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  
  (use-package helm-ls-git)
  (custom-set-variables
   '(helm-source-ls-git (helm-ls-git-build-ls-git-source))
   '(helm-source-ls-git-status (helm-ls-git-build-git-status-source))
   '(helm-for-files-preferred-list
     '(helm-source-buffers-list
       helm-source-recentf
       helm-source-files-in-current-dir
       helm-source-ls-git-status
       helm-source-ls-git
       helm-source-file-cache
       helm-source-locate
       )))
  (use-package helm-xref
    :config
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs))
)
