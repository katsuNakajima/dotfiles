(use-package projectile
  :config
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))
  )

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c C-d" . eglot-help-at-point)
              ("C-c C-r" . eglot-code-actions))
  :hook
  ((c-mode-common . eglot-ensure)
   (python-mode-comon . eglot-ensure))
  )
