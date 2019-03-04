(add-to-list 'load-path "~/.local/bin")
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (add-hook 'python-mode-hook #'flycheck-python-setup)
  (use-package flycheck-inline
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
    )
  )
