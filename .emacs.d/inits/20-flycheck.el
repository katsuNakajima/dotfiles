(use-package flycheck
  :config
  (use-package flycheck-inline
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
    )
  )
