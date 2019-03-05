(add-to-list 'load-path "~/.local/bin")
(use-package flycheck
  :hook
  ((python-mode . flycheck-mode))
  :config
  (use-package flycheck-inline
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
    )
  )
