(add-to-list 'load-path "~/.local/bin")
(use-package python-mode
  :bind (:map python-mode-map
              ("C-c F" . py-autopep8))
  :config
  (add-hook 'before-save-hook 'py-autopep8-before-save)
  )
