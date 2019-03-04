(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode))
  :bind ( :map yaml-mode-map
               ("C-m" . newline-and-indent))
  )
