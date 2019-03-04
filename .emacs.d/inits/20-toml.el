(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode))
  :config
  ((put 'upcase-region 'disabled nil))
  )
