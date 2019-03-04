(use-package clang-format
  :config
  (bind-key "C-c r" 'clang-format-region)
  (bind-key "C-c u" 'clang-format-buffer)
  (setq clang-format-style-option "file"))
