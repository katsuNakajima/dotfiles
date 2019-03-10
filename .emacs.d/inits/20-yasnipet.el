(use-package yasnippet
  :init
  (use-package yasnippet-snippets)
  (yas-global-mode 1)
  (add-to-list 'yas/root-directory "~/.emacs.d/snippets")
  (yas/initialize)
)
