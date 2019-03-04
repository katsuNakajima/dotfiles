(use-package company
  :bind (:map company-mode-map
              ("C-c TAB" . company-complete))
  :config
  (global-company-mode)

  :diminish company-mode)
