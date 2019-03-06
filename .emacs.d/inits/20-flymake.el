(use-package flymake
  :config
  (setq flymake-no-changes-timeout 2))

(use-package flymake-diagnostic-at-point
  :after flymake
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))
