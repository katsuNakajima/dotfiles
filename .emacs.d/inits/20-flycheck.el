;;;;--------------------------------------------------------
;;;; flycheck
;;;;--------------------------------------------------------
(when (require 'flycheck nil 'noerror)
  (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
  (add-hook 'after-init-hook #'global-flycheck-mode))
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
