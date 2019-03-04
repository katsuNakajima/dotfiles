;;;;--------------------------------------------------------
;;;; jedi
;;;;--------------------------------------------------------
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;;;--------------------------------------------------------
;;;; autopep8 & pylint on flycheck
;;;;--------------------------------------------------------
(add-to-list 'load-path "~/.local/bin")
(require 'python-mode)
(define-key python-mode-map (kbd "C-c F") 'py-autopep8)          ; バッファ全体のコード整形
(define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)   ; 選択リジョン内のコード整形
(add-hook 'before-save-hook 'py-autopep8-before-save)

(defun flycheck-python-setup ()
  (flycheck-mode))
(add-hook 'python-mode-hook #'flycheck-python-setup)
