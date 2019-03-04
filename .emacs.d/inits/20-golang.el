;;;;--------------------------------------------------------
;;;; go-mode
;;;;--------------------------------------------------------
(defvar my/helm-go-source
  '((name . "Helm Go")
    (candidates . (lambda ()
                    (cons "builtin" (go-packages))))
    (action . (("Show document" . godoc)
               ("Import package" . my/helm-go-import-add)))))

(defun my/helm-go-import-add (candidate)
  (dolist (package (helm-marked-candidates))
    (go-import-add current-prefix-arg package)))

(defun my/helm-go ()
  (interactive)
  (helm :sources '(my/helm-go-source) :buffer "*helm go*"))
