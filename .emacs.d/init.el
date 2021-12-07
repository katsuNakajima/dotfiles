(defvar bootstrap-version)
(let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
         (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
                "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package el-patch)

(use-package init-loader)
(init-loader-load (concat user-emacs-directory "inits"))

(setq custom-file "~/.emacs.d/site-lisp/custom.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/site-lisp/custom.el"))
    (load (expand-file-name custom-file) t nil nil))

(el-patch-validate-all)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
