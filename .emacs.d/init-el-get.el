(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle 'helm)
(el-get-bundle 'auto-complete)
(el-get-bundle 'yasnippet)
(el-get-bundle 'yasnippet-snippets)
(el-get-bundle 'helm-c-yasnippet)
(el-get-bundle 'jedi)
(el-get-bundle 'clang-format)
(el-get-bundle 'magit)
(el-get-bundle 'glsl-mode)
(el-get-bundle 'dockerfile-mode)
(el-get-bundle 'rust-mode)
(el-get-bundle 'racer)
(el-get-bundle 'company-mode)
(el-get-bundle 'flycheck-rust)
(el-get-bundle 'rust-lang-nursery/rust-clippy)
(el-get-bundle 'cargo)
(el-get-bundle 'flycheck)
(el-get-bundle 'flycheck/flycheck-inline)
(el-get-bundle 'docker-tramp)
(el-get-bundle 'markdown-mode)
(el-get-bundle 'py-autopep8)
(el-get-bundle 'python-mode)
(el-get-bundle 'toml-mode)
(el-get-bundle 'use-package)
(el-get-bundle 'rtags)
