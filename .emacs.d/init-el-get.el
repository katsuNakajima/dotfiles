(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle helm)
(el-get-bundle auto-complete)
(el-get-bundle yasnippet)
(el-get-bundle yasnippet-snippets)
(el-get-bundle helm-c-yasnippet)
(el-get-bundle jedi)
(el-get-bundle clang-format)
(el-get-bundle magit)
