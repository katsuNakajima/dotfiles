;;;;--------------------------------------------------------
;;;; Dockerfile-mode
;;;;--------------------------------------------------------
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;If you don't, you'll be prompted for an image name each time you build. You may want to add the following to your emacs config:
    (put 'dockerfile-image-name 'safe-local-variable #'stringp)
