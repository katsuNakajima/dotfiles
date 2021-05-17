(defun my-paste-function ()
    (interactive)
    (shell-command-to-string "xclip -o"))

(defun my-cut-function (text &optional rest)
    (interactive)
    (let ((process-connection-type nil))
    (let ((proc (start-process "xclip" "*Messages*" "xclip")))
        (process-send-string proc text)
        (process-send-eof proc))))

(when (and (not window-system)
    (executable-find "xclip"))
    (setq interprogram-cut-function 'my-cut-function)
    (setq interprogram-paste-function 'my-paste-function))

(defun x-clipboard-copy ()
    (interactive)
    (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "xsel -ib" nil nil)))

;makefile-mode with hard-tab
(add-hook 'makefile-mode-hook
    (function (lambda ()
    (setq indent-tabs-mode t))))

; 現在開いているバッファのディレクトリをtmuxで開く
(defun tmux-new-window ()
    (interactive)
    (if (string-equal major-mode "dired-mode")
        (let ((dirname (expand-file-name default-directory)))
        (shell-command (concat "tmux new-window -c \"" dirname "\"")))
    (let ((fname buffer-file-name))
    (if (stringp fname)
        (shell-command (concat "tmux new-window -c \"$(dirname " fname ")\""))))))
