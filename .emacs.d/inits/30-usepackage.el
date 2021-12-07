(use-package all-the-icons)

(use-package nyan-mode
    :config
    (nyan-mode 1))

(use-package doom-themes
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :custom-face
    (doom-modeline-bar ((t (:background "#6272a4"))))
    :config
    (load-theme 'doom-one t)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    )

(use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    )

(use-package hide-mode-line
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package counsel
    :after ivy
    :config (counsel-mode))

(use-package ivy
    :defer 0.1
    :diminish
    :bind
    (("C-c C-r" . ivy-resume)
        ("C-x B" . ivy-switch-buffer-other-window))
    :custom
    (ivy-count-format "(%d/%d) ")
    (ivy-use-virtual-buffers t)
    :config
    (ivy-mode))

(use-package all-the-icons-ivy-rich
    :after ivy
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
    :after ivy
    :ensure t
    :init (ivy-rich-mode 1))

(use-package swiper
    :after ivy
    :bind
    (("C-s" . swiper)
        ("C-r" . swiper)))

(use-package magit
    :bind
    (("C-x g" . magit-status)))

(use-package company
    :bind
    (:map company-mode-map
        ("C-c TAB" . company-complete))
    (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("TAB" . company-complete-selection))
    (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
    :config
    (global-company-mode)
    :custom
    (company-idle-delay 0)
    (company-minimum-prefix-length .5)
    (company-selection-wrap-around t)
    :diminish company-mode)

(use-package irony
    :config
    (use-package company-irony
        :config
        (push 'company-irony company-backends)))

(use-package company-box
    :hook (company-mode . company-box-mode))

(use-package clang-format
    :config
    (bind-key "C-c r" 'clang-format-region)
    (bind-key "C-c u" 'clang-format-buffer)
    (setq clang-format-style-option "file"))

(use-package dockerfile-mode
    :mode (("Dockerfile\\'" . dockerfile-mode))
    )

(use-package projectile
    :ensure t
    :diminish
    :custom
    (projectile-switch-project-action 'projectile-dired)
    :config
    (projectile-mode +1)
    (when (executable-find "ghq")
        (setq projectile-known-projects
            (mapcar
                (lambda (x) (abbreviate-file-name x))
                (split-string (shell-command-to-string "ghq list --full-path")))))
    (defun projectile-project-find-function (dir)
        (let* ((root (projectile-project-root dir)))
            (and root (cons 'transient root))))
    (with-eval-after-load 'project
        (add-to-list 'project-find-functions 'projectile-project-find-function))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    )

(use-package lsp-mode
    :ensure t
    :init (yas-global-mode)
    :hook (rust-mode . lsp)
    :bind ("C-c h" . lsp-describe-thing-at-point)
    :custom
    (lsp-response-timeout 5)
    (lsp-prefer-flymake 'flymake)
    (lsp-rust-server 'rust-analyzer)
    )

(use-package lsp-ui
    :ensure t
    :custom
    (lsp-ui-doc-position 'bottom) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    (lsp-ui-flycheck-enable nil)
    (lsp-ui-sideline-enable nil)
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
        (interactive)
        (if lsp-ui-doc-mode
            (progn
                (lsp-ui-doc-mode -1)
                (lsp-ui-doc--hide-frame))
            (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ("C-c i"   . lsp-ui-peek-find-implementation)
        ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c f"   . lsp-format-buffer)
        ("C-c d"   . ladicle/toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode)
    )

;; cclsは別途hookする
(use-package ccls
    :custom (ccls-executable "/usr/local/bin/ccls")
    :hook ((c-mode c++-mode objc-mode) .
              (lambda () (require 'ccls) (lsp))))

(use-package flymake
    :config
    (setq flymake-no-changes-timeout 2)
    ;; https://github.com/emacs-ess/ESS/issues/883
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
    )

(use-package flymake-diagnostic-at-point
    :after flymake
    :custom
    (flymake-diagnostic-at-point-timer-delay 0.1)
    (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
    :hook
    (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package markdown-mode
    :mode
    (("\\.text\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode)
        ("\\.md\\'" . markdown-mode))
    )

(use-package plantuml-mode
    :mode
    (("\\.plantuml\\'" . plantuml-mode))
    :config
    (setq plantuml-jar-path "/home/nakaji-wsl/plantuml.jar")
    (setq plantuml-default-exec-mode 'jar)
    (setq plantuml-output-type "png")
    ;;日本語を含むUMLを書く場合はUTF-8を指定
    (setq plantuml-options "-charset UTF-8")
    )

(use-package cmake-mode
    :mode
    (("CMakeLists\\.txt\\'" . cmake-mode))
    )

(use-package yaml-mode
    :mode (("\\.yml\\'" . yaml-mode))
    :bind ( :map yaml-mode-map
              ("C-m" . newline-and-indent))
    )

(use-package yasnippet
    :init
    (use-package yasnippet-snippets)
    (yas-global-mode 1)
    (add-to-list 'yas/root-directory "~/.emacs.d/snippets")
    (yas/initialize)
    )

(use-package beacon
    :custom
    (beacon-color "yellow")
    :config
    (beacon-mode 1))

(use-package atomic-chrome)
(atomic-chrome-start-server)

(use-package git-gutter
    :custom
    (git-gutter:modified-sign "~")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
    :custom-face
    (git-gutter:modified ((t (:background "#f1fa8c"))))
    (git-gutter:added    ((t (:background "#50fa7b"))))
    (git-gutter:deleted  ((t (:background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1))

(use-package highlight-indent-guides
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled t)
    (highlight-indent-guides-responsive t)
    (highlight-indent-guides-method 'character))

(use-package paren
    :ensure nil
    :hook
    (after-init . show-paren-mode)
    :custom-face
    (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
    :custom
    (show-paren-style 'mixed)
    (show-paren-when-point-inside-paren t)
    (show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))

(use-package volatile-highlights
    :diminish
    :hook
    (after-init . volatile-highlights-mode)
    :custom-face
    (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

(use-package which-key
    :diminish which-key-mode
    :hook (after-init . which-key-mode))

(use-package amx)

(use-package editorconfig
    :init
    (editorconfig-mode 1)
    )

(add-to-list 'exec-path (expand-file-name "/opt/homebrew/Cellar"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

(use-package rust-mode
    :ensure t
    :custom rust-format-on-save t)

(use-package cargo
    :ensure t
    :hook (rust-mode . cargo-minor-mode))

(use-package lsp-pyright
    :ensure t
    :hook (python-mode . (lambda ()
                             (require 'lsp-pyright)
                             (lsp))))  ;; or lsp-deferred

(use-package ace-window
    :bind
    (("C-'" . ace-window))
    :custom-face
    (aw-leading-char-face ((t (:height 3.0 :foreground "#f1fa8c"))))
    )

(use-package emoji-cheat-sheet-plus
    :defer t
    :init
    (progn
        ;; enabled emoji in buffer
        (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
        (add-hook 'markdown-mode-hook 'emoji-cheat-sheet-plus-display-mode)
        (add-hook 'magit-log-mode-hook 'emoji-cheat-sheet-plus-display-mode)))

(use-package google-translate
    :bind
    (("C-c t" . google-translate-enja-or-jaen))
    :config
    (defun google-translate-enja-or-jaen (&optional string)
        "Translate words in region or current position. Can also specify query with C-u"
        (interactive)
        (setq string
            (cond ((stringp string) string)
                (current-prefix-arg
                    (read-string "Google Translate: "))
                ((use-region-p)
                    (buffer-substring (region-beginning) (region-end)))
                (t
                    (thing-at-point 'word))))
        (let* ((asciip (string-match
                           (format "\\`[%s]+\\'" "[:ascii:]’“”–")
                           string)))
            (run-at-time 0.1 nil 'deactivate-mark)
            (google-translate-translate
                (if asciip "en" "ja")
                (if asciip "ja" "en")
                string)))

    (defun remove-c-comment (args)
        (let ((text (nth 2 args)))
            (setf (nth 2 args)
                (replace-regexp-in-string "\n" " "
                    (replace-regexp-in-string "[ \t]*//[/*!]*[ \t]+" ""
                        (replace-regexp-in-string "[ \t]+\\(\\*[ \t]+\\)+" " " text))))
            args))

    (advice-add 'google-translate-request :filter-args
        #'remove-c-comment)

    :config/el-patch
    (el-patch-defun google-translate--search-tkk ()
        "Search TKK."
        (el-patch-swap
            (let ((start nil)
                     (tkk nil)
                     (nums '()))
                (setq start (search-forward ",tkk:'"))
                (search-forward "',")
                (backward-char 2)
                (setq tkk (buffer-substring start (point)))
                (setq nums (split-string tkk "\\."))
                (list (string-to-number (car nums))
                    (string-to-number (car (cdr nums)))))
            (list 430675 2721866130)))
    )
