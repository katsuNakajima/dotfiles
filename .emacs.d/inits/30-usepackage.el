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
    (load-theme 'doom-dracula t)
    (doom-themes-neotree-config)
    (doom-themes-org-config))

(use-package doom-modeline
      :custom
      (doom-modeline-buffer-file-name-style 'truncate-with-project)
      (doom-modeline-icon t)
      (doom-modeline-minor-modes nil)
      :hook
      (after-init . doom-modeline-mode)
      :config
      (line-number-mode 0)
      (column-number-mode 0)
      (doom-modeline-def-modeline 'main
    '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

(use-package hide-mode-line
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package helm
  :bind
  (("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-for-files)
   ("C-x I" . helm-semantic-or-imenu)
   ("C-x r l" . helm-bookmarks)
   ("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-<f6>" . helm-ls-git-ls)
   ("C-x C-r" . helm-recentf)
   ("C-M-z" . helm-resume)
   ("M-g g" . helm-do-grep)
   ("M-g l" . helm-occur-from-isearch)
   ("C-x C-d" . helm-browse-project))
  :config
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  
  (use-package helm-ls-git)
  (custom-set-variables
   '(helm-source-ls-git (helm-ls-git-build-ls-git-source))
   '(helm-source-ls-git-status (helm-ls-git-build-git-status-source))
   '(helm-for-files-preferred-list
     '(helm-source-buffers-list
       helm-source-recentf
       helm-source-files-in-current-dir
       helm-source-ls-git-status
       helm-source-ls-git
       helm-source-file-cache
       helm-source-locate
       )))
  (use-package helm-xref
    :config
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs))
  )

(use-package magit
  :bind
  (("C-x g" . magit-status))
  )

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

(use-package clang-format
  :config
  (bind-key "C-c r" 'clang-format-region)
  (bind-key "C-c u" 'clang-format-buffer)
  (setq clang-format-style-option "file"))

(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode))
  )

(use-package projectile
  :config
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))
  )

(use-package lsp-mode
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; always send incremental document
  (lsp-response-timeout 5)
  (lsp-prefer-flymake 'flymake)
  (lsp-enable-completion-at-point nil)
  :bind
  (:map lsp-mode-map
        ("C-c r"   . lsp-rename))
  :config
  (require 'lsp-clients)
  ;; LSP UI tools
  (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions t)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable nil)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
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
    (lsp-mode . lsp-ui-mode))
  ;; Lsp completion
  (use-package company-lsp
    :custom
    (company-lsp-cache-candidates t) ;; always using cache
    (company-lsp-async t)
    (company-lsp-enable-recompletion nil)))

;; cclsは別途hookする
(use-package ccls
  :custom (ccls-executable "/usr/local/bin/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package flycheck
  :config
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  )

(use-package flycheck-inline
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
  )

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

(use-package markdown-mode
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  )

(use-package arduino-mode
  :mode (("\\.ino\\'" . arduino-mode))
  )

(use-package cuda-mode
  :mode (("\\.cu\\'" . cuda-mode))
  )

(defun ladicle/task-clocked-time ()
  "Return a string with the clocked time and effort, if any"
  (interactive)
  (let* ((clocked-time (org-clock-get-clocked-time))
         (h (truncate clocked-time 60))
         (m (mod clocked-time 60))
         (work-done-str (format "%d:%02d" h m)))
    (if org-clock-effort
        (let* ((effort-in-minutes
                (org-duration-to-minutes org-clock-effort))
               (effort-h (truncate effort-in-minutes 60))
               (effort-m (truncate (mod effort-in-minutes 60)))
               (effort-str (format "%d:%02d" effort-h effort-m)))
          (format "%s/%s" work-done-str effort-str))
      (format "%s" work-done-str))))

(use-package tramp
  :config
  (setq tramp-copy-size-limit nil)
  (setq tramp-inline-compress-start-size nil)
  (setq password-cache-expiry nil)
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

(use-package rustic
  :after (lsp-mode)
  )

(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'lsp))

(use-package omnisharp
  :hook (csharp-mode . omnisharp-mode)
  :config
  (setq omnisharp-server-executable-path "/home/nakaji/omnisharp-1.32.18/run")
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook #'company-mode)
  )