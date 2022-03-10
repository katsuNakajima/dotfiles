;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; My init.el.

;;; Code:

;; straight.el settings
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
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

;; leaf settings
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    ;;(leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert
    :config (leaf use-package :ensure t))
  (leaf leaf-tree
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :bind (("C-c e" . macrostep-expand)))

;; Emacs settings

;; Initial frame settings require HackGen35Nerd
(setq default-frame-alist
      (append (list
               '(font . "HackGen35Nerd-20"))
              default-frame-alist))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Katsuaki Nakajima")
            (user-mail-address . "karma.loveless@icloud.com")
            (user-login-name . "katsu")
            (create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            (use-dialog-box . nil)
            (use-file-dialog . nil)
            (menu-bar-mode . t)
            (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

;;Emacs implemented packages

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile))
  :mode-hook
  (c-mode-hook . ((c-set-style "bsd")
                  (setq c-basic-offset 4)))
  (c++-mode-hook . ((c-set-style "bsd")
                    (setq c-basic-offset 4))))

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

;; 3rd party packages

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :straight t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :straight t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :straight t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :straight t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :straight t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :straight t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :config
  (leaf consult-flycheck
    :doc "Provides the command `consult-flycheck'"
    :req "consult-0.8" "flycheck-31" "emacs-26.1"
    :tag "emacs>=26.1"
    :url "https://github.com/minad/consult"
    :added "2021-12-09"
    :emacs>= 26.1
    :straight t
    :bind (:evil-normal-state-map
           ("<leader>ee" . consult-flycheck))
    :after consult flycheck)
  :global-minor-mode global-flycheck-mode)

(leaf flycheck-posframe
  :doc "Show flycheck error messages using posframe.el"
  :req "flycheck-0.24" "emacs-26" "posframe-0.7.0"
  :tag "emacs>=26"
  :url "https://github.com/alexmurray/flycheck-posframe"
  :added "2021-12-09"
  :emacs>= 26
  :straight t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-posframe nil nil)
    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
  :global-minor-mode flycheck-posframe-mode
  :after flycheck posframe)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :straight t
  :require t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0.15)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode
  :config
  (leaf company-box
    :doc "Company front-end with icons"
    :req "emacs-26.0.91" "dash-2.19.0" "company-0.9.6" "frame-local-0.0.1"
    :tag "convenience" "front-end" "completion" "company" "emacs>=26.0.91"
    :url "https://github.com/sebastiencs/company-box"
    :added "2021-12-08"
    :emacs>= 26.0
    :straight t
    :require t
    :hook (company-mode-hook)))

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :straight t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20210826" "git-commit-20211004" "magit-section-20211004" "transient-20210920" "with-editor-20211001"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t
  :bind (("C-x g" . magit-status)))

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :url "http://github.com/joaotavora/yasnippet"
  :added "2021-12-08"
  :straight t
  :require t
  :global-minor-mode yas-global-mode
  :config
  (leaf yasnippet-snippets
    :doc "Collection of yasnippet snippets"
    :req "yasnippet-0.8.0"
    :tag "snippets"
    :url "https://github.com/AndreaCrotti/yasnippet-snippets"
    :added "2021-12-09"
    :straight t
    :require t
    :after yasnippet))

(leaf nyan-mode
  :doc "Nyan Cat shows position in current buffer in mode-line."
  :tag "build something amazing" "pop tart cat" "scrolling" "lulz" "cat" "nyan"
  :url "https://github.com/TeMPOraL/nyan-mode/"
  :added "2021-12-08"
  :straight t
  :require t
  :config
  (nyan-mode 1))

(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3"
  :tag "lisp" "convenient" "emacs>=24.3"
  :url "https://github.com/domtronn/all-the-icons.el"
  :added "2021-12-08"
  :emacs>= 24.3
  :straight t
  :require t)

(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "faces" "custom themes" "emacs>=25.1"
  :url "https://github.com/hlissner/emacs-doom-themes"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t
  :custom-face ((doom-modeline-bar quote
                                   ((t
                                     (:background "#6272a4")))))
  :init
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(doom-themes-enable-italic t nil nil "Customized with use-package doom-themes")
                                '(doom-themes-enable-bold t nil nil "Customized with use-package doom-themes")))
  :require t
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf doom-modeline
  :doc "A minimal and modern mode-line"
  :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
  :tag "mode-line" "faces" "emacs>=25.1"
  :url "https://github.com/seagle0128/doom-modeline"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t
  :hook (after-init-hook)
  :config
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(doom-modeline-buffer-file-name-style 'truncate-with-project nil nil "Customized with use-package doom-modeline")
                                '(doom-modeline-icon t nil nil "Customized with use-package doom-modeline")
                                '(doom-modeline-minor-modes nil nil nil "Customized with use-package doom-modeline"))))

(leaf hide-mode-line
  :doc "minor mode that hides/masks your modeline"
  :req "emacs-24.4"
  :tag "mode-line" "frames" "emacs>=24.4"
  :url "https://github.com/hlissner/emacs-hide-mode-line"
  :added "2021-12-08"
  :emacs>= 24.4
  :straight t
  :hook (neotree-mode-hook imenu-list-minor-mode-hook minimap-mode-hook))

(leaf all-the-icons-ivy-rich
  :doc "Better experience with icons for ivy"
  :req "emacs-25.1" "ivy-rich-0.1.0" "all-the-icons-2.2.0"
  :tag "ivy" "icons" "convenience" "emacs>=25.1"
  :url "https://github.com/seagle0128/all-the-icons-ivy-rich"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t
  :config
  (with-eval-after-load 'ivy
    (all-the-icons-ivy-rich-mode 1)
    (require 'all-the-icons-ivy-rich nil nil)))

(leaf ivy-rich
  :doc "More friendly display transformer for ivy"
  :req "emacs-25.1" "ivy-0.13.0"
  :tag "ivy" "convenience" "emacs>=25.1"
  :url "https://github.com/Yevgnen/ivy-rich"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t
  :config
  (with-eval-after-load 'ivy
    (ivy-rich-mode 1)
    (require 'ivy-rich nil nil)))

(leaf beacon
  :doc "Highlight the cursor whenever the window scrolls"
  :req "seq-2.14"
  :tag "convenience"
  :url "https://github.com/Malabarba/beacon"
  :added "2021-12-08"
  :straight t
  :init
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(beacon-color "yellow" nil nil "Customized with use-package beacon")))
  :require t
  :config
  (beacon-mode 1))

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/emacsorphanage/git-gutter"
  :added "2021-12-08"
  :emacs>= 24.4
  :straight t
  :custom-face ((git-gutter:modified quote
                                     ((t
                                       (:background "#f1fa8c"))))
                (git-gutter:added quote
                                  ((t
                                    (:background "#50fa7b"))))
                (git-gutter:deleted quote
                                    ((t
                                      (:background "#ff79c6")))))
  :init
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(git-gutter:modified-sign "~" nil nil "Customized with use-package git-gutter")
                                '(git-gutter:added-sign "+" nil nil "Customized with use-package git-gutter")
                                '(git-gutter:deleted-sign "-" nil nil "Customized with use-package git-gutter")))
  :require t
  :config
  (global-git-gutter-mode 1))

(leaf highlight-indent-guides
  :doc "Minor mode to highlight indentation"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :added "2021-12-08"
  :emacs>= 24.1
  :straight t
  :hook (prog-mode-hook yaml-mode-hook)
  :config
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(highlight-indent-guides-auto-enabled t nil nil "Customized with use-package highlight-indent-guides")
                                '(highlight-indent-guides-responsive t nil nil "Customized with use-package highlight-indent-guides")
                                '(highlight-indent-guides-method 'character nil nil "Customized with use-package highlight-indent-guides")))
  (with-eval-after-load 'highlight-indent-guides
    (if (fboundp 'diminish)
        (diminish 'highlight-indent-guides-mode))))

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :added "2021-12-08"
  :straight t
  :hook (prog-mode-hook))

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/justbur/emacs-which-key"
  :added "2021-12-08"
  :emacs>= 24.4
  :straight t
  :hook (after-init-hook)
  :config
  (with-eval-after-load 'which-key
    (if (fboundp 'diminish)
        (diminish 'which-key-mode))))

(leaf avy
  :doc "Jump to arbitrary positions in visible text and select text quickly."
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "location" "point" "emacs>=24.1"
  :url "https://github.com/abo-abo/avy"
  :added "2021-12-08"
  :emacs>= 24.1
  :straight t)

(leaf ace-window
  :doc "Quickly switch windows."
  :req "avy-0.5.0"
  :tag "location" "window"
  :url "https://github.com/abo-abo/ace-window"
  :added "2021-12-08"
  :straight t
  :bind (("C-'" . ace-window))
  :custom-face ((aw-leading-char-face quote
                                      ((t
                                        (:height 3.0 :foreground "#f1fa8c")))))
  :after avy)

(leaf volatile-highlights
  :doc "Minor mode for visual feedback on some operations."
  :tag "wp" "convenience" "emulations"
  :url "http://www.emacswiki.org/emacs/download/volatile-highlights.el"
  :added "2021-12-08"
  :straight t
  :hook (after-init-hook)
  :custom-face ((vhl/default-face quote
                                  ((nil
                                    (:foreground "#FF3333" :background "#FFCDCD")))))
  :config
  (with-eval-after-load 'volatile-highlights
    (if (fboundp 'diminish)
        (diminish 'volatile-highlights-mode))))

(leaf amx
  :doc "Alternative M-x with extra features."
  :req "emacs-24.4" "s-0"
  :tag "completion" "usability" "convenience" "emacs>=24.4"
  :url "http://github.com/DarwinAwardWinner/amx/"
  :added "2021-12-08"
  :emacs>= 24.4
  :straight t)

(leaf irony
  :doc "C/C++ minor mode powered by libclang"
  :req "cl-lib-0.5" "json-1.2"
  :tag "tools" "convenience" "c"
  :url "https://github.com/Sarcasm/irony-mode"
  :added "2021-12-09"
  :straight t
  :config
  (leaf company-irony
    :doc "company-mode completion back-end for irony-mode"
    :req "emacs-24.1" "company-0.8.0" "irony-1.1.0" "cl-lib-0.5"
    :tag "convenience" "emacs>=24.1"
    :url "https://github.com/Sarcasm/company-irony/"
    :added "2021-12-09"
    :emacs>= 24.1
    :straight t
    :require t
    :config
    (push 'company-irony company-backends)
    :after company irony))

(add-to-list 'exec-path (expand-file-name "/opt/homebrew/Cellar"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "docker" "emacs>=24"
  :url "https://github.com/spotify/dockerfile-mode"
  :added "2021-12-08"
  :emacs>= 24
  :straight
  :mode ("Dockerfile\\'"))

(leaf cmake-mode
  :doc "major-mode for editing CMake sources"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2021-12-08"
  :emacs>= 24.1
  :straight t
  :mode ("CMakeLists\\.txt\\'"))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :url "https://github.com/yoshiki/yaml-mode"
  :added "2021-12-08"
  :emacs>= 24.1
  :straight t
  :bind ((yaml-mode-map
          ("C-m" . newline-and-indent)))
  :mode ("\\.yml\\'"))

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-25.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=25.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t
  :mode
  (("\\.text\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :url "https://github.com/rust-lang/rust-mode"
  :added "2021-12-08"
  :emacs>= 25.1
  :straight t)

(leaf cargo
  :doc "Emacs Minor Mode for Cargo, Rust's Package Manager."
  :req "emacs-24.3" "markdown-mode-2.4"
  :tag "tools" "emacs>=24.3"
  :added "2021-12-08"
  :emacs>= 24.3
  :straight t
  :after markdown-mode)

(leaf cargo
  :doc "Emacs Minor Mode for Cargo, Rust's Package Manager."
  :req "emacs-24.3" "markdown-mode-2.4"
  :tag "tools" "emacs>=24.3"
  :added "2021-12-08"
  :emacs>= 24.3
  :straight t
  :commands cargo-minor-mode
  :hook ((rust-mode-hook . cargo-minor-mode))
  :after markdown-mode)

(leaf spinner
  :doc "Add spinners and progress-bars to the mode-line for ongoing operations"
  :req "emacs-24.3"
  :tag "mode-line" "processes" "emacs>=24.3"
  :url "https://github.com/Malabarba/spinner.el"
  :added "2021-12-08"
  :emacs>= 24.3
  :straight t)

(leaf lv
  :doc "Other echo area"
  :added "2021-12-08"
  :straight t)

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1"
  :tag "convenience" "project" "emacs>=25.1"
  :url "https://github.com/bbatsov/projectile"
  :added "2021-12-09"
  :emacs>= 25.1
  :straight t
  :config
  (require 'bind-key)
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(projectile-switch-project-action 'projectile-dired nil nil "Customized with use-package projectile")))
  (with-eval-after-load 'projectile
    (projectile-mode 1)
    (when (executable-find "ghq")
      (setq projectile-known-projects (mapcar
                                       (lambda (x)
                                         (abbreviate-file-name x))
                                       (split-string
                                        (shell-command-to-string "ghq list --full-path")))))
    (defun projectile-project-find-function (dir)
      (let* ((root (projectile-project-root dir)))
        (and root
             (cons 'transient root))))

    (with-eval-after-load 'project
      (add-to-list 'project-find-functions 'projectile-project-find-function))

    (if (fboundp 'diminish)
        (diminish 'projectile-mode)))

  (bind-key "C-c p"
            #'(lambda nil
                (interactive)
                (use-package-autoload-keymap 'projectile-command-map 'projectile nil))))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2021-12-08"
  :emacs>= 26.1
  :straight t
  :commands lsp
  :hook
  (rust-mode-hook . lsp)
  :custom
  ((lsp-enable-snippet . t)
   (lsp-enable-indentation . nil)
   (lsp-prefer-flymake . nil)
   (lsp-document-sync-method . 'incremental)
   (lsp-inhibit-message . t)
   (lsp-message-project-root-warning . t)
   (create-lockfiles . nil)
   (lsp-file-watch-threshold .nil)
   (lsp-rust-server . 'rust-analyzer))
  :preface (global-unset-key (kbd "C-l"))
  :bind
  (("C-l C-l"  . lsp)
   ("C-l h"    . lsp-describe-session)
   ("C-l t"    . lsp-goto-type-definition)
   ("C-l r"    . lsp-rename)
   ("C-l <f5>" . lsp-restart-workspace)
   ("C-l l"    . lsp-lens-mode))
  :after spinner markdown-mode lv)

(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :added "2021-12-08"
  :emacs>= 26.1
  :straight t
  :commands lsp-ui-mode
  :after lsp-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable . t)
  (lsp-ui-doc-header . t)
  (lsp-ui-doc-include-signature . t)
  (lsp-ui-doc-position . 'bottom)
  (lsp-ui-doc-max-width . 150)
  (lsp-ui-doc-max-height . 30)
  (lsp-ui-doc-use-childframe . t)
  (lsp-ui-doc-use-webkit . t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable . t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable . nil)
  (lsp-ui-sideline-ignore-duplicate . t)
  (lsp-ui-sideline-show-symbol . nil)
  (lsp-ui-sideline-show-hover . nil)
  (lsp-ui-sideline-show-diagnostics . nil)
  (lsp-ui-sideline-show-code-actions . nil)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable . nil)
  (lsp-ui-imenu-kind-position . 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable . t)
  (lsp-ui-peek-always-show . t)
  (lsp-ui-peek-peek-height . 30)
  (lsp-ui-peek-list-width . 30)
  (lsp-ui-peek-fontify . 'always)
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  :bind
  (("C-l s"   . lsp-ui-sideline-mode)
   ("C-l C-d" . lsp-ui-peek-find-definitions)
   ("C-l C-r" . lsp-ui-peek-find-references))
  :after lsp-mode markdown-mode)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
