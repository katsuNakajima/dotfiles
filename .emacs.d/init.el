;;;;--------------------------------------------------------
;;;; el-get
;;;;--------------------------------------------------------

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(load (concat user-emacs-directory "init-el-get.el"))
(package-initialize)

;;;;--------------------------------------------------------
;;;; 日本語の設定
;;;;--------------------------------------------------------

(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)

;;;;--------------------------------------------------------
;;;; スクロール
;;;;--------------------------------------------------------

;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)

;; スクロール開始のマージンの行数
(setq scroll-margin 10)

;; 1 画面スクロール時に重複させる行数
(setq next-screen-context-lines 10)

;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)

;;;;--------------------------------------------------------
;;;; 検索、置換時の大文字、小文字の区別
;;;;--------------------------------------------------------

;; オプションの "Ignore Case for Search" で設定可
;; バッファー名の検索
(setq read-buffer-completion-ignore-case t)

;; ファイル名の検索
(setq read-file-name-completion-ignore-case t)

;; dabbrev 時の置換
(setq dabbrev-case-replace nil)

;;;;--------------------------------------------------------
;;;; Tips
;;;;--------------------------------------------------------

;; ビープ音禁止
(setq ring-bell-function 'ignore)

;; 選択領域を削除キーで一括削除
(delete-selection-mode t)

;; 行頭 kill-line (C-k) で行全体をカット
(setq kill-whole-line t)

;; 読み取り専用バッファーでもカット系でコピー可能
(setq kill-read-only-ok t)

;; ediff 時にフレームを使わない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; png, jpg などのファイルを画像として表示
(setq auto-image-file-mode t)

;;; バックアップファイルの作成を禁止
(setq backup-inhibited t)

;;; 終了時に自動保存ファイルを削除
(setq delete-auto-save-files t)

;;; バッファ末尾に余計な改行コードを防ぐ
(setq next-line-add-newlines nil)

;;; デフォルトのタブ幅
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

;;; 行番号・列番号を表示
(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 0)

;;; 対応する括弧を強調表示
(setq show-paren-delay 0) ;; 0秒(遅延なし)で表示
(show-paren-mode t)

;;; 選択領域を可視化
(setq-default transient-mark-mode t)

;;; 長い行は折り返して表示
(setq truncate-lines t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;;;;--------------------------------------------------------
;;;;クリップボードにコピー共有
;;;;--------------------------------------------------------

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

;;;;--------------------------------------------------------
;;;; 初期表示位置、サイズ
;;;;--------------------------------------------------------

;; デフォルトのフレームサイズ
  (setq default-frame-alist
		'((width . 80) (height . 40)))

;;;;--------------------------------------------------------
;;;; bind-key
;;;;--------------------------------------------------------

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;;;;--------------------------------------------------------
;::; カスタマイズ
;;;;--------------------------------------------------------

;; カスタマイズ用のファイルを設定
(setq custom-file "~/.emacs.d/custom_setttings.el")

;; カスタマイズ用ファイルをロード
(load custom-file t)

;;;;--------------------------------------------------------
;;;; yasnippet
;;;;--------------------------------------------------------
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
(yas-global-mode 1)
;;;;--------------------------------------------------------
;;;; magit
;;;;--------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)

;;;;--------------------------------------------------------
;;;; jedi
;;;;--------------------------------------------------------
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;----------------------------------------------------------
;; Auto Complete
;;----------------------------------------------------------
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;;;;--------------------------------------------------------
;;;; clang-format
;;;;--------------------------------------------------------
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

(setq clang-format-style-option "llvm")

;;;;--------------------------------------------------------
;;;; helm
;;;;--------------------------------------------------------
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-M-z")   'helm-resume)
(define-key global-map (kbd "C-x C-f")  'helm-find-files)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-g g")   'helm-do-grep)
(define-key global-map (kbd "M-g l")   'helm-occur-from-isearch)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)

(define-key helm-map (kbd "C-h") 'delete-backward-char)

;;;;--------------------------------------------------------
;;;; glsl-mode
;;;;--------------------------------------------------------
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;;;--------------------------------------------------------
;;;; Dockerfile-mode
;;;;--------------------------------------------------------
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
;If you don't, you'll be prompted for an image name each time you build. You may want to add the following to your emacs config:
(put 'dockerfile-image-name 'safe-local-variable #'stringp)

;;;;--------------------------------------------------------
;;;; rust-mode
;;;;--------------------------------------------------------
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save t)
