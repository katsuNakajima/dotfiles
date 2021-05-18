;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)

;; スクロール開始のマージンの行数
(setq scroll-margin 10)

;; 1 画面スクロール時に重複させる行数
(setq next-screen-context-lines 10)

;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)

;; オプションの "Ignore Case for Search" で設定可
;; バッファー名の検索
(setq read-buffer-completion-ignore-case t)

;; ファイル名の検索
(setq read-file-name-completion-ignore-case t)

;; dabbrev 時の置換
(setq dabbrev-case-replace nil)

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

;; バックアップファイルの作成を禁止
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 終了時に自動保存ファイルを削除
(setq delete-auto-save-files t)

;; バッファ末尾に余計な改行コードを防ぐ
(setq next-line-add-newlines nil)

;; デフォルトのタブ幅
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; スペース・タブを可視化
;(global-whitespace-mode 1)

;; 対応する括弧を強調表示
(setq show-paren-delay 0) ;; 0秒(遅延なし)で表示
(show-paren-mode t)

;; 選択領域を可視化
(setq-default transient-mark-mode t)

;; 長い行は折り返して表示
(setq truncate-lines t)

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#bf616a"
                    :height 0.9)

;; 行番号フォーマット
(setq linum-format "%4d")

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバーを非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; titilebar file full name
(setq frame-title-format "%f")

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; Initial frame settings require HackGen35Nerd
(setq default-frame-alist
    (append (list
        '(font . "HackGen35Nerd-16"))
        default-frame-alist))

;; 日本語設定
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
