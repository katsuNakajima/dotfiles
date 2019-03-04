(use-package rust-mode
              :mode(("\\.rs\\'" . rust-mode))
              :config
              (setq rust-format-on-save t)
;;; racerやrustfmt、コンパイラにパスを通す
              (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;;; rust-modeでrust-format-on-saveをtにすると自動でrustfmtが走る
              (eval-after-load "rust-mode"
                '(setq-default rust-format-on-save t))
              (use-package flycheck-rust
                :config
;;; rustのファイルを編集するときにflycheckを起動する
                  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
                )
              (use-package racer
                :config
;;; rustのファイルを編集するときにracerを起動する
                (add-hook 'rust-mode-hook (lambda ()
                                            (racer-mode)))
                ;; racerのeldocサポートを使う
(add-hook 'racer-mode-hook #'eldoc-mode)
;;; racerの補完サポートを使う
(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)
                             ;;; この辺の設定はお好みで
                             (set (make-variable-buffer-local 'company-idle-delay) 0.1)
                             (set (make-variable-buffer-local 'company-minimum-prefix-length) 0)))))
