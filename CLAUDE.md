# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## リポジトリ概要

macOS / Debian系Linux（Debian, Ubuntu, WSL含む）向けの個人開発環境設定を管理するdotfilesリポジトリ。
シンボリックリンクによる設定ファイルの配置、Homebrew/aptによるパッケージ管理、Emacs（straight.el + leaf.el構成）の設定を含む。

## よく使うコマンド

### 初期セットアップ
```bash
./setup.sh
```
`setup.sh`内の`DOT_FILES`配列（`.clang-format`, `.tmux.conf`, `.vimrc`, `.zshrc`, `.zshrc.Darwin`, `.zshrc.Linux`, `.emacs.d`, `.starship`）を`$HOME`直下にシンボリックリンクする。新しい設定ファイルをリポジトリに追加した場合は、この配列にも追記が必要。

### パッケージインストール
- **macOS**: `brew bundle`（`Brewfile`を参照。tap/formulaeを追加・削除したら`Brewfile`を直接編集する）
- **Debian/Ubuntu**: `./package.sh`（apt, Rust/cargo, Golangのパッケージをまとめてインストール）

### Git設定
- `./github_config.sh` — GitHub用のグローバルgit設定（user名・email・ghq.root・デフォルトブランチ）を適用
- `./gitlab_config.sh` — GitLab用のローカルgit設定（user名・email、リポジトリごとに実行する想定）

## アーキテクチャ / 構成の要点

### シェル設定の階層構造
`.zshrc`が共通設定で、末尾でOS別ファイルを読み込む分岐構造になっている:
```
.zshrc → source .zshrc.`uname` (Darwinなら.zshrc.Darwin、Linuxなら.zshrc.Linux)
```
- `.zshrc`: 補完・履歴・peco連携（ghqと組み合わせたリポジトリ移動 `cr`/`crh`、履歴検索）、eza（ls代替）のエイリアス、starship初期化など、OS非依存の設定
- `.zshrc.Darwin`: Homebrew関連（GNU utils優先PATH、nodenv、PostgreSQL、zsh-completions/zsh-syntax-highlighting）
- `.zshrc.Linux`: WSL向けumask設定など最小限

OS判定は`uname`コマンドの結果に依存しているため、新しいOS固有設定は`.zshrc.<uname出力>`という命名規則に従うこと。

### Emacs設定（`.emacs.d/`）
- パッケージ管理は`straight.el`（`init.el`冒頭でbootstrap）+ `leaf.el`（宣言的な設定マクロ）を使用。`use-package`ではなく`leaf`ブロックで各パッケージを設定する構成。
- `.emacs.d/straight/repos/`配下はstraight.elが管理するサードパーティパッケージのクローン（`.gitignore`で`straight/versions`以外は除外）。編集対象ではない。
- `.emacs.d/Makefile.toml`（cargo-make用）に、Claude Code用のSerena MCPサーバーを起動するタスク`run-serena`が定義されている（`cargo make run-serena`で起動、ポートは`SERENA_PORT`環境変数で変更可）。

### Rust関連の補助スクリプト
- `rust-xbuild-env.sh`: `x86_64-unknown-linux-musl`向けクロスコンパイル用の環境変数（CC/CXX/AR/リンカ）
- `sccache-rust.sh`: `RUSTC_WRAPPER=sccache`を設定してビルドキャッシュを有効化
- `cargo-atcoder-mac.toml` / `cargo-atcoder-linux.toml`: OS別の`cargo-atcoder`（競技プログラミング用ツール）設定

## コミットメッセージ規約

Conventional Commits風だが、このリポジトリ独自の接頭辞を使用している:
- `add:` — 新規ファイル/パッケージ/設定の追加
- `mod:` — 既存設定の変更・更新
- （日本語の説明文をそのまま使う場合もある）
