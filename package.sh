#!/bin/bash
sudo -E apt update && sudo -E apt install ibus-mozc zsh tilix vim tmux xsel xclip tree curl build-essential cmake pkg-config git texinfo python3-ipython python3-venv python3-pip clang-format rapidjson-dev npm markdown pandoc zip peco htop golang-go -y

# Rust and Rust-based-packages
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install cargo-edit cargo-make cargo-atcoder cargo-watch cross cargo-clean-all cargo-cache cargo-update
## only linux
cargo install eza bat fd-find ripgrep hexyl procs
sh -c "$(curl -fsSL https://starship.rs/install.sh)"

# Golang and Go-based-packages
go install github.com/x-motemen/ghq@latest
