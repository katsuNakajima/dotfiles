#! /bin/zsh
curl -sL https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | zcat > ~/.cargo/bin/rust-analyzer && sudo chmod +x ~/.cargo/bin/rust-analyzer
