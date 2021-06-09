# loading mac settings
[ -f $HOME/.zshrc.`uname` ] && source $HOME/dotfiles/.zshrc.`uname`

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# pyenv root
export PYENV_ROOT="${HOME}/.pyenv"

# 共通path設定
export PATH="$PATH:$HOME/.local/bin:$HOME/.npm-global/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/development/flutter/bin:$PATH"
export PATH="${PYENV_ROOT}/bin:$PATH"

# python setup
eval "$(pyenv init --path)"
eval "$(pyenv virtualenv-init -)"
export PIPENV_VENV_IN_PROJECT=true

# for WSL permission
umask 022

# git repo control ghq and peco
alias cr='cd $(ghq root)/$(ghq list | peco)'
alias crh='hub browse $(ghq list | peco | cut -d "/" -f 2,3)'

# sharchip
eval "$(starship init zsh)"
