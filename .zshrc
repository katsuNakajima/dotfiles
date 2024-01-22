# loading OS settings
[ -f $HOME/.zshrc.`uname` ] && source $HOME/dotfiles/.zshrc.`uname`

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# gem Home
export GEM_HOME=$HOME/.gem

#GOPATH
export GOPATH=$HOME/.go
export GO111MODULE=on

#nvm
export NVM_DIR="$HOME/.nvm"
  [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# 共通path設定
export PATH="$PATH:$HOME/.local/bin:$HOME/.npm-global/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"
export PATH="$PATH":"$HOME/fvm/default/bin"
export PATH="${PYENV_ROOT}/bin:$PATH"
export PATH=$GEM_HOME/bin:$PATH
export PATH=$GOPATH/bin:$PATH
export PATH="$PATH:$HOME/.local/platform-tools"

# Python setup
source "$HOME/.rye/env"

# for WSL permission
umask 022

# cdr with peco
function peco-cdr () {
    local selected_dir="$(cdr -l | sed 's/^[0-9]\+ \+//' | peco --prompt="cdr >" --query "$LBUFFER")"
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
}
zle -N peco-cdr
bindkey '^W' peco-cdr

# cdr
if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':completion:*' recent-dirs-insert both
    zstyle ':chpwd:*' recent-dirs-default true
    zstyle ':chpwd:*' recent-dirs-max 1000
    zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/chpwd-recent-dirs"
fi

# git repo control ghq and peco
alias cr='cd $(ghq root)/$(ghq list | peco)'
alias crh='hub browse $(ghq list | peco | cut -d "/" -f 2,3)'

# histry with peco
function peco-history-selection() {
    BUFFER=`history -n 1 | tac | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection

# exa
if [[ $(command -v exa) ]]; then
    alias e='exa --icons'
    alias l=e
    alias ls=e
    alias ea='exa -a --icons'
    alias la=ea
    alias ee='exa -aal --icons'
    alias ll=ee
    alias et='exa -T -L 3 -a -I "node_modules|.git|.cache" --icons'
    alias lt=et
    alias eta='exa -T -a -I "node_modules|.git|.cache" --color=always --icons | less -r'
    alias lta=eta
fi

# sharchip
export STARSHIP_CONFIG=~/.starship/config.toml
eval "$(starship init zsh)"

# Volta
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
