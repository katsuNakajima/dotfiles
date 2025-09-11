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

# 共通path設定
export PATH="$PATH:$HOME/.local/bin:$HOME/.npm-global/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"
export PATH="$PATH":"$HOME/fvm/default/bin"
export PATH=$GEM_HOME/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
export PATH="$PATH:$HOME/.local/platform-tools"

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

# eza
if [[ $(command -v eza) ]]; then
    alias e='eza --icons'
    alias l=e
    alias ls=e
    alias ea='eza -a --icons'
    alias la=ea
    alias ee='eza -aal --icons'
    alias ll=ee
    alias et='eza -T -L 3 -a -I "node_modules|.git|.cache" --icons'
    alias lt=et
    alias eta='eza -T -a -I "node_modules|.git|.cache" --color=always --icons | less -r'
    alias lta=eta
fi

# sharchip
export STARSHIP_CONFIG=~/.starship/config.toml
eval "$(starship init zsh)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# The following lines have been added by Docker Desktop to enable Docker CLI completions.
fpath=($HOME/.docker/completions $fpath)
autoload -Uz compinit
compinit
# End of Docker CLI completions
