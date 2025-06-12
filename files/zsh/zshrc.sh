# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

fpath=("${GUIX_HOME_PATH}/share/zsh/site-functions" $fpath)

bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "${XDG_CONFIG_HOME}/zsh/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall

eval "$(starship init zsh)"
source <(fzf --zsh)

# This Needs to be done first
source "${GUIX_HOME_PATH}/share/zsh/plugins/fzf-tab/fzf-tab.plugin.zsh"

source "${GUIX_HOME_PATH}/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh"
source "${GUIX_HOME_PATH}/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
source "${GUIX_HOME_PATH}/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
# NOTE: don't use escape sequences (like '%F{red}%d%f') here, fzf-tab will ignore them
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
zstyle ':completion:*' menu no
# preview directory's content with eza when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'lsd -1 --color=always $realpath'
# custom fzf flags
# NOTE: fzf-tab does not follow FZF_DEFAULT_OPTS by default
zstyle ':fzf-tab:*' fzf-flags --color=fg:1,fg+:2 --bind=tab:accept
# To make fzf-tab follow FZF_DEFAULT_OPTS.
# NOTE: This may lead to unexpected behavior since some flags break this plugin. See Aloxaf/fzf-tab#455.
zstyle ':fzf-tab:*' use-fzf-default-opts yes
# switch group using `<` and `>`
zstyle ':fzf-tab:*' switch-group '<' '>'

export BOUNDARY_ADDR="https://boundary.liven.com.au:9200"
alias boundary_login="boundary authenticate oidc -auth-method-id=amoidc_oQJX98VDSU -addr=https://boundary.liven.com.au:9200"

# Replace old-school commands with modern equivalents
alias ls='lsd --group-dirs=first --icon=auto'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'

alias cat='bat --style=plain --paging=never'
alias grep='rg'
alias ps='procs'

# Optional: replacements for file search/view
alias find='fd'
alias less='bat'

ssh_ping() {
  while ! ssh $1 true; do
    sleep 5
  done; echo "Host is back up at $(date)!"
}


ssh_repeat() {
  until ssh $1; do
    sleep 5
  done
}

ssh_network() {
  until ssh home-proxmox -f 'cat /etc/network/interfaces'; do
    sleep 5
  done
}

ghr() {
    wd=$(pwd)
    cd ${HOME}/src/home/
    guix home reconfigure -L . ./home.scm
    cd ${wd}
}

gsr() {
    wd=$(pwd)
    cd ${HOME}/src/home/
    sudo guix pull
    sudo guix system reconfigure -L . ./system.scm
    cd ${wd}
}

gsrq() {
    wd=$(pwd)
    cd ${HOME}/src/home/
    sudo guix system reconfigure -L . ./system.scm
    cd ${wd}
}
