# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "${XDG_CONFIG_HOME}/zsh/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall

source "${GUIX_HOME_PATH}/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "${GUIX_HOME_PATH}/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
source "${GUIX_HOME_PATH}/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh"
source "${GUIX_HOME_PATH}/share/zsh/site-functions/_skim-bindings"

eval "$(starship init zsh)"

alias ls="ls --color=always"

export BOUNDARY_ADDR="https://boundary.liven.com.au:9200"
alias boundary_login="boundary authenticate oidc -auth-method-id=amoidc_oQJX98VDSU -addr=https://boundary.liven.com.au:9200"

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


