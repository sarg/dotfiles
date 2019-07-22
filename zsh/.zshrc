source /usr/share/autojump/autojump.zsh
DISABLE_AUTO_UPDATE=true
ZSH=$HOME/.dotfiles/oh-my-zsh
# FIXME: parser error in .zcompdump
source $ZSH/oh-my-zsh.sh

if [ -n "$INSIDE_EMACS" ]; then
else
source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt
fi

unsetopt flow_control
zstyle ':notify:*' activate-terminal no

alias mount='mount | column -t'
alias tb='nc termbin.com 9999'
alias agrep=ack-grep
alias g=git
alias t=task
alias em=emacs-one-frame.sh

function ff() { find ${2:-.} -name "*$1*" ; }

{
  local -a _ssh_hosts _ssh_users

  # Only complete hosts in ~/.ssh/known_hosts, not /etc/hosts.
  # Parse lines where we connected on a non-standard port
  # (eg: [example.com]:2222,[192.168.1.1] ssh-rsa AAAAA)
  if [[ -f $HOME/.ssh/known_hosts ]]; then
    _ssh_hosts="$(<$HOME/.ssh/known_hosts)"
    _ssh_hosts=(${${${${${(f)${_ssh_hosts}}%%\ *}%,*}/]:[0-9]*}/\[})
  else
    _ssh_hosts=()
  fi

  # ssh host completion
  zstyle ':completion:*:(ssh|scp):*:hosts' hosts $_ssh_hosts

  # Disable completion on users with ssh/scp
  _ssh_users=()
  zstyle ':completion:*:(ssh|scp)*:users' users $_ssh_users
}

chpwd() ls

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey "^[Od" emacs-backward-word
bindkey "^[Oc" emacs-forward-word

# Local config
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
