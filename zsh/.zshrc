source ~/.dotfiles/antigen/antigen.zsh

antigen use oh-my-zsh
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme jreese

antigen apply

alias agrep=ack-grep
alias g=git

export PATH=~/devel/bin:$PATH

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
