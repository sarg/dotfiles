bindkey -e
HISTSIZE=2000
SAVEHIST=$HISTSIZE
HISTFILE=~/.history


# key bindings
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\eOc" emacs-forward-word
bindkey "\e[5D" backward-word
bindkey "\eOd" emacs-backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "^H" backward-delete-word
# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
# for non RH/Debian xterm, can't hurt for RH/DEbian xterm
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
# for freebsd console
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
# completion in the middle of a line
bindkey '^i' expand-or-complete-prefix


eval `dircolors -b`

source ~/.profile

autoload -U compinit promptinit
compinit
promptinit

prompt walters

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' menu select
zstyle ':completion:*' users off

if [[ -e ~/.ssh/known_hosts ]]; then
    # Use .ssh/known_hosts for hostnames.
    hosts=(${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*})
    zstyle ':completion:*:hosts' hosts $hosts 
fi


setopt AUTO_LIST
setopt AUTO_PUSHD
setopt HIST_IGNORE_ALL_DUPS
setopt INC_APPEND_HISTORY
setopt NO_GLOBAL_RCS

setopt AUTOCD
setopt EXTENDEDGLOB
