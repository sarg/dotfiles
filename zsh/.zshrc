source ~/.dotfiles/antigen/antigen.zsh

antigen use oh-my-zsh
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme jreese

antigen apply

alias agrep=ack-grep
alias g=git
