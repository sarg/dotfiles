My dotfiles.
============

Apply with stow.

    git clone https://github.com/sarg/dotfiles.git .dotfiles
    cd .dotfiles
    git submodule init
    sudo apt-get install stow
    stow apply zsh awesome git
