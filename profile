export GREP_OPTIONS="--binary-files without-match"
export LANG=ru_RU.UTF8
export LC_NUMERIC=C
export EDITOR=vim
export VISUAL=vim
export BROWSER=chromium
export PATH=~/devel/bin:$PATH

# abbreviations
alias g=git
alias vim=gvim --remote-tab-silent
alias v=vim
alias pd=perldoc
alias du='du -h'
alias df='df -hT'
alias ls='ls -aF --color=always'
alias top=htop

# tools
alias yaup='yaourt -Syu --aur --noconfirm'
alias rtorrent="ssh -t sarg.org.ru -l rtorrent screen -U -D -RR"
alias home="sudo mount.cifs //192.168.0.1/torrents smb -o iocharset=utf8,uid=$USER,nounix,guest,file_mode=0600,dir_mode=0700"
alias mon="xrandr --output VGA1 --auto --rotate left --above LVDS1 --output LVDS1 --primary --auto"

function tget() { scp $* rtorrent@sarg.org.ru:auto && rm $* }
function publish() { scp $* sarg@sarg.org.ru:www/ && for f in $*; do echo 'http://sarg.org.ru/~sarg/'`basename $f`; done }
function extract () {
    if [[ -f "$1" ]]; then
        case "$1" in
            *.tbz2 | *.tar.bz2) tar -xvjf  "$1"     ;;
            *.txz | *.tar.xz)   tar -xvJf  "$1"     ;;
            *.tgz | *.tar.gz)   tar -xvzf  "$1"     ;;
            *.tar | *.cbt)      tar -xvf   "$1"     ;;
            *.zip | *.cbz)      unzip      "$1"     ;;
            *.rar | *.cbr)      unrar x    "$1"     ;;
            *.arj)              unarj x    "$1"     ;;
            *.ace)              unace x    "$1"     ;;
            *.bz2)              bunzip2    "$1"     ;;
            *.xz)               unxz       "$1"     ;;
            *.gz)               gunzip     "$1"     ;;
            *.7z)               7z x       "$1"     ;;
            *.Z)                uncompress "$1"     ;;
            *.gpg)        gpg -d "$1" | tar -xvzf - ;;
            *) echo "Error: failed to extract '$1'" ;;
        esac
    else
        echo "Error: '$1' is not a valid file for extraction"
    fi
}
