# ---[ Environment ]---------------------------------------------------
export PS_PERSONALITY='linux'
[[ $TERM == eterm-color ]] && export TERM=xterm

# ---[ Keychain ]------------------------------------------------------
keychain -q ~/.ssh/id_rsa
source ~/.keychain/fran-sh

# ---[ Autojump ]------------------------------------------------------
source ~/.z.sh
function j () {
    z "$@" || return 0;
}
function z_preexec () {
    z --add "$(pwd -P)";
}

typeset -ga preexec_functions
preexec_functions+=z_preexec

# ---[ Modules ]-------------------------------------------------------
zmodload zsh/complist
autoload -Uz compinit
compinit
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile

# ---[ Modules ]-------------------------------------------------------
. ~/.zshprompt
setprompt

# ---[ cdm function ]--------------------------------------------------
function cdm () {
    local tmp
    if [[ -z "${TMUX}" ]]; then
        echo 'fatal: Not inside tmux.'
        return 1
    fi
    if [[ -n "$1" ]]; then
        [[ "$1" == . ]] && tmp="${PWD}" || tmp="$1"
    else
        tmp="${HOME}"
    fi
    cd "${tmp}"
    tmp="${PWD}"
    tmux "set-option" "default-path" "${tmp}"
    [[ -n "${DISPLAY}" ]] && tmp=on || tmp=off
    tmux "set-option" "set-titles" "${tmp}"
    return 0
}

# ---[ Shell exports ]-------------------------------------------------
export EDITOR="emacsclient"
export PATH=~/svn/prefix/svn-trunk/bin:~/bin:~/bin/depot_tools:/var/lib/gems/1.8/bin:~/.python/bin:~/.cabal/bin:$PATH
export PYTHONPATH=~/.python/lib
export GEM_HOME=~/lib/ruby/gems/1.8
export RUBYLIB=~/lib/ruby:~/lib/site_ruby/1.8
export PYTHONSTARTUP=~/.pythonrc
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export ACK_PAGER='less -r'

# ---[ GPG Key ]-------------------------------------------------------
export GPGKEY=B8BB3FE9

# ---[ Proxy exports ]-------------------------------------------------
export http_proxy=http://localhost:8888/
export ftp_proxy=http://localhost:8888/

# ---[ Debian Developer ]----------------------------------------------
export DEBFULLNAME="Ramkumar Ramachandra"
export DEBEMAIL="artagnon@gmail.com"

# ---[ Simple calculator ]---------------------------------------------
function calc () {
    awk "BEGIN { print $@ }"
}

# ---[ Aliases ]-------------------------------------------------------
# abbreviations
alias ls='ls --color'
alias l='ls --color'
alias halt='sudo halt'
alias reboot='sudo reboot'
alias grep='ack-grep -i'
alias diff='diff -u'
alias less='less -r'
alias ec='emacsclient'
alias et='emacsclient -t'
alias rsync='rsync --progress -avze "ssh -p 51000"'
alias fetch='git fetch'

# apt aliases
alias au='sudo aptitude update'
alias aup='sudo aptitude safe-upgrade'
alias ai='sudo aptitude install'
alias as='aptitude search'
alias ashow='aptitude show'
alias arp='sudo aptitude purge'
alias dl='dpkg -l | grep'
alias dL='dpkg -L'

# tiny helpers
alias rmdup='find . -name "*\ \(1\)*" -exec rm {} \;'
alias entertain='vlc "$(find . -type f -regextype posix-awk -iregex ".*\.(avi|mpg|mpeg|mkv|wmv|dat)$" | sort --random-sort | head -n 1)"'
alias sprunge='curl -F "sprunge=<-" http://sprunge.us | xclip'
alias xrandr-restore='xrandr --output CRT1 --auto; xrandr --output CRT2 --auto; xrandr --output CRT2 --left-of CRT1'
alias incognito='export HISTFILE=/dev/null'

# suffix aliases
alias -s html=x-www-browser
alias -s org=$EDITOR
alias -s c=$EDITOR
alias -s cc=$EDITOR
alias -s py=$EDITOR
alias -s hs=$EDITOR
alias -s pdf=evince
alias -s djvu=evince
alias -s avi=vlc
alias -s mpg=vlc
alias -s mpeg=vlc
alias -s mkv=vlc
alias -s wmv=vlc
alias -s dat=vlc
alias -s mp3=mpg321

# ---[ ZSH Options ]----------------------------------------------------
setopt   NO_GLOBAL_RCS NO_FLOW_CONTROL
setopt   ALWAYS_TO_END BASH_AUTO_LIST NO_BEEP
setopt   AUTO_CD MULTIOS
setopt   CHECK_JOBS NO_HUP
setopt   INC_APPEND_HISTORY EXTENDED_HISTORY HIST_IGNORE_DUPS HIST_FIND_NO_DUPS
setopt	 EXTENDED_HISTORY HIST_EXPIRE_DUPS_FIRST SHARE_HISTORY
setopt   HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS

setopt   notify globdots pushdtohome
setopt   recexact longlistjobs
setopt   autoresume pushdsilent
setopt   autopushd pushdminus rcquotes
unsetopt BG_NICE HUP autoparamslash

# ---[ History ]-------------------------------------------------------
HISTFILE=~/.zsh-history
HISTSIZE=3000
SAVEHIST=$HISTSIZE

# ---[ Completition system ]-------------------------------------------
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z} l:|=* r:|=*' +'r:|[._-]=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*' insert-tab false
zstyle ':completion:*:*:git:*' verbose no
zstyle ':completion:*:files' ignored-patterns '*?.o' '*?~'
zstyle ':completion:*:files' file-sort 'date'
zstyle ':completion:*:default' list-prompt
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 5 )) )'
zstyle ':completion:*:functions' ignored-patterns '_*'

# ---[ System settings ]------------------------------------------------
limit -s coredumpsize 0
umask 0027
