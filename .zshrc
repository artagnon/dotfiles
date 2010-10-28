# ---[ Environment ]---------------------------------------------------
export PS_PERSONALITY='linux'
[[ $TERM == eterm-color ]] && export TERM=xterm

# ---[ Keychain ]------------------------------------------------------
keychain -q ~/.ssh/id_rsa
source ~/.keychain/fran-sh

# ---[ Autojump ]------------------------------------------------------
function autojump_preexec() {
    { (autojump -a "$(pwd -P)"&)>/dev/null 2>>|${HOME}/.autojump_errors ; } 2>/dev/null
}

typeset -ga preexec_functions
preexec_functions+=autojump_preexec

alias jumpstat="autojump --stat"

function j { local new_path="$(autojump $@)";if [ -n "$new_path" ]; then echo -e "\\033[31m${new_path}\\033[0m"; cd "$new_path";fi }

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
export PATH=$HOME/svn/prefix/svn-trunk/bin:$HOME/bin:$HOME/bin/depot_tools:/var/lib/gems/1.8/bin:$HOME/.python/bin:$HOME/.cabal/bin:$PATH
export PYTHONPATH=$HOME/.python/lib
export PREFIX=$HOME
export GEM_HOME=$PREFIX/lib/ruby/gems/1.8
export RUBYLIB=$PREFIX/lib/ruby:$PREFIX/lib/site_ruby/1.8
export PYTHONSTARTUP=$HOME/.pythonrc
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export ACK_PAGER='less -r'
export LD_LIBRARY_PATH="/usr/local/lib"

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
alias vplayer='mplayer -af volume=-20'
alias entertain='mplayer "$(find "." -type f -name "*.avi" | sort --random-sort | head -n 1)"'
alias sprunge='curl -F "sprunge=<-" http://sprunge.us | xclip'
alias xrandr-restore='xrandr --output CRT1 --auto && xrandr --output CRT2 --auto && xrandr --output CRT2 --left-of CRT1'
alias incognito='export HISTFILE=/dev/null'

# helper functions
function stream-and-save() {
    url=$1;
    rm -f "$2";
    if [[ $url == *youtube* ]]; then
	url=`youtube-dl -g $url`;
    fi
    wget $url -O - | tee -a "$2" | mplayer -cache 8192 -;
}

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
HISTFILE=$HOME/.zsh-history
HISTSIZE=3000
SAVEHIST=$HISTSIZE

# ---[ Completition system ]-------------------------------------------
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*default' list-prompt
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 2
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu interactive
zstyle ':completion:*' verbose yes

# ---[ System settings ]------------------------------------------------
limit -s coredumpsize 0
umask 0027
