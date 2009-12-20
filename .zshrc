# ---[ Environment ]---------------------------------------------------
export PS_PERSONALITY='linux'
[[ $TERM == eterm-color ]] && export TERM=xterm

# ---[ Keychain ]------------------------------------------------------
keychain -q ~/.ssh/id_rsa
source ~/.keychain/fran-sh

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

# ---[ Shell exports ]-------------------------------------------------
export EDITOR="emacsclient"
#export PS1="%{${fg[red]}%}[%{${fg[green]}%}%T%{${fg[red]}%}]%{${fg[white]}%}%25<...<%~%{${fg[white]}%}: "
#export PS2="%_> "
export PATH=$PATH:/home/artagnon/bin:/home/artagnon/bin/depot_tools
export PYTHONPATH=/opt/python
export PYTHONSTARTUP=/home/artagnon/.pythonrc
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ---[ GPG Key ]-------------------------------------------------------
export GPGKEY=B8BB3FE9

# ---[ Proxy exports ]-------------------------------------------------
#export all_proxy=http://144.16.192.245:8080
#export http_proxy=http://144.16.192.245:8080
#export ftp_proxy=http://144.16.192.245:8080

# ---[ Debian Developer ]----------------------------------------------
export DEBFULLNAME="Ramkumar Ramachandra"
export DEBEMAIL="artagnon@gmail.com"

# ---[ Simple calculator ]---------------------------------------------
function calc () {
    awk "BEGIN { print $@ }"
}

# ---[ Aliases ]-------------------------------------------------------
# system aliases
alias ls='ls --color'
alias l='ls --color'
alias halt='sudo halt'
alias reboot='sudo reboot'
alias grep='ack-grep -i'
alias rmdup='find . -name "*\ \(1\)*" -exec rm {} \;'
alias diff='diff -u'
alias screen='tmux'

# custom aliases
alias rsync='rsync -avz -e ssh'
alias pulseoff='mv ~/.asoundrc ~/.asoundrc.pulse'
alias pulseon='cp ~/.asoundrc.pulse ~/.asoundrc'
alias mutt='http_proxy= ftp_proxy= proxychains mutt'
alias ec='emacsclient'
alias et='emacsclient -t'

# apt aliases
alias au='sudo aptitude update'
alias aup='sudo aptitude safe-upgrade'
alias ai='sudo aptitude install'
alias as='aptitude search'
alias ashow='aptitude show'
alias arp='sudo aptitude purge'
alias dl='dpkg -l | grep'
alias dL='dpkg -L'

# pulseaudio aliases
alias pa='pulseaudio -D'

# bluetooth
alias btallow="dbus-send --system --type=method_call --print-reply --dest=org.bluez /org/bluez/hci0 org.bluez.Adapter.SetMode string:discoverable && dbus-send --system --type=method_call --print-reply --dest=org.bluez /org/bluez/hci0 org.bluez.Adapter.SetDiscoverableTimeout uint32:200 > /dev/null"

# ---[ ZSH Options ]----------------------------------------------------
setopt   ALWAYS_TO_END BASH_AUTO_LIST NO_BEEP
setopt   AUTO_CD MULTIOS
setopt   CHECK_JOBS NO_HUP
setopt   INC_APPEND_HISTORY EXTENDED_HISTORY HIST_IGNORE_DUPS HIST_FIND_NO_DUPS
setopt	 EXTENDED_HISTORY HIST_EXPIRE_DUPS_FIRST SHARE_HISTORY
setopt   HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS

setopt   notify globdots pushdtohome
setopt   recexact longlistjobs
setopt   autoresume pushdsilent
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt BG_NICE HUP autoparamslash

# ---[ History ]-------------------------------------------------------
HISTFILE=/home/artagnon/.zsh-history
HISTSIZE=3000
SAVEHIST=$HISTSIZE

# ---[ Completition system ]-------------------------------------------
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=3 yes
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/artagnon/.zshrc'

# ---[ System settings ]------------------------------------------------
limit -s coredumpsize 0
umask 0027
