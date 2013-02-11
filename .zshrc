# ---[ Environment ]---------------------------------------------------
export PS_PERSONALITY='linux'
[[ $TERM == eterm-color ]] && export TERM=xterm

# ---[ Keychain ]------------------------------------------------------
keychain --nogui -q ~/.ssh/id_rsa
source ~/.keychain/localhost-sh

# ---[ Autojump ]------------------------------------------------------
source ~/dotfiles/z/z.sh
function j () {
	z "$@" || return 0;
}
function _z_preexec () {
	z --add "$(pwd -P)";
}

preexec_functions=(_z_preexec $preexec_functions)

# ---[ Modules ]-------------------------------------------------------
zmodload zsh/complist
autoload -Uz compinit
autoload -Uz vcs_info
compinit
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile
autoload colors zsh/terminfo

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
	echo .
	return 0
}

# ---[ Autols ]--------------------------------------------------------
function chpwd() {
	case `pwd` in
		*'/git'*|'/tmp') ;;
		*) ls --color -v ;;
	esac
}

# ---[ Shell exports ]-------------------------------------------------
export EDITOR="emacsclient"
export PATH=~/bin:~/bin/depot_tools:~/.python/bin:~/.cabal/bin:~/.rbenv/bin:~/.ruby/bin:~/.local/bin:~/.android/sdk/tools:~/.android/sdk/platform-tools:/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin
export PYTHONPATH=~/.local/lib
export GEM_HOME=~/.ruby
export PYTHONSTARTUP=~/.pythonrc
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export LESS="FSRX"

# ---[ rbenv ]---------------------------------------------------------
eval "$(rbenv init -)"

# ---[ hub ]---------------------------------------------------------
eval "$(hub alias -s)"

# ---[ Android SDK/ NDK ]----------------------------------------------
export JAVA_HOME=/usr/lib/jvm/java-6-openjdk-amd64
export ANDROIDSDK=~/.android/sdk
export ANDROIDNDK=~/.android/ndk
export ANDROIDNDKVER=r8c
export ANDROIDAPI=14

# ---[ GPG Key ]-------------------------------------------------------
export GPGKEY=B8BB3FE9

# ---[ Debian Developer ]----------------------------------------------
export DEBFULLNAME="Ramkumar Ramachandra"
export DEBEMAIL="artagnon@gmail.com"

# ---[ Simple calculator ]---------------------------------------------
function calc () {
	awk "BEGIN { print $@ }"
}

# ---[ Aliases ]-------------------------------------------------------
# abbreviations
alias resh='source ~/.zshrc'
alias ll='ls -lha'
alias halt='sudo halt'
alias reboot='sudo reboot'
alias hibernate='sudo pm-hibernate'
alias grep='ack-grep -i'
alias diff='diff -u'
alias ec='emacsclient'
alias ecr='emacsclient -n -c'
alias et='emacsclient -t'
alias fetch='git fetch'
alias time='/usr/bin/time -p'
alias easy_install='pip install --user'
alias jnettop='sudo jnettop'

# apt aliases
alias au='sudo aptitude update'
alias aup='sudo aptitude safe-upgrade'
alias ai='sudo aptitude install'
alias as='aptitude search'
alias ashow='aptitude show'
alias arp='sudo aptitude purge'
alias are='sudo aptitude reinstall'
alias dl='dpkg -l | grep'
alias dL='dpkg -L'

# pacman aliases
alias pS='sudo pacman -S'
alias pSs='pacman -Ss'
alias pSu='sudo pacman -Su'
alias pR='sudo pacman -Rs'
alias pQ='pacman -Q'
alias pQl='pacman -Ql'
alias pQi='pacman -Qi'
alias pQo='pacamn -Qo'
alias pQs='pacman -Qs'

# gem and pip aliases
alias gi='gem install'
alias pi='pip install --user'

# tiny helpers
function l () {
	case "$1" in
		date|mtime)
			shift
			ls --color -vt "$@"
			;;
		atime)
			shift
			ls --color -vu "$@"
			;;
		recent)
			shift
			ls --color -vt "$@" | head -n 5
			;;
		size)
			shift
			ls --color -vS "$@"
			;;
		all)
			shift
			ls --color -vlha "$@"
			;;
		extension)
			shift
			ls --color -vX "$@"
			;;
		*)
			ls --color -v "$@"
			;;
	esac
}

function x () {
	case "$1" in
		*.tar*)
			tar xf "$1"
			;;
		*.zip)
			unzip "$1"
			;;
	esac
}

function man_ () {
	emacsclient -e "(man \"$*\")" 2>&1 >/dev/null || man "$*"
}

alias man=man_
alias rmdup='find . -name "*\ \(1\)*" -exec rm {} \;'
alias entertain='vlc "$(find . -type f -regextype posix-awk -iregex ".*\.(avi|mpg|mpeg|mkv|wmv|dat)$" | sort --random-sort | head -n 1)"'
alias sprunge='curl -F "sprunge=<-" http://sprunge.us'
alias xrandr-restore='xrandr --output CRT1 --auto; xrandr --output CRT2 --auto; xrandr --output CRT2 --left-of CRT1'
alias incognito='export HISTFILE=/dev/null'
alias git-prove='make -j 8 DEFAULT_TEST_TARGET=prove GIT_PROVE_OPTS="-j 15" test'

# suffix aliases
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
setopt   NO_GLOBAL_RCS NO_FLOW_CONTROL NO_BEEP MULTIOS
setopt   AUTO_LIST NO_LIST_AMBIGUOUS MENU_COMPLETE AUTO_REMOVE_SLASH
setopt   LIST_PACKED LIST_TYPES
setopt   INC_APPEND_HISTORY EXTENDED_HISTORY SHARE_HISTORY HIST_REDUCE_BLANKS
setopt   HIST_SAVE_NO_DUPS HIST_IGNORE_DUPS HIST_FIND_NO_DUPS HIST_EXPIRE_DUPS_FIRST
setopt   NO_NOTIFY LONG_LIST_JOBS
setopt   AUTO_CD AUTO_PUSHD PUSHD_SILENT
setopt   PROMPT_SUBST

# ---[ History ]-------------------------------------------------------
HISTFILE=~/.zsh-history
HISTSIZE=3000
SAVEHIST=$HISTSIZE

# ---[ Completition system ]-------------------------------------------
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list '+' '+m:{|:lower:|}={|:upper:|}' '+l:|=* r:|=*' '+r:|[._-]=** r:|=**'
zstyle ':completion:*' list-colors no=00 fi=00 di=01\;34 pi=33 so=01\;35 bd=00\;35 cd=00\;34 or=00\;41 mi=00\;45 ex=01\;32
zstyle ':completion:*' verbose yes
zstyle ':completion:*' insert-tab false
zstyle ':completion:*:*:git:*' verbose no
zstyle ':completion:*:files' ignored-patterns '*?.o' '*?~'
zstyle ':completion:*:files' file-sort 'date'
zstyle ':completion:*:default' list-prompt
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 5 )) )'
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats '|%F{green}%b%c%u%f'

# ---[ ZLE ]------------------------------------------------------------
history-incremental-search-backward-initial() {
	zle history-incremental-search-backward $BUFFER
}
zle -N history-incremental-search-backward-initial
bindkey '^R' history-incremental-search-backward-initial
bindkey -M isearch '^R' history-incremental-search-backward

# ---[ Prompt ]--------------------------------------------------------
function precmd() { vcs_info }
[[ "$terminfo[colors]" -ge 8 ]] && colors
if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="white"; fi

PROMPT='%F{$NCOLOR}%B%n%b%f\
${vcs_info_msg_0_}:\
%F{yellow}%B%~%b%f%(!.#.$) '

# ---[ System settings ]------------------------------------------------
limit -s coredumpsize 0
umask 0027
