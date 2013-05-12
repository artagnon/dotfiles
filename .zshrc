# ---[ Environment ]---------------------------------------------------
export PS_PERSONALITY='linux'
[[ $TERM == eterm-color ]] && export TERM=xterm

# ---[ Augment fpath ]-------------------------------------------------
fpath=(~/.zsh/completion $fpath)

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

# ---[ Autols ]--------------------------------------------------------
function chpwd() {
	case `pwd` in
		*'/git/t/'*) ls --color -v ;;
		*'/git'*|'/tmp') ;;
		*) ls --color -v ;;
	esac
}

# ---[ Shell exports ]-------------------------------------------------
export LANG=en_US.utf8
export EDITOR="emacsclient"
export PATH=~/bin:~/bin/depot_tools:~/.python/bin:~/.cabal/bin:~/.rbenv/bin:~/.ruby/bin:~/.local/bin:~/.android/sdk/tools:~/.android/sdk/platform-tools:/bin:/usr/local/bin:/usr/bin:/usr/bin/core_perl:/usr/bin/vendor_perl:/usr/sbin:/sbin
export LD_LIBRARY_PATH=/usr/local/lib
export PYTHONPATH=~/.local/lib
export GEM_HOME=~/.ruby
export PYTHONSTARTUP=~/.pythonrc
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export LESS="FSRX"

# ---[ rbenv ]---------------------------------------------------------
eval "$(rbenv init -)"

# ---[ Perl ]----------------------------------------------------------
export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:$HOME/.perl5"
export PERL_MB_OPT="--install_base $HOME/.perl5"
export PERL_MM_OPT="INSTALL_BASE=$HOME/.perl5"
export PERL5LIB="$HOME/.perl5/lib/perl5:$PERL5LIB"
export PATH="$HOME/.perl5/bin:$PATH"

# ---[ Android SDK/ NDK ]----------------------------------------------
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk
export ANDROIDSDK=~/.android/sdk
export ANDROIDNDK=~/.android/ndk
export ANDROIDNDKVER=r8c
export ANDROIDAPI=14

# ---[ GPG Key ]-------------------------------------------------------
export GPGKEY=B8BB3FE9

# ---[ Simple calculator ]---------------------------------------------
function calc () {
	awk "BEGIN { print $@ }"
}

# ---[ Aliases ]-------------------------------------------------------
# abbreviations
alias resh='source ~/.zshrc'
alias ll='ls --color -vlha'
alias halt='sudo halt'
alias reboot='sudo reboot'
alias hibernate='sudo pm-hibernate'
alias diff='diff -u'
alias ec='emacsclient'
alias ecr='emacsclient -n -c'
alias et='emacsclient -t'
alias fetch='git fetch'
alias easy_install='pip install --user'
alias jnettop='sudo jnettop'
alias mount='sudo mount -o uid=artagnon,gid=artagnon,fmask=113,dmask=002'
alias umount='sudo umount'
alias g='git'

# pacman aliases
alias pS='sudo pacman -S'
alias pSs='pacman -Ss'
alias pSy='sudo pacman -Sy'
alias pSu='sudo pacman -Syu'
alias pU='sudo pacman -U'
alias pR='sudo pacman -Rs'
alias pQ='pacman -Q'
alias pQl='pacman -Ql'
alias pQi='pacman -Qi'
alias pQo='pacman -Qo'
alias pQs='pacman -Qs'

# gem and pip aliases
alias gi='gem install'
alias pi='pip install --user'

# tiny helpers
function l () {
	if test "true" = $(g rp --is-inside-work-tree); then
		g s
		return
	fi
	case "$1" in
		recent)
			shift
			ls --color -vt "$@" | head -n 5
			;;
		size)
			shift
			ls --color -vS "$@"
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
alias incognito='export HISTFILE=/dev/null'
alias git-make='make -j 8'
alias git-prove='make -j 8 test'

# reload the git completer from fpath
function regitsh () {
	unfunction -m _git\*
	autoload -Uz $^fpath/_git*(N:t)
}

function - () {
	if test "true" = $(g rp --is-inside-work-tree); then
		g co -
	else
		cd - >/dev/null
	fi
}

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

# ---[ ZSH Options ]---------------------------------------------------
setopt   NO_GLOBAL_RCS NO_FLOW_CONTROL NO_BEEP MULTIOS
setopt   NO_NOMATCH
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

# ---[ ZLE ]-----------------------------------------------------------
history-incremental-search-backward-initial() {
	zle history-incremental-search-backward $BUFFER
}
zle -N history-incremental-search-backward-initial
bindkey '^R' history-incremental-search-backward-initial
bindkey -M isearch '^R' history-incremental-search-backward

# ---[ Prompt ]--------------------------------------------------------
source ~/.zsh/prompt/git-prompt.zsh

GIT_PS1_DESCRIBE_STYLE=branch
GIT_PS1_SHOWUPSTREAM=auto
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWCOLORHINTS=true

precmd () { __git_ps1 "%F{$NCOLOR}%B%n%b%f" ":%F{yellow}%B%~%b%f%(!.#.$) " "|%s" }

# ---[ System settings ]-----------------------------------------------
limit -s coredumpsize 0
umask 002
