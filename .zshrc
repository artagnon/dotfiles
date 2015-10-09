# ---[ Prezto ]--------------------------------------------------------
if [[ -s ~/.zprezto/init.zsh ]]; then
	source ~/.zprezto/init.zsh
fi

# ---[ Keychain ]------------------------------------------------------
keychain --nogui -q ~/.ssh/id_rsa 2>/dev/null &&
source ~/.keychain/localhost-sh

# ---[ Modules ]-------------------------------------------------------
zmodload zsh/complist
autoload -Uz compinit
compinit
zmodload -a zsh/stat stat
zmodload -ap zsh/mapfile mapfile
autoload colors zsh/terminfo

export LSCOLORS=cxfxcxdxbxegedabagacad
export LS_COLORS="di=32;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:"
case `uname` in
Darwin)
	LSCOLORSW=-G;;
Linux)
	LSCOLORSW=--color;;
esac

# ---[ Autols ]--------------------------------------------------------
function chpwd() {
case `pwd` in
"$HOME/src/git"|"$HOME/src/git/"*|"/tmp") ;;
*) ls $LSCOLORSW -v ;;
esac
}

# ---[ Save canceled command ]-----------------------------------------
TRAPINT () {
zle && [[ $HISTNO -eq $HISTCMD ]] && print -rs -- $BUFFER
return $1
}

# ---[ MathWorks ]-----------------------------------------------------
if [[ $USER == rramacha ]]; then
	. /mathworks/hub/share/sbtools/bash_setup.bash
fi

# ---[ Shell exports ]-------------------------------------------------
export PATH=~/bin:~/.ruby/bin:~/.rbenv/bin:~/.cask/bin:$PATH
export PATH=/hub/share/sbtools/apps/cgir_tools:/hub/share/sbtools/bin/$SBARCH:$PATH
EMACSCLIENT=emacsclient

if [[ $USER == rramacha ]]; then
    PATH=~/bin/mw:~/bin/bear:$PATH
    EMACSCLIENT=sbemacsclient
fi
export EDITOR="atom --wait"
export VISUAL=$EDITOR
export GIT_EDITOR=$EDITOR
export ATOM_REPOS_HOME=~/src
export BROWSER=google-chrome
export LD_LIBRARY_PATH=/usr/local/lib:~/src/linux/tools/perf
export PYTHONPATH=~/.local/lib:/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Resources/Python
export GEM_HOME=~/.ruby
export PYTHONSTARTUP=~/.pythonrc
export WORDCHARS='*?[]~&;!#$%^(){}<>'
export MENUCONFIG_COLOR=mono
export PS_PERSONALITY=linux
export LLDB_DEBUGSERVER_PATH=~/bin/debugserver
export PPPATH=/tmp/ppdump

# ---[ set MANPATH ]---------------------------------------------------
export MANPATH=~/share/man:/opt/local/share/manpath:$MANPATH

# ---[ evals ]---------------------------------------------------------
eval "$(rbenv init -)"
test -f ~/.opam/opam-init/init.zsh && source ~/.opam/opam-init/init.zsh

# ---[ Perl ]----------------------------------------------------------
export PERL_LOCAL_LIB_ROOT=$PERL_LOCAL_LIB_ROOT:~/.perl5
export PERL_MB_OPT="--install_base $HOME/.perl5"
export PERL_MM_OPT="INSTALL_BASE=$HOME/.perl5"
export PERL5LIB=~/.perl5/lib/perl5:$PERL5LIB
export PATH=~/.perl5/bin:$PATH
export PERLBREW_ROOT=~/.perl5
test -f ~/.perl5/etc/bashrc && source ~/.perl5/etc/bashrc

# ---[ Aliases ]-------------------------------------------------------
# abbreviations
alias resh='source ~/.zshrc'
alias ll='ls $LSCOLORSW -vlha'
alias diff='diff -u'
alias ec='$EMACSCLIENT -n'
alias jnettop='sudo jnettop'
alias mountfat="sudo mount -o uid=$USER,gid=$USER,fmask=113,dmask=002"
alias mountl="sudo mount -o loop"
alias umount='sudo umount'
alias rf='rm -rf'
alias chrome='google-chrome'
alias p4='p4g'
alias sb='sb -softwareopengl'
alias sbnd='sb -softwareopengl -nodesktop'
alias sbndd='sb -softwareopengl -nodesktop -debug'

# aptitude aliases
alias pS='sudo aptitude install'
alias pSs='aptitude search'
alias pSu='sudo aptitude upgrade'
alias pU='sudo aptitude update'
alias pR='sudo aptitude purge'
alias dL='dpkg -L'

# cower aliases
alias cS='cower -s'
alias cD='cower -d'
alias cU='cower -u'

# gem and pip aliases
alias gi='gem install'
alias pi='pip install --user'
alias bi='brew install'

# remove bad aliases set by prezto
unalias -m l
unalias -m rm
unalias -m mv

# tiny helpers
function l () {
	case "$1" in
	recent)
		shift
		ls $LSCOLORSW -vt "$@" | head -n 5
		;;
	size)
		shift
		ls $LSCOLORSW -vS "$@"
		;;
	esac
	ls $LSCOLORSW -v "$@"
}

function g () {
    if test $# = 0; then
	git status
    else
	git "$@"
    fi
}

function calc () {
	awk "BEGIN { print $@ }"
}

alias rmdup='find . -name "*\ \(1\)*" -exec rm {} \;'
alias entertain='mpv "$(find . -type f -regextype posix-awk -iregex ".*\.(avi|mpg|mpeg|mkv|wmv|dat)$" | sort --random-sort | head -n 1)"'
alias incognito='export HISTFILE=/dev/null'
alias cdtop='cd $(g rp --show-toplevel)'
alias fgrep='find . -print0 | grep -FZz'
alias ag='ag --pager "less -R"'
alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g S='| sort'
alias syncmaster='mw -using Bcgir_core sbsyncmaster -C /local-ssd/rramacha -log-dir /tmp/bcgir_core -src-root Bcgir_core -cfg cgir_syncmaster_debug'
alias sbs='mw -using Bmain sbs'
alias b='sbmake -j16'
alias bd='DEBUG=1 sbmake -j16'
alias bv='VERBOSE=1 sbmake -j16'
alias bdv='VERBOSE=1 DEBUG=1 sbmake -j16'
alias t=cgtddd
alias p4v=sbp4v
alias review=sbreviewboard

function sbtest() {
    sb -nodesktop -r "rerun $1 $2"
}

function sbtestd() {
    sb -nodesktop -debug -r "rerun $1 $2"
}

function fgr () {
    find . -name "*$1*"
}

# usage: git-make       ;for x86 linux.git build or make -j 8
#    or: git-make prove ;for git.git tests
#    or: git-make um    ;for um linux.git build
#    or: git-make um32  ;for um32 linux.git build
#    or: git-make arm   ;for arm64 linux.git build
function git-make () {
	unset ARCH
	unset SUBARCH
	unset CROSS_COMPILE

	test "true" = "$(g rp --is-inside-work-tree 2>/dev/null)" || return 1
	case "$1" in
	    prove)
		make -j 8 test
		;;
	    um)
		make mrproper
		make defconfig ARCH=um
		make -j 8 ARCH=um
		;;
	    um32)
		make mrproper
		make defconfig ARCH=um SUBARCH=i386
		make -j 8 ARCH=um SUBARCH=i386
		;;
	    arm32-native)
		make mrproper
		make defconfig ARCH=arm32
		make -j 2 ARCH=arm32
		;;
	    arm)
		make mrproper
		make defconfig ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu-
		make -j 8 ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu-
		;;
	    android-em)
		if test "$2" = "-i"; then
		    make -j 8 ARCH=arm CROSS_COMPILE=arm-none-linux-gnueabi-;
		else
		    make mrproper
		    make goldfish_armv7_defconfig ARCH=arm
		    make -j 8 ARCH=arm CROSS_COMPILE=arm-none-linux-gnueabi-
		fi
		;;
	    android-flo)
		if test "$2" = "-i"; then
		    make -j 8 ARCH=arm CROSS_COMPILE=arm-eabi-;
		else
		    make mrproper
		    make flo_defconfig ARCH=arm
		    make -j 8 ARCH=arm CROSS_COMPILE=arm-eabi-;
		fi
		;;
	    sparc32)
		make mrproper
		make defconfig ARCH=sparc CROSS_COMPILE=sparc-leon3-linux-
		make -j 8 ARCH=sparc CROSS_COMPILE=sparc-leon3-linux-
		;;
	    *)
		if test -f Kconfig; then
			make mrproper
			make defconfig ARCH=x86
			make -j 8 ARCH=x86
		else
			make -j 8
		fi
	esac
}

# usage: git-prove-all [<branch>]
function git-prove-all () {
	GIT_SEQUENCE_EDITOR="sed -i '/^pick/aexec make -j 8 test'" git ri ${1-master}
}

# usage: gsh <file>
#    or: gsh <name-to-match>
function gsh () {
	if test $# != 1; then
		return 1
	fi
	if test -f "$1"; then
		sh "$1" -v -i
		return
	fi
	find . -maxdepth 1 -type f -name "*$1*" -exec echo "== {}" \; -exec sh {} \;
}

# usage: reload-completer (git|rustc)
function reload-completer () {
	test $# != 1 && return 1
	unfunction -m _$1\*
	autoload -Uz $^fpath/_$1*(N:t)
}

function - () {
	if test "true" = "$(g rp --is-inside-work-tree 2>/dev/null)"; then
		g co -
	else
		cd - >/dev/null
	fi
}

function swd () {
        cd $(pwd | sed -e "s|/local-ssd/$USER/[^/]*|/local-ssd/$USER/$1|")
}

# ---[ ZSH Options ]---------------------------------------------------
setopt   NO_GLOBAL_RCS NO_FLOW_CONTROL NO_BEEP MULTIOS
setopt   NO_NOMATCH EXTENDED_GLOB
setopt   LIST_AMBIGUOUS AUTO_LIST AUTO_REMOVE_SLASH
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

# TODO: how to remove it from the history list?
zshaddhistory () {
    [[ $1 != *dcommit* ]]
}

# ---[ Completion system ]---------------------------------------------
case $USER in
rramacha)
	DOTZSHPATH=~/.zsh/linux;;
artagnon)
	DOTZSHPATH=~/.zsh;;
esac

fpath=($DOTZSHPATH/completion $fpath)
test -f $DOTZSHPATH/completion/go.zsh && source $DOTZSHPATH/completion/go.zsh
test -f $DOTZSHPATH/completion/perf.sh && source $DOTZSHPATH/completion/perf.sh

# Hack to complete some aliases
_git_fp () { _git_format_patch; }
_git_sel () { _git_send_email; }
_git_seg () { _git_send_email; }
compdef g=git

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path $DOTZSHPATH/cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list '+' '+m:{|:lower:|}={|:upper:|}' '+l:|=* r:|=*' '+r:|[._-]=** r:|=**'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' verbose yes
zstyle ':completion:*' insert-tab false
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
source $DOTZSHPATH/prompt/git-prompt.sh

GIT_PS1_DESCRIBE_STYLE=branch
GIT_PS1_SHOWUPSTREAM=git
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWCOLORHINTS=true
GIT_PS1_STATESEPARATOR=""

SSH_PROMPT_INDICATOR=""
if [[ -n $SSH_CLIENT ]]; then
	SSH_PROMPT_INDICATOR="%F{cyan}^%f"
fi

precmd () { __git_ps1 "%F{white}%B%n%b%f" "$SSH_PROMPT_INDICATOR:%F{yellow}%B%~%b%f%(!.#.$) " "|%s" }
