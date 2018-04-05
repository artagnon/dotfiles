alias l='ls --color=auto'
alias ll='ls --color=auto -lha'
alias resh='source ~/.bash_profile'
alias rf='rm -rf'
shopt -s autocd
function parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\u@\h \[\033[32m\]\w\[\033[33m\]\$(parse_git_branch)\[\033[00m\] $ "
export PATH=~/bin:$PATH
export PATH="$HOME/.cargo/bin:$PATH"
source scl_source enable python27
test -r /curr/ramk/.opam/opam-init/init.sh && . /curr/ramk/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
function mygit() {
    if [[ $# -eq 0 ]]; then
        git status;
    else
        git "$@";
    fi
}
alias g=mygit
