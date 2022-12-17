set PATH ~/.rbenv/bin /usr/local/bin ~/.local/bin ~/bin ~/go/bin ~/.cargo/bin ~/.yarn/bin ~/.config/yarn/global/node_modules/.bin $PATH
set -gx GPG_TTY $(tty)

# zoxide
zoxide init fish | source

# rbenv
status --is-interactive; and rbenv init - | source

# opam configuration
source /Users/artagnon/.opam/opam-init/init.fish >/dev/null 2>/dev/null; or true

# perl
set PERL_MM_OPT "INSTALL_BASE=$HOME/.perl5"
