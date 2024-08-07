set PATH ~/bin ~/.rbenv/bin ~/.local/bin ~/.cargo/bin ~/.yarn/bin /opt/homebrew/bin /opt/homebrew/sbin $PATH

# venv
source ~/src/llvm-test-suite/venv/bin/activate.fish

# rbenv
rbenv init - | source

# perl
set PERL_MM_OPT "INSTALL_BASE=$HOME/.perl5"

# opam configuration
source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
