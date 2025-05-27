set PATH ~/bin ~/.rbenv/bin ~/.local/bin ~/.cargo/bin ~/.yarn/bin /opt/homebrew/bin /opt/homebrew/sbin $PATH
set PATH ~/install/avr8-gnu-toolchain-linux_x86_64/bin $PATH

# venv
source ~/venv/bin/activate.fish

# aliases
alias code=code-insiders

# rbenv
rbenv init - | source

# ccache
set -gx CMAKE_C_COMPILER_LAUNCHER ccache
set -gx CMAKE_CXX_COMPILER_LAUNCHER ccache

# perl
set -gx PERL_MM_OPT "INSTALL_BASE=$HOME/.perl5"
set -gx PERL5LIB "$HOME/.perl5/lib/perl5"
set -gx PERL_LOCAL_LIB_ROOT "$HOME/.perl5/lib/perl5"

# opam configuration
source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
