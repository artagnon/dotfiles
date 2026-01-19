fish_add_path ~/bin ~/.rbenv/bin ~/.local/bin ~/.cargo/bin ~/.yarn/bin ~/install/avr8-gnu-toolchain-linux_x86_64/bin

# venv
test -d ~/venv && source ~/venv/bin/activate.fish; or true

# aliases
alias code=code-insiders

# rbenv
command -q rbenv && rbenv init - | source; or true

# ime for weztterm
set -gx XMODIFIERS @im=ibus

# ccache
set -gx CMAKE_C_COMPILER_LAUNCHER ccache
set -gx CMAKE_CXX_COMPILER_LAUNCHER ccache

# perl
set -gx PERL_MM_OPT "INSTALL_BASE=$HOME/.perl5"
set -gx PERL5LIB "$HOME/.perl5/lib/perl5"
set -gx PERL_LOCAL_LIB_ROOT "$HOME/.perl5/lib/perl5"

# opam configuration
test -f ~/.opam/opam-init/init.fish && source ~/.opam/opam-init/init.fish; or true
