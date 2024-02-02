set PATH ~/.rbenv/bin /opt/homebrew/bin ~/.local/bin ~/bin ~/go/bin ~/.cargo/bin /usr/local/opt/llvm/bin ~/.yarn/bin ~/.config/yarn/global/node_modules/.bin $PATH

# rbenv
rbenv init - | source

# perl
set PERL_MM_OPT "INSTALL_BASE=$HOME/.perl5"

# opam configuration
source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
