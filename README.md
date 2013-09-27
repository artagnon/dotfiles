# dotfiles

My dotfiles.  Clone into `~/dotfiles` and run `make`.  Note that this
can take a long time depending on your internet connection, as it will
attempt to recursively fetch all submodules.

The Makefile will create symlinks to all the files in the repository
in `~`.  Don't worry though: if you already have existing files like
`~/.zshrc`, the Makefile will issue a warning and not overwrite it.

## ZSH

`~/.zshrc` will attempt to source `~/.keychain/localhost-sh`.  This
file must be a symlink to `~/.keychain/$HOSTNAME-sh`.  If you don't
know what keychain is, or don't want to use it, simply uncomment the
relevant portion.

grep is aliased to ack-grep, which is also not included in this
repository.  Get ack-grep for your distribution.

## Xdefaults

I use the Droid Sans Mono font.  Should be available for your
distribution easily enough.

## rbenv

ruby-build is not included, so please `git clone
git://github.com/sstephenson/ruby-build ~/.rbenv/plugins/ruby-build`
before you can use `rbenv install`.

## Emacs

You need the latest development version of Emacs.  Either compile from
sources, or find a suitable package for your distribution.  For
Debian, use emacs-snapshot from http://emacs.naquadah.org.
