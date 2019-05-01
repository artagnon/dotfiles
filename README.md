# dotfiles

My dotfiles.  Clone into `~/dotfiles` and run `make`. The Makefile will create
symlinks to all the files in the repository in `~`.  Don't worry though: if you
already have existing directories like `~/.ssh`, the Makefile will issue a
warning and not overwrite it.
