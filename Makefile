symlink_to_home =						\
	if test -e ~/$(file); then				\
		echo "Warning: ~/$(file) already exists";	\
	else							\
		ln -s ~/dotfiles/$(file) ~/$(file);		\
	fi;

remove_if_symlink =							\
	if test -e ~/$(file); then					\
		if test -h ~/$(file); then				\
			rm ~/$(file);					\
		else							\
			echo "Warning: ~/$(file) is not a symlink";	\
		fi;							\
	fi;

candidates = .zshrc .emacs .gitconfig .hgrc .Xdefaults .i3	\
.ncmpcpp .xinitrc .tmux.conf .i3status.conf bin .ssh		\
.irbrc .perlcriticrc .aspell.conf .aspell.personal .zsh		\
.mpdscribble .gdbinit .npmrc .mpdconf .atom

all:
	@$(foreach file,$(candidates),$(symlink_to_home))
	git submodule update --init --recursive;

	# fish is a special case
	if test -e ~/.config/fish; then				\
		echo "Warning: ~/.config/fish already exists";	\
	else							\
		ln -s ~/dotfiles/fish ~/.config/fish;		\
	fi;

update:
	git submodule foreach 'git checkout master; git pull'

clean:
	@$(foreach file,$(candidates),$(remove_if_symlink))

.PHONY: update clean all
