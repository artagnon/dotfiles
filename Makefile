symlink_to_home =							\
	if test -e ~/$(file); then					\
		echo "Warning: ~/$(file) already exists";		\
	else								\
		ln -s ~/dotfiles/$(file) ~/$(file);			\
	fi;

remove_if_symlink =							\
	if test -e ~/$(file); then					\
		if test -h ~/$(file); then				\
			rm ~/$(file);					\
		else							\
			echo "Warning: ~/$(file) is not a symlink";	\
		fi;							\
	fi;

candidates = .zshrc .emacs .gitconfig .Xdefaults .xmonad	 	\
.ncmpcpp .xinitrc .tmux.conf .elisp .rbenv .xmobarrc			\
.irbrc .perlcriticrc .aspell.conf .aspell.personal

all::
	@$(foreach file,$(candidates),$(symlink_to_home))
	git submodule update --init --recursive

clean:
	@$(foreach file,$(candidates),$(remove_if_symlink))
