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

candidates = .gitconfig 	\
.tmux.conf bin .ssh		\
.irbrc .perlcriticrc  		\
.gdbinit .atom .bash_profile	\

all:
	@$(foreach file,$(candidates),$(symlink_to_home))

vscode:
	mkdir -p "~/Library/Application Support/Code - Insiders/User"
	ln -s ~/dotfiles/vscode/settings.json "~/Library/Application Support/Code - Insiders/User/settings.json"
	ln -s ~/dotfiles/vscode/keybindings.json "~/Library/Application Support/Code - Insiders/User/keybindings.json"

clean:
	@$(foreach file,$(candidates),$(remove_if_symlink))

.PHONY: clean all
