symlink_to_home =															\
	if test -e ~/$(file); then									\
		echo "Warning: ~/$(file) already exists";	\
	else																				\
		ln -s ~/dotfiles/$(file) ~/$(file);				\
	fi;

symlink_to_config =																		\
	if test -e ~/.config/$(file); then									\
		echo "Warning: ~/.config/$(file) already exists";	\
	else																								\
		ln -s ~/dotfiles/$(file) ~/.config/$(file);				\
	fi;

remove_home_symlink =															\
	if test -e ~/$(file); then											\
		if test -h ~/$(file); then										\
			rm ~/$(file);																\
		else																					\
			echo "Warning: ~/$(file) is not a symlink";	\
		fi;																						\
	fi;

remove_config_symlink =																		\
	if test -e ~/.config/$(file); then											\
		if test -h ~/.config/$(file); then										\
			rm ~/.config/$(file);																\
		else																									\
			echo "Warning: ~/.config/$(file) is not a symlink";	\
		fi;																										\
	fi;

home_candidates = .gitconfig bin .perlcriticrc .irbrc \
									.zshrc .wezterm.lua

config_candidates = fish nvim

all:
	@$(foreach file,$(home_candidates),$(symlink_to_home))
	@$(foreach file,$(config_candidates),$(symlink_to_config))

clean:
	@$(foreach file,$(home_candidates),$(remove_home_symlink))
	@$(foreach file,$(config_candidates),$(remove_config_symlink))

.PHONY: clean all
