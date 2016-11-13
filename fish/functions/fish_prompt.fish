function fish_prompt
	set_color yellow
	echo (pwd) | sed -e "s|^$HOME|~|" -e "s|^/sandbox/rramacha|\$s|"
	set_color normal
	echo '$ '
end
