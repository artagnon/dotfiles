function fish_prompt
    set -l cwd (echo (pwd) | sed -e "s|^$HOME|~|" -e "s|^/sandbox/rramacha|\$s|")
    echo -n (set_color yellow)$cwd(set_color normal)'$ '
end
