#!/bin/sh
which nvidia-settings && nvidia-settings -a InitialPixmapPlacement=0
xsetroot -cursor_name left_ptr
xset b off
setxkbmap -option ctrl:nocaps
setxkbmap -option altwin:swap_alt_win
DEFAULT_SESSION=i3

case $1 in
awesome)
	exec awesome
	;;
i3)
	exec i3
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
