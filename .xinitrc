#!/bin/sh
which nvidia-settings && nvidia-settings -a InitialPixmapPlacement=0
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
xset b off
setxkbmap -option ctrl:nocaps
$HOME/bin/dzen.sh &
DEFAULT_SESSION=xmonad

case $1 in
xmonad)
	exec $HOME/bin/xmonad
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
