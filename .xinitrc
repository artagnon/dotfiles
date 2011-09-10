#!/bin/sh
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
xset b off
setxkbmap -option ctrl:nocaps
$HOME/bin/dzen.sh &
DEFAULT_SESSION=xmonad

case $1 in
stumpwm)
	exec stumpwm
	;;
lxde)
	exec lxde
	;;
xmonad)
	exec $HOME/bin/xmonad
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
