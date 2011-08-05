#!/bin/sh
xrdb -merge ~/.Xresources
$HOME/bin/randwal.py &
setxkbmap -option ctrl:nocaps
$HOME/bin/dzen.sh &
$HOME/bin/mpdas &
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
