#!/bin/sh

xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap
$HOME/bin/randwal.py &
$HOME/bin/dzen.sh &
mpdas &
DEFAULT_SESSION=xmonad

case $1 in
stumpwm)
	exec stumpwm
	;;
lxde)
	exec lxde
	;;
xmonad)
	exec /usr/local/bin/xmonad
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
