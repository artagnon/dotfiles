#!/bin/zsh
# Copyright (C) 2009 Ramkumar Ramachandra <artagnon@gmail.com>

# ---[ Dzen variables ]------------------------------------------------
BG='#1f1f1f'
FG='#eeeeec'
W=3840
H=20
X=0
Y=0
FN='inconsolata:size=11'
ICONPATH=$HOME/.dzen

# ---[ gdbar variables ]-----------------------------------------------
GW=50            # width of the gauge
GFG='#73d216'    # color of the gauge
GH=7             # height of the gauge
GBG='#333333'    # color of gauge background
LOWBAT=25        # percentage of battery life marked as low
LOWCOL='#ff4747' # color when battery is low

# ---[ MPD controls ]--------------------------------------------------
MPD_TOGGLE="mpc toggle &> /dev/null"
MPD_NEXT="mpc next &> /dev/null"

# ---[ Time intervals ]------------------------------------------------
MASTER_INTERVAL=1
MUSIC_INTERVAL=1
BATTERY_INTERVAL=10
DATE_INTERVAL=30

# ---[ Functions ]-----------------------------------------------------

fbattery()
{

    STATE=`acpi|cut -d ' ' -f3|cut -d, -f1`;
    RPERC=`acpi|cut -d, -f2|cut -d% -f1`;
    
    if [ $RPERC -le $LOWBAT ]; then
	GFG=$LOWCOL
    fi
    
    if [ $STATE = "Discharging" ]; then
	PREBAR="^i(${ICONPATH}/battery.xbm)"
    else
	PREBAR="^i(${ICONPATH}/ac.xbm)"
    fi

    echo "$PREBAR^p(;+3)" `echo $RPERC | gdbar -h $GH -w $GW -fg $GFG -bg $GBG` "^p()"
}

fdate()
{
    DATE_FORMAT='%A, %d %b %y | %H:%M'
    date +$DATE_FORMAT
}

fmusic()
{
    STATUS=`mpc | sed -ne 's/^.*\[\([a-z]*\)\].*$/\1/p'`
    if [ $STATUS ]; then
	POS=`mpc | sed -ne 's/^.*(\([0-9]*\)%).*$/\1/p'`
	POSM="$POS 100"
	STATUS_MESSAGE=`mpc | sed -n '1p' | tr '\n' ' '`
	PREBAR="^i(${ICONPATH}/play.xbm)"
	if [ $STATUS = "paused" ]; then
	    PREBAR="^i(${ICONPATH}/pause.xbm)"
	fi
    else
	STATUS_MESSAGE="[Stopped]"
	PREBAR="^i(${ICONPATH}/music.xbm)"
	POSM="0 100"
    fi
    echo "$PREBAR $STATUS_MESSAGE^p(;+4)" `echo $POSM | gdbar -h $GH -w $GW -fg $GFG -bg $GBG` "^p()"
}

# ---[ Main eval loop ]------------------------------------------------
BATTERY_COUNTER=$BATTERY_INTERVAL;DATE_COUNTER=$DATE_INTERVAL;MUSIC_COUNTER=$MUSIC_INTERVAL

while true; do
    if [ $DATE_COUNTER -ge $DATE_INTERVAL ]; then
	PDATE=$(fdate)
	DATE_COUNTER=0
    fi
    
    if [ $MUSIC_COUNTER -ge $MUSIC_INTERVAL ]; then
	PMUSIC=$(fmusic)
	MUSIC_COUNTER=0
    fi
    
    print "$PMUSIC | $PDATE^p(+10)"

    DATE_COUNTER=$((DATE_COUNTER+1))
    BATTERY_COUNTER=$((BATTERY_COUNTER+1))
    MUSIC_COUNTER=$((MUSIC_COUNTER+1))
    sleep $MASTER_INTERVAL
done | dzen2 -ta r -tw $W -h $H -y $Y -x $X -fg $FG -bg $BG -fn $FN -e "button1=exec:$MPD_TOGGLE;button3=exec:$MPD_NEXT"
