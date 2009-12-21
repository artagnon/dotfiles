#!/bin/zsh
# Copyright (C) 2009 Ramkumar Ramachandra <artagnon@gmail.com>


# ---[ Dzen variables ]------------------------------------------------
BG='#000'
FG='#999'
W=1280
X=0
Y=0
FN="-*-terminus-*-*-*-*-9-*-*-*-*-*-*-*"
ICONPATH=$HOME/.dzen

# ---[ Time intervals ]------------------------------------------------
MASTER_INTERVAL=1
BATTERY_INTERVAL=10
DATE_INTERVAL=30

# ---[ Functions ]-----------------------------------------------------

fbattery()
{
    GW=50      # width of the gauge
    GFG='#999'  # color of the gauge
    GH=7       # height of the gauge
    GBG='#333'  # color of gauge background
    LOWBAT=25        # percentage of battery life marked as low
    LOWCOL='#ff4747' # color when battery is low

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

# ---[ Main eval loop ]------------------------------------------------
BATTERY_COUNTER=$BATTERY_INTERVAL;DATE_COUNTER=$DATE_INTERVAL

while true; do
    if [ $DATE_COUNTER -ge $DATE_INTERVAL ]; then
	PDATE=$(fdate)
	DATE_COUNTER=0
    fi
    
    if [ $BATTERY_COUNTER -ge $BATTERY_INTERVAL ]; then
	PBATTERY=$(fbattery)
	MAILCOUNTER=0
    fi
    
    print "$PBATTERY^p(+10)$PDATE"

    DATE_COUNTER=$((DATE_COUNTER+1))
    BATTERY_COUNTER=$((BATTERY_COUNTER+1))
    sleep $MASTER_INTERVAL
done | dzen2 -ta r -tw $W -y $Y -x $X -fg $FG -bg $BG -fn $FN
