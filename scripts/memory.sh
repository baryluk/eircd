#!/bin/sh

PID=`pgrep -f 'sname eircd_1'`
if [ "x$PID" = "x" ]; then
	echo notrunning
	exit 1
fi
/bin/ps -o rss= -p $PID
