#!/bin/bash

#ulimit -n 102400

DIR=/tmp/eircd_mnesia_data
mkdir $DIR

# +K true - enable kernel poll
# +P 134217727 - max processes
# +P 262144 - max processes
# +A 16 - async threads, not very important

#/usr/bin/erl -mnesia dir '"$DIR"' -sname eircd_1 -boot eircd-A_Lenny +K true +P 262144 -smp enable
/usr/bin/erl -mnesia dir '"$DIR"' -sname eircd_1 -boot eircd-A_Lenny +K true -smp disable -setcookie "dupad12"
