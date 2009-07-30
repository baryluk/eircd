#!/bin/bash

ulimit -n 102400

# +K true - enable kernel poll
# +P 134217727 - max processes
# +P 262144 - max processes

/usr/bin/erl -sname eircd_flood +K true +P 262144 -smp enable
