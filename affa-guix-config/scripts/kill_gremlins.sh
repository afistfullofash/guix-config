#!/bin/sh
#
# kill_gremlins
# Kill all process's matching <name>
# This is very usefull for programs like:
# - steam
kill -9 $(procs $1 | awk '{ print $1 }' | tail -n +3)
