#!/bin/sh
#
# notify_me
# Sends a notification to the desktop when the command it is passed finishes running
# 
"$@"
ret=$?

if [ "$ret" -eq 0 ]; then
    dunstify "Command Success" \
        "Executed: $*" \
        -u normal \
        -t 0 \
        -i dialog-information
else
    dunstify "Command Failed" \
        "$(printf 'Exit Code: %s\nCommand: %s' "$ret" "$*")" \
        -u critical \
        -t 0 \
        -i dialog-error
fi

exit "$ret"
