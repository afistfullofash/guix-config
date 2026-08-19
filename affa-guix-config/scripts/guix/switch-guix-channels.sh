#!/bin/sh
CHANNEL_FILE=~/.config/guix/channels.scm

echo "Removing managed channels.scm and replacing with provided working one"

sudo rm ${CHANNEL_FILE}
cp ~/.scripts/guix/channels.scm.bak ${CHANNEL_FILE}
