#!/bin/sh
case "$1" in
    dark) THEME=dracula ;;
    light) THEME=catppuccin ;;
esac
mkdir -p ~/.cache/starship
echo "$THEME" > ~/.cache/starship/mode
