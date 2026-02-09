#!/bin/sh
case "$1" in
    dark) THEME=dracula ;;
    light) THEME=catppuccin ;;
esac
cp -f ~/.config/alacritty/themes/${THEME}.toml ~/.config/alacritty/themes/theme.toml
