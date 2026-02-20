#!/bin/sh
case "$1" in
    dark) THEME=dracula ;;
    light) THEME=catppuccin ;;
esac
ln -sf ~/.config/alacritty/themes/${THEME}.toml ~/.config/alacritty/alacritty.toml
