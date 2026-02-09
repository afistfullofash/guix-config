#!/bin/sh
case "$1" in
    dark) THEME=Dracula ;;
    light) THEME=Dracula ;;
esac
gsettings set org.gnome.desktop.interface gtk-theme "$THEME";
gsettings set org.gnome.desktop.wm.preferences theme "$THEME"
gsettings set org.gnome.desktop.interface icon-theme "$THEME" 
