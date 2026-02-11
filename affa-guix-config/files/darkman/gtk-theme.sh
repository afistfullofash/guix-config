#!/bin/sh
case "$1" in
    dark) THEME=Dracula ;;
    light) THEME=Dracula ;;
esac

case "$1" in
    dark) gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark';;
    light) gsettings set org.gnome.desktop.interface color-scheme 'prefer-light';;
esac

gsettings set org.gnome.desktop.interface gtk-theme "$THEME";
gsettings set org.gnome.desktop.wm.preferences theme "$THEME"
gsettings set org.gnome.desktop.interface icon-theme "$THEME"

cp -f ${XDG_CONFIG_HOME}/darkman/gtk2/${THEME}.gtkrc-2.0 ${HOME}/.gtkrc-2.0 
