#!/bin/sh
SHADE=${1}

case "$1" in
    dark)
	THEME=Dracula;
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
	gsettings set org.gnome.desktop.interface icon-theme "$THEME"
	;;
    light)
	THEME=catppuccin-latte-mauve-standard+default
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
	gsettings set org.gnome.desktop.interface icon-theme Adwaita
	;;
esac

gsettings set org.gnome.desktop.interface gtk-theme "$THEME";
gsettings set org.gnome.desktop.wm.preferences theme "$THEME";

ln -sf ${XDG_CONFIG_HOME}/gtk-3.0/${SHADE}.settings.ini ${XDG_CONFIG_HOME}/gtk-3.0/settings.ini
ln -sf ${XDG_CONFIG_HOME}/gtk-2.0/${SHADE}.gtkrc-2.0 ${HOME}/.gtkrc-2.0

