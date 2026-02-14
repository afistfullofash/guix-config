#!/bin/sh
SHADE=${1}

case "$1" in
    dark)
	THEME=Dracula;
	;;
    light)
	THEME=catppuccin-latte-mauve-standard+default
	;;
esac

ln -sf ${XDG_CONFIG_HOME}/gtk-4.0/${SHADE}.settings.ini ${XDG_CONFIG_HOME}/gtk-3.0/settings.ini
ln -sf ${XDG_CONFIG_HOME}/gtk-3.0/${SHADE}.settings.ini ${XDG_CONFIG_HOME}/gtk-3.0/settings.ini
ln -sf ${XDG_CONFIG_HOME}/gtk-2.0/${SHADE}.gtkrc-2.0 ${HOME}/.gtkrc-2.0

gsettings set org.gnome.desktop.interface gtk-theme "$THEME";
gsettings set org.gnome.desktop.wm.preferences theme "$THEME";

case "$1" in
    dark)
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
	gsettings set org.gnome.desktop.interface icon-theme "$THEME"
	gsettings set org.gnome.desktop.interface cursor-theme "Dracula-cursors"
	;;
    light)
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
	gsettings set org.gnome.desktop.interface icon-theme Adwaita
	gsettings set org.gnome.desktop.interface cursor-theme "Adwaita"
	;;
esac
