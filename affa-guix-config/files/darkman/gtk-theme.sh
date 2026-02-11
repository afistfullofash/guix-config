#!/bin/sh
case "$1" in
    dark)
	THEME=Dracula;
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
	gsettings set org.gnome.desktop.interface icon-theme "$THEME"
	ln -sf ${XDG_CONFIG_HOME}/gtk-3.0/dark.settings.ini ${XDG_CONFIG_HOME}/gtk-3.0/settings.ini
	;;
    light)
	THEME=catppuccin-latte-mauve-standard+default
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
	gsettings set org.gnome.desktop.interface icon-theme Adwaita
	ln -sf ${XDG_CONFIG_HOME}/gtk-3.0/light.settings.ini ${XDG_CONFIG_HOME}/gtk-3.0/settings.ini
	;;
esac

gsettings set org.gnome.desktop.interface gtk-theme "$THEME";
gsettings set org.gnome.desktop.wm.preferences theme "$THEME";

cp -f ${XDG_CONFIG_HOME}/gtk-2.0/${THEME}.gtkrc-2.0 ${HOME}/.gtkrc-2.0

