#!/bin/sh
# Darkman passes "light" or "dark" as the first argument ($1)

case "$1" in
    light)
        # Point to your light config
        ln -sf ~/.config/dunst/catppucin.theme.conf ~/.config/dunst/dunstrc
        ;;
    dark)
        # Point to your dark config
        ln -sf ~/.config/dunst/dracula.theme.conf ~/.config/dunst/dunstrc
        ;;
esac

# Reload dunst to apply changes
herd restart dunst
