#!/bin/sh
case "$1" in
    dark)
    echo "Xcursor.theme: Dracula-cursors" | xrdb -merge
    xsetroot -cursor_name left_ptr
    ln -sf ~/.config/xresources/dark.Xresources ~/.Xresources;;
    
    light)
    echo "Xcursor.theme: Adwaita" | xrdb -merge
    xsetroot -cursor_name left_ptr
    ln -sf ~/.config/xresources/light.Xresources ~/.Xresources;;	  
esac
