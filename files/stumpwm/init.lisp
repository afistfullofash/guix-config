;;; Swap Caps with Ctrl
(run-shell-command "~/fix-screens.sh")
(setq *startup-message* (format nil "Welcome Thomas!~%Slynk is on port 4004~%Happy Hacking!"))

(load "~/.stumpwm.d/visual.lisp")
(load "~/.stumpwm.d/keybindings.lisp")

(run-shell-command "dex -a -s $XDG_CONFIG_HOME/autostart/")
