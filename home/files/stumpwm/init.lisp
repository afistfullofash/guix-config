(setq *startup-message* (format nil "Welcome Natalie!"))

(load "~/.stumpwm.d/visual.lisp")
(load "~/.stumpwm.d/keybindings.lisp")

(run-shell-command "dex -a -s $XDG_CONFIG_HOME/autostart/")
