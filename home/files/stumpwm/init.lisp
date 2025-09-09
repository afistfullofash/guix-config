(asdf:require-system :slynk)
(slynk:create-server :port 1337
		     :dont-close t)

(setq *startup-message* (format nil "Welcome Natalie!"))

(load "~/.stumpwm.d/visual.lisp")
(load "~/.stumpwm.d/keybindings.lisp")

(run-shell-command "dex -a -s $XDG_CONFIG_HOME/autostart/")
