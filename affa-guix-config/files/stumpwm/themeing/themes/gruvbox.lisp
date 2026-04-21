(in-package :stumpwm-themeing)

;; *** Gruvbox
(add-theme :gruvbox
  	   (let ((fg "#ebdbb2")
  		 (bg "#282828")
  		 (border "#665c54"))
      	     (make-instance 'theme
			    :name "Gruvbox"
      			    :fg fg
      			    :bg bg
      			    :border border
      			    :focus fg
      			    :unfocus bg
      			    :mode-line-fg fg
      			    :mode-line-bg bg
      			    :mode-line-border border
      			    :black bg
  			    :white fg)))

(add-theme :gruvbox-light
  	   (let* ((fg "#3c3836")
  		  (fg4 "#7c6f64")
  		  (bg "#fbf1c7")
  		  ;; 124
  		  (red '("#cc241d" "#9d0006"))
  		  ;; 106
  		  (green '("#98971a" "#79740e"))
  		  ;; 172
  		  (yellow '("#d79921" "#b57614"))
  		  ;; 66
  		  (blue '("#458588" "#076678"))
  		  ;; 132
  		  (purple '("#b16286" "#8f3f71"))
  		  ;; 72
  		  (aqua '("#689d6a" "#427b58"))
  		  (orange '("#d65d0e" "#af3a03"))
  		  ;; 243
  		  (gray '("#7c6f64" "#928374"))
  		  ;; Bright is bg1
  		  (warning yellow)
  		  ;; Bright is bg3
  		  (error orange))
      	     (make-instance 'theme
			    :name "Gruvbox"
      			    :fg fg
      			    :bg bg
      			    :border (car orange)
      			    :focus fg
      			    :unfocus bg

      			    :mode-line-fg fg
      			    :mode-line-bg bg
      			    :mode-line-border (car orange)
  			    
  			    :black gray
  			    :red red
  			    :green green
  			    :yellow yellow
  			    :blue blue
  			    :magenta purple
  			    :cyan aqua
  			    :white (list fg fg4)
  			    
  			    :custom-one warning
  			    :custom-two error)))
