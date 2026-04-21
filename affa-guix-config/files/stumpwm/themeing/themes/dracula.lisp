(in-package :stumpwm-themeing)

;; *** Draculao
(add-theme :dracula
       	   (let* ((green "#50FA7B")
		  (yellow "#F1FA8C")
		  (red "#FF5555")
		  (purple '("#BD93F9" "#ff79c6"))

		  (fg '("#F8F8F2" "#f1fa8c"))
		  (bg '("#282A36" "#44475a"))
		  (border (car purple)))
       	     (make-instance 'theme
			    :name "Dracula"
       			    :fg (car fg)
       			    :bg (car bg)
       			    :border border
       			    :focus (car purple)
       			    :unfocus (car purple)

  			    :mode-line-fg (car purple)
       			    :mode-line-bg (car bg)
       			    :mode-line-border border

       			    :black  bg
       			    :white  fg
  			    :red red
       			    :green green
       			    :yellow yellow 
       			    :blue "#8BE9FD"
  			    :magenta purple
       			    :cyan "#FF79C6"

  			    :low green
  			    :medium yellow
  			    :high red
  			    :light-fg fg
       			    :custom-one "#44475A"
       			    :custom-two "#6272A4")))
