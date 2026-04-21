(in-package :stumpwm-themeing)

;; *** Spacemacs
(add-theme :spacemacs
  	   (let ((grey "#292b2e")
  		 (purple "#5d4d7a"))
  	     (make-instance 'theme
			    :name "Spacemacs"
  			    :fg purple
  			    :bg grey
  			    :border purple
  			    :focus purple
  			    :unfocus grey
  			    
  			    :mode-line-fg purple
  			    :mode-line-bg grey
  			    :mode-line-border purple
  			    
  			    :black grey
  			    :white purple)))
