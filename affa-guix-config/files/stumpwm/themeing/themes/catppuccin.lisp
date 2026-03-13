(in-package :stumpwm-themeing)

;; *** Catppuccin
(let ((rosewater "#dc8a78")
      (flamingo "#dd7878")
      (pink "#ea76cb")
      (mauve "#8839ef")
      (red "#d20f39")
      (maroon "#e64553")
      (peach "#fe640b")
      (yellow "#df8e1d")
      (green "#40a02b")
      (teal "#179299")
      (sky "#04a5e5")
      (saphire "#209fb5")
      (blue "#1e66f5")
      (lavender "#7287fd")

      ;; Items
      (text "#4c4f69")

      (subtext-1 "#5c5f77")
      (subtext-0 "#6c6f85")

      (overlay-2 "#7c7f93")
      (overlay-1 "#8c8fa1")
      (overlay-0 "#9ca0b0")

      (surface-2 "#acb0be")
      (surface-1 "#bcc0cc")
      (surface-0 "#ccd0da")

      (base "#eff1f5")
      (mantle "#e6e9ef")
      (crust "#dce0e8"))	   
  (add-theme
   'catppuccin-latte
   ;; Latte Pallette
   (make-instance 'theme

  		  :fg text
     		  :bg base
     		  :border pink
     		  :focus pink
     		  :unfocus pink
  		  
  		  :mode-line-fg text
     		  :mode-line-bg base
     		  :mode-line-border pink
  		  
     		  :black  overlay-2
  		  :red red
     		  :green green
  		  :yellow yellow
     		  :blue blue
  		  :magenta mauve
     		  :cyan teal
  		  :white  base
  		  
  		  :low green
  		  :medium yellow
  		  :high red
  		  
  		  :light-fg mantle
  		  
     		  :custom-one mantle
     		  :custom-two crust)))
