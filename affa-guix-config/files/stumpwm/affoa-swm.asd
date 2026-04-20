(asdf:defsystem "affoa-swm"
  :depends-on ("stumpwm")
  :components ((:file "package.lisp")))

(asdf:defsystem "affoa-swm/themeing"
  :depends-on ("stumpwm"
	       "affoa-swm")
  
  :components
  ((:module "themeing"
    :serial t
    
    :components
    ((:file "package")
     (:file "theme")
     (:module "themes"
      :serial t
      
      :components
      ((:file "dracula")
       (:file "gruvbox")
       (:file "spacemacs")
       (:file "catppuccin")))))))
