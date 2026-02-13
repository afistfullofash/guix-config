(define-module (affa-guix-config home themes)
  #:use-module (guix gexp)
  #:use-module (afistfullofash packages themes)

  #:export (theme-home-locations
	    theme-xdg-config-locations))

(define (theme-path theme-name theme-package)
  (file-append theme-package (string-append "/share/themes/" theme-name)))

(define (qt-theme-path theme-name theme-package)
  (file-append theme-package (string-append "/share/color-schemes/" theme-name ".conf")))

(define (icon-theme-path theme-name theme-package)
  (file-append theme-package (string-append "/share/icons/" theme-name)))

(define (cursors-theme-path theme-name theme-package)
  (file-append theme-package (string-append "/share/icons/" theme-name)))

(define (package-theme-path theme-name app-theme-name theme-package)
  (file-append theme-package (string-append "/share/themes/" theme-name "/" app-theme-name)))

(define theme-home-locations
  `((".Xresources" ,(package-theme-path "Dracula"
					"xresources/Xresources"
					xresources-dracula-theme))
    
    (".icons/Dracula" ,(icon-theme-path "Dracula" gtk-dracula-icons))
    
    (".themes/Dracula" ,(theme-path "Dracula" gtk-dracula-theme))
    (".themes/catppuccin-latte-mauve-standard+default"
     ,(theme-path "catppuccin-latte-mauve-standard+default" gtk-catppuccin-theme))))

(define theme-xdg-config-locations
  `(("alacritty/themes/catppuccin.toml"
     ,(package-theme-path "catppuccin"
			  "alacritty/catppuccin-latte.toml"
			  alacritty-catppuccin-theme))
    ("alacritty/themes/dracula.toml"
     ,(package-theme-path "Dracula"
			  "alacritty/dracula.toml"
			  alacritty-dracula-theme))
    
    ("dunst/dracula.theme.conf"
     ,(package-theme-path "Dracula"
			  "dunst/dunstrc"
			  dunst-dracula-theme))
    ("dunst/catppuccin.theme.conf"
     ,(package-theme-path "catppuccin"
			  "dunst/latte.conf"
			  dunst-catppuccin-theme))
    
    ("lsd"
     ,(package-theme-path "Dracula"
			  "lsd/"
			  lsd-dracula-theme))

    ("qt5ct/colors"
     ,(qt-theme-path "Dracula"
		     qt5-dracula-theme))

    ("assets"
     ,(package-theme-path "Dracula"
			  "assets"
			  gtk-dracula-theme))
    
    ("starship/catppuccin.toml"
     ,(package-theme-path "catppuccin"
			  "starship/latte.toml"
			  starship-catppuccin-theme))
    ("starship/dracula.toml"
     ,(package-theme-path "Dracula"
			  "starship/starship.theme.toml"
			  starship-dracula-theme))
    ("gtk-4.0/gtk-dark.css"
     ,(package-theme-path "Dracula"
			  "gtk-4.0/gtk-dark.css"
			  gtk-dracula-theme))
    ("gtk-4.0/gtk.css"
     ,(package-theme-path "Dracula"
			  "gtk-4.0/gtk.css"
			  gtk-dracula-theme))))
