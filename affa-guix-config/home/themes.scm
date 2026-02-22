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
  `((".icons/Dracula" ,(icon-theme-path "Dracula" gtk-dracula-icons))
    (".icons/cat-latte-pink-Papirus" ,(icon-theme-path "cat-latte-pink-Papirus" catppuccin-papirus-folders-icons))
    (".icons/Dracula-cursors" ,(cursors-theme-path "Dracula-cursors" dracula-cursors))
    
    (".themes/Dracula" ,(theme-path "Dracula" gtk-dracula-theme-2026))
    (".themes/catppuccin-latte-mauve-standard+default"
     ,(theme-path "catppuccin-latte-mauve-standard+default" gtk-catppuccin-theme))))

(define alacritty-theme-files
  `(("alacritty/themes/catppuccin.toml"
     ,(package-theme-path "catppuccin"
			  "alacritty/catppuccin-latte.toml"
			  alacritty-catppuccin-theme))
    ("alacritty/themes/dracula.toml"
     ,(package-theme-path "Dracula"
			  "alacritty/dracula.toml"
			  alacritty-dracula-theme))))

(define bat-theme-files
  `(("bat/themes" ,(package-theme-path "catppuccin"
				       "bat"
				       bat-catppuccin-theme))))

(define dunst-theme-files
  `(("dunst/dracula.theme.conf"
       ,(package-theme-path "Dracula"
			    "dunst/dunstrc"
			    dunst-dracula-theme))
      ("dunst/catppuccin.theme.conf"
       ,(package-theme-path "catppuccin"
			    "dunst/latte.conf"
			    dunst-catppuccin-theme))))

(define lsd-theme-files
  `(("lsd"
     ,(package-theme-path "Dracula"
			  "lsd/"
			  lsd-dracula-theme))))

(define starship-theme-files
  `(("starship/catppuccin.toml"
     ,(package-theme-path "catppuccin"
			  "starship/latte.toml"
			  starship-catppuccin-theme))
    ("starship/dracula.toml"
     ,(package-theme-path "Dracula"
			  "starship/starship.theme.toml"
			  starship-dracula-theme))))

(define gtk-theme-files
  `(("gtk-4.0/gtk-dark.css"
     ,(package-theme-path "Dracula"
			  "gtk-4.0/gtk-dark.css"
			  gtk-dracula-theme-2026))
    ("gtk-4.0/gtk.css"
     ,(package-theme-path "Dracula"
			  "gtk-4.0/gtk.css"
			  gtk-dracula-theme-2026))))

(define xresources-theme-files
  `(("xresources/dark.Xresources" ,(package-theme-path "Dracula"
						       "xresources/Xresources"
						       xresources-dracula-theme))
    ("xresources/light.Xresources" ,(package-theme-path "catppuccin"
							"Xresources/latte.Xresources"
							xresources-catppuccin-theme))))

(define qt-theme-files
  `(("qt5ct/colors"
     ,(qt-theme-path "Dracula"
		     qt5-dracula-theme))))

(define assets-theme-files
  `(("assets"
     ,(package-theme-path "Dracula"
			  "assets"
			  gtk-dracula-theme-2026))))

(define theme-xdg-config-locations
  (append alacritty-theme-files
	  bat-theme-files
	  dunst-theme-files
	  lsd-theme-files
	  starship-theme-files
	  gtk-theme-files
	  xresources-theme-files
	  qt-theme-files
	  assets-theme-files))
