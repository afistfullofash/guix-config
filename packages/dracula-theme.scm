(define-module (packages dracula-theme)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)

  #:export (dracula-gtk-theme))

(define-public dracula-gtk-theme
  (package
    (name "dracula-gtk-theme")
   (version "918905b1d6175c506ad0f56fa9bb799a912e45f1")
   (source (origin
	     (uri (git-reference
		       (url "https://github.com/dracula/gtk.git")
		       (commit "918905b1d6175c506ad0f56fa9bb799a912e45f1")))
             (method git-fetch)
             (sha256
              (base32
               "0411xwm0zaaajlviil2za14113jlgksxayxvaxp73rrq8mi8x63i"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan '(("Art" "/usr/share/themes/Dracula/Art")
		       ("assets" "/usr/share/themes/Dracula/assets")
		       ("cinnamon" "/usr/share/themes/Dracula/cinnamon")
		       ("gnome-shell" "/usr/share/themes/Dracula/gnome-shell")
		       ("gtk-2.0" "/usr/share/themes/Dracula/gtk-2.0")
		       ("gtk-3.0" "/usr/share/themes/Dracula/gtk-3.0")
		       ("gtk-3.20" "/usr/share/themes/Dracula/gtk-3.20")
		       ("gtk-4.0" "/usr/share/themes/Dracula/gtk-4.0")
		       ("kde" "/usr/share/themes/Dracula/kde")
		       ("metacity-1" "/usr/share/themes/Dracula/metacity-1")
		       ("src" "/usr/share/themes/Dracula/src")
		       ("unity" "/usr/share/themes/Dracula/unity")
		       ("xfwm4" "/usr/share/themes/Dracula/xfwm4"))))
   (synopsis "A dark theme for GTK.")
   (description
    "A dark theme for GTK.")
   (home-page "https://github.com/dracula/gtk")
   (license license:isc)))

dracula-gtk-theme
