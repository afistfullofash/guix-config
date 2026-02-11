(define-module (affa-guix-config home base)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services gnupg) 
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services backup)

  #:use-module (gnu services)

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix utils)

  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnupg)
  
  #:use-module (ice-9 popen)

  #:use-module (affa-guix-config home utils)

  #:use-module (afistfullofash packages themes)
  #:use-module (affa-guix-config services notifications)

  #:use-module (affa-guix-config home package-collections desktop)
  #:use-module (affa-guix-config home package-collections emacs)
  #:use-module (affa-guix-config home package-collections misc)
  #:use-module (affa-guix-config home package-collections programming)
  #:use-module (affa-guix-config home package-collections spellcheck)
  #:use-module (affa-guix-config home package-collections terminal)
  #:use-module (affa-guix-config home package-collections tree-sitter)
  
  #:export (base-home-environment
	    base-home-services
	    base-home-backup-service))

;;; This tangles the emacs init org file into init.el
;;; Ensuring that the emacs config is never out of date
(define emacs-init-el
  (computed-file
   "init.el"
   (with-imported-modules '((guix build utils))
     #~(begin
	 (use-modules (guix build utils))
	 (let* ((emacs-bin (string-append #$(file-append (specification->package "emacs-lucid") "/bin/emacs")))
		(init-org-file #$(config-file "/emacs/init.org")))
	   (format #t "Tangling ~a...\n" init-org-file)
	   (invoke emacs-bin
		   "--batch"
		   "-q"	; Don't load a user init file
		   init-org-file ; Open the org file in a buffer
		   "--eval" "(require 'org)"
		   "--eval" "(org-babel-tangle)"))
	 (rename-file (string-append (dirname #$(config-file "/emacs/init.org")) "/init.el")
		      #$output)))))

(define isyncrc
  (computed-file
   "isyncrc"
   (with-imported-modules '((guix build utils))
     #~(begin
	 (use-modules (guix build utils))
	  (let ((template #$(config-file "/mbsync/isyncrc"))
		(tool-path (string-append #$libsecret "/bin/secret-tool")))
	    (copy-file template "isyncrc.tmp")
	    ;; Use 'sed' to replace the placeholder with the store path
	    (invoke #$(file-append sed "/bin/sed") "-i"
		    (string-append "s|secret-tool|" tool-path "|g")
		    "isyncrc.tmp")
	    (copy-file "isyncrc.tmp" #$output))))))

(define autorandr-service
  (simple-service
   'autorandr home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run autorandr to set screens")
     (requirement '(x11-display))
     (auto-start? #t)
     (one-shot? #t)
     (provision '(autorandr))
     (start #~(make-forkexec-constructor
               (list #$(file-append autorandr "/bin/autorandr") "--change")))
     (stop #~(make-kill-destructor))))))

(define base-home-channels-service
  (service home-channels-service-type
		  (list
		   (channel
		     (name 'guix)
		     (url "https://codeberg.org/guix/guix.git")
		     (branch "master")
		     (introduction
		      (make-channel-introduction
		       "9edb3f66fd807b096b48283debdcddccfea34bad"
		       (openpgp-fingerprint
			"BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
		   (channel
		    (name 'afistfullofash)
		    (url "https://github.com/afistfullofash/afistfullofash")
		    (branch "main"))
		   (channel
		    (name 'nonguix)
		    (url "https://gitlab.com/nonguix/nonguix")
		    ;; Enable signature verification:
		    (introduction
		     (make-channel-introduction
		      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
		      (openpgp-fingerprint
		       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))))

(define environment-variables-service
  (simple-service 'base-environment-variables-service
		  home-environment-variables-service-type
		  `(("GTK_THEME" .  "Dracula")
		    ("LIBSEAT_BACKEND" . "logind")
		    ("GUILE_LOAD_PATH" . ,(environment-variable-seperated-path
					   '("/src/guix-config"
					     "/src/afistfullofash")))
		    ("GUIX_HOME_PATH" . ,(home-file-path "/.guix-home/profile"))
		    ("PATH" . ,(string-join (list (home-file-path "/src/shell-scripts/") ; Custom Shell Scripts
						  "${PATH}") ; Original Value
					    ":"))
		    ("BOUNDARY_KEYRING_TYPE" . "secret-service")
		    ("BROWSER" . ,(specification->package "firefox"))
		    ("BAT_THEME" . "Dracula")
		    ("TREE_SITTER_LIBDIR"
		     . ,#~(string-join (map (lambda (pkg)
					      (string-append pkg "/lib"))
					    '#$tree-sitter-grammar-packages) ;
				       ":"))
		    ("AFFOA_SYSTEM_THEME_TYPE" . "dark")
		    ;; Locale
		    ("TZ" . "Australia/Sydney")
		    ("LC_ALL" . "en_AU.utf8")
		    ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"))))

(define stumpwm-init-lisp
  (computed-file
   "init.lisp"
   (with-imported-modules '((guix build utils))
     #~(begin
	 (use-modules (guix build utils))
	 (let* ((emacs-bin (string-append #$(file-append (specification->package "emacs-lucid") "/bin/emacs")))
		(init-org-file #$(config-file "/stumpwm/config.org")))
	   (format #t "Tangling ~a...\n" init-org-file)
	   (invoke emacs-bin
		   "--batch"
		   "-q"		       ; Don't load a user init file
		   init-org-file       ; Open the org file in a buffer
		   "--eval" "(require 'org)"
		   "--eval" "(org-babel-tangle)"))
	 (rename-file (string-append (dirname #$(config-file "/stumpwm/config.org")) "/init.lisp")
		      #$output)))))

(define home-file-locations
  `((".themes/Dracula" ,(file-append gtk-dracula-theme "/Dracula"))
    (".icons/Dracula" ,gtk-dracula-icons)
    (".Xresources" ,xresources-dracula-theme)
    ;; Setup Git for multiple emails
    ;; This gets configured based on file path
    (".gitconfig" ,(config-file "/git/gitconfig"))
    (".gitignore" ,(config-file "/git/gitignore"))
    ("work/.gitconfig" ,(config-file "/git/work.gitconfig"))
    ;; For some reason this does not work when we pass directories to it
    (".stumpwm.d/init.lisp" ,stumpwm-init-lisp)
    ;; Emacs
    (".emacs.d/init.el" ,emacs-init-el)
    ;; SSH Public Keys
    (".ssh/work.pub" ,(config-file "/ssh/work.pub"))
    (".ssh/nat.pub" ,(config-file "/ssh/nat.pub"))
    ;; Email
    ("mail/work/.gmailieer.json" ,(config-file "/gmi/work.gmailieer.json"))
    (".local/share/darkman" ,(config-file "/darkman"
					  #:recursive? #t))
    ;; Ensure screenshot directory exists
    ("Pictures/Screenshots/.keep" ,(config-file "/keep"))))

(define xdg-config-file-locations
  ;; This Stats with a heap of THEMEING
  `(("assets" ,(file-append gtk-dracula-theme "/Dracula/assets"))
    ;; GTK
    ("gtk-4.0/gtk.css" ,(file-append gtk-dracula-theme "/Dracula/gtk-4.0/gtk.css"))
    ("gtk-4.0/gtk-dark.css" ,(file-append gtk-dracula-theme "Dracula/gtk-4.0/gtk-dark.css"))
    ("gtk-4.0/settings.ini" ,(config-file "/gtk-4.0/settings.ini"))
    ("gtk-2.0/dracula.gtkrc-2.0" ,(config-file "/gtk-2.0/dracula.gtkrc-2.0"))
    ;; QT5
    ("qt5ct/colors" ,qt5-dracula-theme)
    ("qt5ct/qt5ct.conf" ,(config-file "/qt5ct/qt5ct.conf"))
    ;; Specific Programs
    ;; Mahogany
    ("mahogany/init.lisp" ,(config-file "/mahogany/init.lisp"))
    ;; Alacritty
    ("alacritty/alacritty.toml" ,(config-file "/alacritty/alacritty.toml"))
    ("alacritty/themes/dracula.toml" ,(file-append alacritty-dracula-theme "/dracula.toml"))
    ("alacritty/themes/catppuccin.toml" ,(file-append alacritty-catppuccin-theme "/catppuccin-latte.toml"))
    ;; LSD
    ("lsd" ,lsd-dracula-theme)
    ;; Starship
    ("starship/dracula.toml" ,(file-append starship-dracula-theme "/starship.theme.toml"))
    ("starship/catppuccin.toml" ,(file-append starship-catppuccin-theme "/themes/latte.toml"))
    ;; Autorandr
    ("autorandr" ,(config-file "/autorandr"
			      #:recursive? #t))
    ;;mbsync
    ("isyncrc" ,isyncrc)
    ("afew/config" ,(config-file "/afew/config"))
    ("notmuch/default/config" ,(config-file "/notmuch/notmuch-config"))
    ;; Guix
    ("guix/shell-authorized-directories" ,(let ((auth-directorys (string-append (home-file-path "/work") "\n")))
					    (plain-file "shell-authorized-directories" auth-directorys)))
    ;; xdg-desktop-portals
    ("xdg-desktop-portal/portals.conf" ,(config-file "/xdg-desktop-portal/portals.conf"))

    ;; Autostarts
    ("autostart/keepassxc.desktop" ,(config-file "/autostart/keepassxc.desktop"))))

(define ssh-configuration
  (home-openssh-configuration
   (hosts
    (list (openssh-host (name "*")
			(identity-file "~/.ssh/nat.pub"))
	  (openssh-host (name "gitlab.com")
			(identity-file "~/.ssh/work.pub"))))))

(define base-home-services
  (list
   (service home-dunst-service-type
	    (home-dunst-configuration
	     (base-config dunst-dracula-theme)
	     (config
	      (home-dunst-extra-config
	       (global
		(home-dunst-global-config
		 (frame-width 5)
		 (frame-color "#BD93F9")))))))
   
   autorandr-service
   environment-variables-service
   base-home-channels-service

   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (service home-syncthing-service-type)
   (service home-redshift-service-type)
   (service home-ssh-agent-service-type)
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry "/bin/pinentry"))
             (extra-content "allow-loopback-pinentry")
             (default-cache-ttl 3600)))
   (service home-openssh-service-type ssh-configuration)
   (service home-files-service-type home-file-locations)
   (service home-xdg-configuration-files-service-type xdg-config-file-locations)
   (service home-zsh-service-type
	    (home-zsh-configuration
	     (zshrc (list
		     (config-file "/zsh/zshrc.sh")))))))

(define base-home-environment
  (home-environment
   (packages (append 
	      terminal-packages
	      spellcheck-packages
	      development-packages
	      desktop-packages
	      emacs-packages
	      language-server-packages
	      zsh-plugin-packages
	      misc-packages
	      tree-sitter-grammar-packages))
   (services base-home-services)))
