(define-module (systems base))

(use-modules (gnu)
	     (gnu services)
	     (gnu services linux)
	     
	     (gnu packages shells)
	     (gnu packages wm)
	     (gnu packages lisp-xyz)
	     (gnu packages hardware)

	     (guix packages)
	     (guix utils)
     
	     (nongnu packages linux)
	     (nongnu system linux-initrd)

	     (srfi srfi-1))

(use-service-modules cups desktop networking ssh xorg docker)

(define-public stumpwm-with-extensions
  (package
    (inherit stumpwm)
    (name "stumpwm-with-extensions")
    (inputs
     (list sbcl-local-time
	   sbcl-stumpwm-pamixer
	   sbcl-stumpwm-battery-portable
	   stumpwm))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 (setenv "HOME" "/tmp")
                 (build-program program outputs
                                #:entry-program '((stumpwm:stumpwm) 0)
                                #:dependencies '("stumpwm"
						 "local-time"
						 "battery-portable"
						 "pamixer")
                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm"
				       "sbcl-local-time"
				       "sbcl-stumpwm-battery-portable"
				       "sbcl-stumpwm-pamixer"))))))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'remove-temporary-cache)
           (delete 'cleanup)))))))

(define-public base-operating-system
  (operating-system
    (host-name "base")
    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))
    (locale "en_AU.utf8")
    (timezone "Australia/Sydney")
    (keyboard-layout (keyboard-layout "au"))


    (users (cons* (user-account
		   (name "natalie")
		   (comment "Natalie Atkinson")
		   (group "users")
		   (home-directory "/home/natalie")
		   (shell (file-append zsh "/bin/zsh"))
		   (supplementary-groups '("wheel" "netdev" "audio" "video" "docker" "lp")))
		  %base-user-accounts))

    (packages (append
	       (specifications->packages
		'("blueman"
		  "bluez"
		  "bluez-alsa"
		  "glibc-locales"
		  "alsa-plugins"
		  "xdg-utils"
		  "firefox"
		  "zsh"
		  "font-google-noto"
		  "font-liberation"
		  "hicolor-icon-theme"
		  "i2c-tools"
		  "openrgb"
		  "flatpak"
		  "gzip"
		  "bzip2"
		  "unzip"
		  "simple-scan"
		  "pamixer"
		  ;; For setting the screenshot time
		  "sbcl-local-time"))
	       (list stumpwm-with-extensions)
	       %base-packages))

    (services
     (append (list
	      (service kernel-module-loader-service-type
		       '("i2c-dev" "i2c-piix4"))
	      (udev-rules-service 'openrgb openrgb)
	      (service bluetooth-service-type
		       (bluetooth-configuration (auto-enable? #t)
						(multi-profile 'multiple)))	    
	      (service iwd-service-type)
	      (service connman-service-type
		       (connman-configuration
			(disable-vpn? #t)
			(shepherd-requirement '(iwd))))
	      (service containerd-service-type)
	      (service docker-service-type)
	      (service cups-service-type
		       (cups-configuration
			(web-interface? #t)))
	      (service gnome-keyring-service-type)
	      (service gnome-desktop-service-type)
	      (set-xorg-configuration
	       (xorg-configuration (keyboard-layout keyboard-layout))))
	     (modify-services %desktop-services
	       (delete wpa-supplicant-service-type)
	       (delete network-manager-service-type)
	       (guix-service-type config => (guix-configuration
					     (inherit config)
					     (substitute-urls
					      (append (list "https://substitutes.nonguix.org")
						      %default-substitute-urls))
					     (authorized-keys
					      (append (list (local-file "../files/nonguix/signing-key.pub"))
						      %default-authorized-guix-keys)))))))
    
    (file-systems (cons*
                   (file-system
                     (mount-point "/tmp")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))
		      
    (bootloader (bootloader-configuration
		 (bootloader grub-efi-bootloader)
		 (targets (list "/boot/efi"))
		 (keyboard-layout keyboard-layout)))))
