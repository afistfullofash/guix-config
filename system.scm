(use-modules (gnu)
	     (gnu services)
	     (gnu packages shells)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))

(use-service-modules cups desktop networking ssh xorg docker)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_AU.utf8")
  (timezone "Australia/Sydney")
  (keyboard-layout (keyboard-layout "au"))
  (host-name "reason")

  (users (cons* (user-account
                 (name "thomas")
                 (comment "Thomas Atkinson")
                 (group "users")
                 (home-directory "/home/thomas")
		 (shell (file-append zsh "/bin/zsh"))
                 (supplementary-groups '("wheel" "netdev" "audio" "video" "docker")))
                %base-user-accounts))

  (packages (append
	     (specifications->packages
	      '("stumpwm"
		"blueman"
		"xdg-utils"
		"firefox"
		"zsh"
		"font-google-noto"
		"font-liberation"))
             %base-packages))

  (services
   (append (list (service xfce-desktop-service-type)
		 (service bluetooth-service-type)
		 (service gnome-desktop-service-type)
		 (service containerd-service-type)
		 (service docker-service-type)
                 (service cups-service-type)
		 (service gnome-keyring-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; This is the default list of services we
           ;; are appending to.
	   ;; (modify-services %desktop-services
           ;;   (guix-service-type config => (guix-configuration
           ;;     (inherit config)
           ;;     (substitute-urls
           ;;      (append (list "https://substitutes.nonguix.org")
           ;;        %default-substitute-urls))
           ;;     (authorized-keys
           ;;      (append (list (local-file "./signing-key.pub"))
           ;;        %default-authorized-guix-keys)))))
           %desktop-services))
  
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))
               (keyboard-layout keyboard-layout)))

  (file-systems (cons* (file-system
                         (mount-point "/home/thomas")
                         (device (uuid
                                  "930472ef-8c72-4154-92fb-8d045196d45e"
                                  'btrfs))
                         (type "btrfs"))
                       (file-system
                         (mount-point "/steam")
                         (device (uuid
                                  "782358bc-ce0d-4ef3-b6d1-6655b619a883"
                                  'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "E80B-DBBE"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "a49c3c67-4927-41d6-b48f-fa4f5cc2e502"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
