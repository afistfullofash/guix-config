(define-module (systems desktop))

(use-modules (systems base)
	     (gnu)
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

(define system
  (operating-system
    (inherit base-operating-system)
    (host-name "reason")

    (file-systems
     (cons*
      (file-system
	(mount-point "/home/natalie")
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
	(type "ext4")) %base-file-systems))))

system
