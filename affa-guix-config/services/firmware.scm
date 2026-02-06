(define-module (affa-guix-config services firmware)
  #:use-module (guix gexp)
  
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)

  #:use-module (gnu packages polkit)
  #:use-module (nongnu packages firmware)
  
  #:export (fwupd-service-type))

(define fwupd-service-type
  (service-type
   (name 'fwupd)
   (extensions
    (list
     ;; Ensure that fwupdmgr and pkttyagent are avaliable
     (service-extension profile-service-type (lambda _ (list fwupd-nonfree polkit)))
     ;; 1. Extend D-Bus with fwupd config
     (service-extension dbus-root-service-type (lambda _ (list fwupd-nonfree)))
     ;; 2. Extend Polkit for update permissions
     (service-extension polkit-service-type (lambda _ (list fwupd-nonfree)))
     ;; 3. Define the Shepherd daemon
     (service-extension shepherd-root-service-type
                        (lambda _
                          (list (shepherd-service
                                 (provision '(fwupd))
                                 (requirement '(dbus-system))
                                 (start #~(make-forkexec-constructor
                                           (list #$(file-append fwupd-nonfree "/libexec/fwupd/fwupd"))))
                                 (stop #~(make-kill-destructor))))))))
   (default-value #f)
   (description "Firmware management service for fwupd.")))
