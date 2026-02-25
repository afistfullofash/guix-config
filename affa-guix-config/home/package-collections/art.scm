(define-module (affa-guix-config home package-collections art)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages kde-graphics)
  
  #:export (art-packages))

(define art-packages
  (list
   ;; Photo Management
   darktable
   digikam
   entangle
   gimp
   rawtherapee))


