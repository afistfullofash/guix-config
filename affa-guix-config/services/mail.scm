(define-module (affa-guix-config services mail)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:use-module (gnu packages mail)
  #:use-module (affa-guix-config home utils)
  #:use-module (afistfullofash packages mail)
  
  #:export (home-tnatkinson95-gmail-sync-timer
	    home-notmuch-new-timer))


(define home-tnatkinson95-gmail-sync-timer
  (let ((gmi-script
	 (program-file
	  "gmi-script"
	  (let ((mail-dir (home-file-path "/mail/tnatkinson95-gmail/"))
		(gmi (file-append lieer "/bin/gmi")))
	    #~(begin
		 (chdir #$mail-dir)
		 (system* #$gmi "sync"))))))
    (shepherd-timer '(tnatkinson-gmail-sync)
		    "*/10 * * * *"
		    #~(#$gmi-script)
		    #:requirement '())))

(define home-notmuch-new-timer
  (let ((notmuch-script
	 (program-file
	  "notmuch-script"
	  (let ((mail-dir (home-file-path "/mail/"))
		(gmi (file-append notmuch "/bin/notmuch")))
	    #~(begin
		 (chdir #$mail-dir)
		 (system* #$gmi "new"))))))
    (shepherd-timer '(notmuch-new)
		    "*/10 * * * *"
		    #~(#$notmuch-script)
		    #:requirement '())))
