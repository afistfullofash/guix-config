(define-module (affa-guix-config services mail)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:use-module (gnu packages mail)
  #:use-module (affa-guix-config home utils)
  #:use-module (afistfullofash packages mail)
  
  #:export (home-hydroxide-service
	    home-tnatkinson95-gmail-sync-timer
	    home-work-email-sync-timer
	    home-natalieatkinson95-proton-sync-timer
	    home-notmuch-new-timer))


(define home-hydroxide-service
  (simple-service
   'hydroxide home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run hydroxide proton bridge")
     (requirement '())
     (auto-start? #t)
     (one-shot? #f)
     (provision '(hydroxide))
     (start #~(make-forkexec-constructor
               (list #$(file-append hydroxide "/bin/hydroxide") "imap")))
     (stop #~(make-kill-destructor))))))

(define home-tnatkinson95-gmail-sync-timer
  (let ((gmi-script
	 (program-file
	  "gmi-script"
	  (let ((mail-dir (home-file-path "/mail/tnatkinson95-gmail/"))
		(gmi (file-append lieer "/bin/gmi")))
	    #~(begin
		 (chdir #$mail-dir)
		 (system* #$gmi "sync"))))))
    (shepherd-timer '(mail-tnatkinson-gmail-sync)
		    "*/5 * * * *"
		    #~(#$gmi-script)
		    #:requirement '())))

(define home-work-email-sync-timer
  (let ((gmi-script
	 (program-file
	  "gmi-script"
	  (let ((mail-dir (home-file-path "/mail/work/"))
		(gmi (file-append lieer "/bin/gmi")))
	    #~(begin
		 (chdir #$mail-dir)
		 (system* #$gmi "sync"))))))
    (shepherd-timer '(mail-work-email-sync)
		    "*/5 * * * *"
		    #~(#$gmi-script)
		    #:requirement '())))


(define home-natalieatkinson95-proton-sync-timer
  (shepherd-timer '(mail-natalieatkinson95-proton-sync)
		  "*/5 * * * *"
		  #~(#$(file-append isync "/bin/mbsync") "natalie-atkinson-proton")
		  #:requirement '()))

(define home-notmuch-new-timer
  (let ((notmuch-script
	 (program-file
	  "notmuch-script"
	  (let ((mail-dir (home-file-path "/mail/"))
		(gmi (file-append notmuch "/bin/notmuch")))
	    #~(begin
		 (chdir #$mail-dir)
		 (system* #$gmi "new"))))))
    (shepherd-timer '(mail-notmuch-new)
		    "*/2 * * * *"
		    #~(#$notmuch-script)
		    #:requirement '())))
