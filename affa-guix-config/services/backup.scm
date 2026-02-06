(define-module (affa-guix-config services backup)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)

  #:use-module (affa-guix-config services utils)
  #:export (home-restic-backup-timer))

(define restic (specification->package "restic"))

(define (daily-at h m)
  (format #f "~a ~a * * *" m h))

(define* (restic-b2-wrapper files
                            #:key (extra-args '()) (subcommand "backup"))
  (program-file
   "restic-b2-wrapper"
   (with-imported-modules (source-module-closure '((srfi srfi-1)
						   (affa-guix-config services utils)))
     #~(begin
	 (use-modules (srfi srfi-1)
		      (affa-guix-config services utils))
	 
	 (let* ((restic-bin #$(file-append restic "/bin/restic"))
		(secret-tool #$(file-append libsecret "/bin/secret-tool"))
		(notify-send #$(file-append libnotify "/bin/notify-send"))
		(flock #$(file-append util-linux "/bin/flock"))
		(backup-lock-file "/tmp/restic-backup.lock")
		(get-secret (lambda (secret)
			      (cadr (get-program-output secret-tool #:arguments (append '("lookup") secret))))))
	   
	   (setenv "AWS_ACCESS_KEY_ID" (get-secret '("label" "restic_b2_access_key_id")))
	   (setenv "AWS_SECRET_ACCESS_KEY" (get-secret '("label" "restic_b2_secret_access_key")))
	   (setenv "RESTIC_REPOSITORY" (get-secret '("label" "restic_b2_repository")))
	   (setenv "RESTIC_PASSWORD" (get-secret '("label" "restic_b2_password")))

	   (send-notification notify-send "Backup" "Starting Backup")
           
	   (let* ((notify-id (cadr (send-notification
				 notify-send
				 "Backup"
				 "restic is currently running"
				 #:get-id? #t)))
		 (args (append (list "backup" "--one-file-system" "--verbose")
			       (expand-tilde-paths #$files)))
		 (restic-result (execute-with-lock flock restic-bin args)))
	     (cond
	      ((zero? (car restic-result))
	       (send-notification notify-send
				  "Backup"
				  (format #f "Successfully Backed up files\nExit Code: ~a" restic-result)
				  #:replace notify-id)
	       (format #t "Successfully performed backup"))
	      ((equal (car restic-result) "-500")
	       (send-notification notify-send
				  "Backup" "Another backup is already running"
				  #:level "critical"
				  #:replace notify-id))
	      (else
	       (send-notification notify-send
				  "Backup"
				  (format #f "Failed to run backup\nBackup Return Code: ~a" (car restic-result))
				  #:level "critical"
				  #:replace notify-id)
	       (format #t "Failed Restic Output: ~a" (cdr restic-result))
	       (error "Could not backup")))))))))

(define* (home-restic-backup-timer user files
                                     #:key
                                     (schedule (daily-at 3 30))
                                     (extra-args '()))
  (let ((wrapper (restic-b2-wrapper files #:extra-args extra-args)))
      (shepherd-timer '(user-restic-backup)
		      schedule
		      #~(#$wrapper)
		      #:requirement '())))
