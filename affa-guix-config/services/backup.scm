(define-module (affa-guix-config services backup)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-restic-backup-timer))

(define restic (specification->package "restic"))

(define (daily-at h m)
  (format #f "~a ~a * * *" m h))

(define* (restic-b2-wrapper env files
                            #:key (extra-args '()) (subcommand "backup"))
  (program-file "restic-b2-wrapper"
    #~(begin
        (use-modules (srfi srfi-1))

	(define (expand-tilde p)
          (if (and (positive? (string-length p))
                   (char=? (string-ref p 0) #\~))
              (string-append (or (getenv "HOME") "") (substring p 1))
              p))
	
        ;; Export credentials/config
        (for-each (lambda (kv) (setenv (car kv) (cdr kv))) '#$env)
        ;; Exec restic with provided subcommand/args/files
	 (let* ((restic-path #$(file-append restic "/bin/restic"))
               (argv (append
                      (list #$subcommand)
                      '#$extra-args
                      (map expand-tilde '#$files))))
          (apply execl restic-path "restic" argv)))))

(define* (home-restic-backup-timer user files env
                                     #:key
                                     (schedule (daily-at 3 30))
                                     (extra-args '()))
  (let ((wrapper (restic-b2-wrapper env files #:extra-args extra-args)))
    (shepherd-timer '(user-restic-backup)
		    schedule
		    #~(#$wrapper)
		    #:requirement '())))
