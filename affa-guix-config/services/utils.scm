(define-module (affa-guix-config services utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ports)
  
  #:use-module (srfi srfi-1)

  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix modules)

  #:export (expand-tilde-path
	    expand-tilde-paths

	    send-notification

	    get-program-output
	    execute-with-lock))

(define (expand-tilde-path path)
  (if (and (positive? (string-length path))
           (char=? (string-ref path 0) #\~))
      (string-append (or (getenv "HOME") "") (substring path 1))
      path))

(define (expand-tilde-paths paths)
  (map expand-tilde-path paths))

(define* (get-program-output prog #:key (arguments '()))  
  (let* ((pid (apply open-pipe*
		     (append (list OPEN_READ
				   prog)
			     arguments)))
	 (output (read-string pid))
	 (status (close-pipe pid)))
    (format #t "~%~%get-program-output:~%program: ~a~%args: ~a~%code: ~a~%returning: ~a~%~%"
	    prog
	    arguments
	    (status:exit-val status)
	    output)
    (list (status:exit-val status) output)))	

(define* (execute-with-lock flock-bin prog #:key (arguments '()))
  (let* ((lock-dir (string-append "/tmp/" (fold string-append "" arguments)))
	 (lock-file (string-append lock-dir "/shepherd.lockfile"))
	 (arguments (append (list "-n" lock-file "-E" "-500" prog) arguments)))
    (mkdir-p lock-dir)
    (get-program-output flock-bin #:arguments arguments)))

(define* (send-notification notify-send-bin
			    title message
			   #:key
			   (level "normal")
			   (with-lock? #f)
			   (get-id? #f)
			   (replace #f))
  (let ((arguments (append
		    (if get-id? '("-p") '())
		    (if replace (list "-r" replace) '())
		    (list "-u" level title message))))
    (get-program-output notify-send-bin #:arguments arguments)))
