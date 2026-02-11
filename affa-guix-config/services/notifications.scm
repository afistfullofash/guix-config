(define-module (affa-guix-config services notifications)
  #:use-module (gnu)
  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (gnu packages wm)
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile-xyz)
  
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)


  #:use-module (afistfullofash packages rust-apps)


  
  #:export (home-dunst-service-type

	    ;; dunst-config
	    
	    home-dunst-configuration
	    home-dunst-configuration?

	    home-dunst-extra-config
	    home-dunst-extra-config?

	    home-dunst-global-config
	    home-dunst-global-config
	    
	    home-runst-service))

(define-record-type* <home-dunst-configuration>
  home-dunst-configuration make-home-dunst-configuration
  home-dunst-configuration?
  (package     home-dunst-package (default dunst))
  (base-config home-dunst-base-config-data (default #f))
  (config home-dunst-extra-config-data (default '(home-dunst-extra-config))))

(define-record-type* <home-dunst-extra-config>
  home-dunst-extra-config make-home-dunst-extra-config
  home-dunst-extra-config?
  (global     home-dunst-global-config-data (default home-dunst-global-config)))


(define-record-type* <home-dunst-global-config>
  home-dunst-global-config make-home-dunst-global-config
  home-dunst-global-config?
  (frame-width     home-dunst-global-frame-width (default #f))
  (frame-color home-dunst-global-frame-color (default #f)))

(define (home-dunst-files-service configuration)
  "Generates the list for home-xdg-configuration-files-service-type."
  (match-record configuration <home-dunst-configuration>
		(base-config config)
    (let* ((global-config (home-dunst-global-config-data config))
	   (frame-width (home-dunst-global-frame-width global-config))
	   (frame-color (home-dunst-global-frame-color global-config)))
	   
      (list
       `("dunst/dunstrc"
	 ,(computed-file
	   "dunstrc"
	   (with-imported-modules '((guix build utils))
	     (let ((base-dunstrc (file-append base-config "/dunstrc"))
		   (sed-bin (file-append sed "/bin/sed"))
		   )    
	       #~(begin
		   (use-modules (ice-9 rdelim)
				(ice-9 regex)
				(srfi srfi-2))
		   (let* ((base-content 
			  (if #$base-config
			      (call-with-input-file
				  (string-append #$base-config "/dunstrc") 
				read-string)
			      ""))
			 (replace-first (lambda (pattern replacement subject)
					  (let ((m (string-match pattern subject)))
					    (if m
						(string-append (regexp-substitute #f m 'pre)
							       replacement
							       (regexp-substitute #f m 'post))
						subject)))))
		     (call-with-output-file #$output
		       (lambda (port)
			 (let ((final-file base-content))
			   (when #$frame-width
			     (set! final-file
				   (replace-first
				    "\\bframe_width\\b = [0-9]*"
				    (string-append
				     "frame_width = "
				     #$(number->string frame-width))
				    final-file)))
			   (when #$frame-color
			     (set! final-file
				   (replace-first
				    "\\bframe_color\\b = \"#[0-9a-fA-F]*\""
				    (string-append
				     "frame_color = \"" #$frame-color "\"")
				    final-file)))
			   (display final-file port))))))))))))))

;; (define-syntax-rule (dunst-config (base base-val) (configuration configuration-val))
;;   (home-dunst-configuration
;;    (base-config base-val)
;;    (config (home-dunst-extra-config
;;             (global (home-dunst-global-config
;;                      (frame-width (home-dunst-global-frame-width
;; 				   (home-dunst-global-config-data
;; 				    configuration-val)))))))))

(define home-dunst-service-type
  (service-type
   (name 'dunst)
   (description "Run the dunst notification daemon")
   (extensions
    (list
     (service-extension home-xdg-configuration-files-service-type
			home-dunst-files-service)
     (service-extension home-shepherd-service-type
			(lambda (config)
                          (list (shepherd-service
                                 (provision '(dunst))
                                 (start #~(make-forkexec-constructor
                                           (list #$(file-append
						    (home-dunst-package config)
						    "/bin/dunst"))))
                                 (stop #~(make-kill-destructor))))))))
   
   (compose concatenate)
   (extend (lambda (configuration extensions)
	     (if (null? extensions)
                 configuration
                 (home-dunst-configuration
                  (inherit configuration)
                  ;; Logic to merge extensions into your record goes here
                  (config (car extensions))))))))

(define runst-service
  (simple-service
   'runst home-shepherd-service-type
   (list
    (shepherd-service
     (documentation "Run the runst notification daemon")
     (requirement '(x11-display))
     (auto-start? #t)
     (provision '(runst))
     (start #~(make-forkexec-constructor
               (list #$(file-append runst "/bin/runst"))
	       #:log-file "runst.log"))
     (stop #~(make-kill-destructor))))))

