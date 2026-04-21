(in-package :stumpwm-logging)

(defvar *logging-levels*
  (list "fatal"
	"error"
	"warn"
	"info"
	"debug"
	"trace"))

(defvar *original-logging-streams*
  (list :error stumpwm::*error-output*
	:standard stumpwm::*standard-output*
	:trace stumpwm::*trace-output*
	:debug stumpwm::*debug-stream*))

(defvar *log-formatter* 'file-logger)

(defparameter *stumpwm-debug-dir*
	      (let ((dir (merge-pathnames (stumpwm-utils:xdg-state-home) "stumpwm/")))
		(if (stumpwm-utils:mkdir-p dir)
		    dir
		  (progn
		    (stumpwm:message (format nil "Logging: Failed to use logging directory~%~a" dir)
				     dir)))))

(defparameter *stumpwm-log-file*
  (merge-pathnames *stumpwm-debug-dir* "stumpwm.log"))

(defparameter *stumpwm-oplog-file*
  (merge-pathnames *stumpwm-debug-dir* "stumpwm-events.log"))

(defun logging-to-debug-mapping (level)
  (cond ((string= level "fatal") 1)
	((string= level "error") 2)
	((string= level "warn") 3)
	((string= level "info") 4)
	((string= level "debug") 5)
	((string= level "trace") 6)))

(defun logging-enabled-for-level (level)
  (cond ((and (string= level "fatal")
	      (eq stumpwm:*debug-level* 1))
	 t)
	((and (string= level "error")
	      (eq stumpwm:*debug-level* 2))
	 t)
	((and (string= level "warn")
	      (eq stumpwm:*debug-level* 3))
	 t)
	((and (string= level "info")
	      (eq stumpwm:*debug-level* 4))
	 t)
	((and (string= level "debug")
	      (eq stumpwm:*debug-level* 5))
	 t)
	((and (string= level "trace")
	      (eq stumpwm:*debug-level* 6))
	 t)
	(t nil)))

(defun can-log (level)
  (and (stumpwm:minor-mode-enabled-p 'logging-minor-mode)
       (logging-enabled-for-level level)))

(defun file-logger (level log-line stream &rest extra-data)
  "A default formatter for the logger

This will output in the form:
12-32-05: info: stuff other stuff"
  (format stream
	  "~&~A: ~a: ~a "
	  (local-time:format-timestring nil (local-time:now))
	  level
	  log-line)
  (apply #'format stream "~a" extra-data))

(defun message-logger (level log-line stream &rest extra-data)
  (declare (ignorable stream))
  (let* ((fmt (format nil
		      "~a:  ~a"
		      level
		      log-line))
	 (args (apply #'format nil fmt extra-data))
	 (message-string (reduce (lambda (arg acc)
				   (concatenate 'string acc arg))
				 args)))
    (stumpwm:message message-string)))

(defun log-message (level fmt &rest args)
  (when (can-log level)
    (with-open-file (s *stumpwm-oplog-file*
		       :direction :output :if-does-not-exist :create :if-exists :append)
      (funcall *log-formatter* level fmt s args)
      (finish-output s))))

(defun enable-logging (&optional (level "info"))
  (if (not (member level *logging-levels* :test 'string=))
      (stumpwm:message "Logging Level not set to a valid option"))

  (setf *original-logging-streams*
	(list :error stumpwm::*error-output*
	      :standard stumpwm::*standard-output*
	      :trace stumpwm::*trace-output*
	      :debug stumpwm::*debug-stream*))
  
  (setf stumpwm:*debug-level* (logging-to-debug-mapping level))
  ;; Redirect *everything* to a file (stdout/stderr included).
  (stumpwm:redirect-all-output *stumpwm-log-file*))

(defun disable-logging ()
  (setf stumpwm::*error-output* (getf *original-logging-streams* :error)
	stumpwm::*standard-output* (getf *original-logging-streams* :standard)
	stumpwm::*trace-output* (getf *original-logging-streams* :trace)
	stumpwm::*debug-stream* (getf *original-logging-streams* :debug)))

(stumpwm-utils:define-minor-mode-safe
    (stumpwm:define-minor-mode logging-minor-mode () ()
      (:scope :unscoped)
      (:make-hooks t)))

(stumpwm:add-hook *logging-minor-mode-hook*
		  (lambda (args)
		    (declare (ignorable args))
		    (enable-logging)))

(stumpwm:add-hook *logging-minor-mode-destroy-hook*
		  (lambda (args)
		    (declare (ignorable args))
		    (disable-logging)))
