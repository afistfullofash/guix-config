;; Compositor
;; Things like window transarency
(in-package :stumpwm-compositor)

(export '(toggle-window-dimming
	  undim-all-windows-on-stumpwm-message-removal
	  dim-window-on-stumpwm-message))

(defparameter *window-dim-opacity* 20)   ; 1–100

(defvar *window-undim-timer* nil)
(defvar *window-undim-timer-length* 1)

(defvar *window-dimming-lock* nil)
(defvar *window-undimming-lock* nil)

(defconstant +net-wm-window-opacity-max+ #xffffffff
  "Maximum _NET_WM_WINDOW_OPACITY value as defined by EWMH.")

(defun opacity-percent->cardinal (opacity)
  "Convert OPACITY percentage in [0, 100] to a _NET_WM_WINDOW_OPACITY CARDINAL/32 value."
  (check-type opacity (real 0 100))
  (round (* +net-wm-window-opacity-max+ (/ opacity 100.0))))

(defun set-window-opacity (window opacity)
  "Set WINDOW opacity percentage to OPACITY using _NET_WM_WINDOW_OPACITY.
Requires a compositor that honors this EWMH property."
  (check-type window window)
  (handler-case
      (progn
	(log-message "info" "set-window-opacity:~%    opacity: ~a~%    window: ~A~%" opacity window)
	(xlib:change-property (window-xwin window)
			      :_NET_WM_WINDOW_OPACITY
			      (list (opacity-percent->cardinal opacity))
			      :cardinal 32))
    (condition (c)
      (log-message "info" "~%~%~%set-window-opacity: handler-case-condition:~%~A~%~%~%" c)))
  window)

(defun clear-window-opacity (window)
  "Remove WINDOW's _NET_WM_WINDOW_OPACITY property."
  (check-type window window)
  (log-message "info" "clear-window-opacity: ~A~%" window)
  (xlib:delete-property (window-xwin window) :_NET_WM_WINDOW_OPACITY)
  window)

(defun map-all-windows (fn &key (screen (current-screen)))
  "Call FN on each window across all groups on SCREEN."
  (dolist (g (screen-groups screen))
    (dolist (w (group-windows g))
      (funcall fn w))))

(defun get-all-visible-windows (&key (screen (current-screen)))
  (reduce
   'cons
   (mapcar
    (lambda (group)
      (remove-if-not
       (lambda (window)
	 (log-message "get-all-visible-windows: checking: ~A~%" window)
	 (if (window-visible-p window)
	     (progn
	       (log-message "get-visible-windows: visible~%~%")
	       t)
	     (progn
	       (log-message "get-visible-windows: invisible~%~%")
	       nil)))
       (group-windows group)))
    (screen-groups screen))))

(defun dim-window (window)
  (set-window-opacity window *window-dim-opacity*)
  (xlib:display-finish-output *display*))

(defun dim-all-windows ()
  (let ((visible-windows (get-all-visible-windows)))
    (mapcar (lambda (window)
	      (log-message "dim-all-windows: window: ~A~%~%" window)
	      (dim-window window))
	    visible-windows)))

(defun undim-window (window)
  (clear-window-opacity window)
  (xlib:display-finish-output *display*))

(defun undim-all-windows ()
  (let ((visible-windows (get-all-visible-windows)))
    (mapcar
     (lambda (window)
       (log-message "undim-all-windows: window: ~A~%~%" window)
       (undim-window window))
     visible-windows)))

(defun all-locks-held-p ()
  (or *window-dimming-lock* *window-undimming-lock*))

(defmacro with-lock ((lock-var) &body body)
  `(if ,lock-var
       (progn (log-message "with-lock: ~A already held~%" ',lock-var) nil)
       (unwind-protect
            (progn (setf ,lock-var t)
                   ,@body)
         (setf ,lock-var nil))))

(defun dim-window-on-stumpwm-message (&rest _lines)
  (declare (ignorable _lines))
  (log-message "dim-window-on-stumpwm-message: begin~%")
  (if (not (all-locks-held-p))
      (with-lock (*window-dimming-lock*)
		 (log-message "dim-window-on-stumpwm-message: alowed to dim~%~%")
		 (dim-all-windows))
      (log-message "dim-window-on-stumpwm-message: unable to dim lock held~%~%")))

(declaim (ftype function set-undimming-timer))

(defun undimming-timer ()
  (log-message "undimming-timer: begin~%")
  (setf *window-undim-timer* nil)
  (handler-case 
      (cond
       ;; Undimm the windows if no locks are held
       ((not (all-locks-held-p))
	(with-lock (*window-undimming-lock*)
		   (log-message "undimming-timer: allowed to undim~%")
		   (undim-all-windows)))
       ;; If the dimming lock is set we want to wait
       ;; Then run undimming once the dimming is done
       (*window-dimming-lock*
	(progn
	  (log-message "undimming-timer: resetting dimming timer~%")
	  (set-undimming-timer)))
       (t
	(progn
	  (log-message "undimming-timer: resetting dimming timer~%")
	  (set-undimming-timer))))
    (condition (c)
	       (log-message "undimmg-timer:~%undimmg-timer:~%undimmg-timer: ERROR~%undimmg-timer:~%~A~%" c))))

(defun set-undimming-timer ()
  (when *window-undim-timer*
    (log-message "set-undimming-timer: clearing dimming timer~%")
    (ignore-errors (cancel-timer *window-undim-timer*))
    (setf *window-undim-timer* nil))

  (log-message "set-undimming-timer: setting new timer~%")
  (let ((timer-to-run (run-with-timer *window-undim-timer-length*
				      nil
				      'undimming-timer)))
    (log-message "set-undimming-timer: timer: ~A~%" timer-to-run)
    (setf *window-undim-timer*
	  timer-to-run)))

(defun undim-all-windows-on-stumpwm-message-removal (&rest args)
  (declare (ignorable args))
  (undimming-timer))

(defun toggle-window-dimming ()
  (if (find #'dim-window-on-stumpwm-message stumpwm:*message-hook*)
      (progn
	(undim-all-windows)
	(stumpwm:remove-hook stumpwm:*message-hook* #'dim-window-on-stumpwm-message)
	(stumpwm:remove-hook stumpwm:*message-hide-hook* #'undim-all-windows-on-stumpwm-message-removal))
      (progn
	(stumpwm:add-hook stumpwm:*message-hook* #'dim-window-on-stumpwm-message)
	(stumpwm:add-hook stumpwm:*message-hide-hook* #'undim-all-windows-on-stumpwm-message-removal))))
