;; Compositor
;; Things like window transarency
(in-package :stumpwm-compositor)

(defparameter *affoa-window-dim-opacity* 20)   ; 1–100

(defvar *affoa-window-undim-timer* nil)
(defvar *affoa-window-undim-timer-length* 1)

(defvar *affoa-window-dimming-lock* nil)
(defvar *affoa-window-undimming-lock* nil)

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

(defun affoa-get-all-visible-windows (&key (screen (current-screen)))
  (reduce
   'cons
   (mapcar
    (lambda (group)
      (remove-if-not
       (lambda (window)
	 (log-message "affoa-get-all-visible-windows: checking: ~A~%" window)
	 (if (window-visible-p window)
	     (progn
	       (log-message "affoa-get-visible-windows: visible~%~%")
	       t)
	     (progn
	       (log-message "affoa-get-visible-windows: invisible~%~%")
	       nil)))
       (group-windows group)))
    (screen-groups screen))))

(defun affoa-dim-window (window)
  (set-window-opacity window *affoa-window-dim-opacity*)
  (xlib:display-finish-output *display*))

(defun affoa-dim-all-windows ()
  (let ((visible-windows (affoa-get-all-visible-windows)))
    (mapcar (lambda (window)
	      (log-op "affoa-dim-all-windows: window: ~A~%~%" window)
	      (affoa-dim-window window))
	    visible-windows)))

(defun affoa-undim-window (window)
  (clear-window-opacity window)
  (xlib:display-finish-output *display*))

(defun affoa-undim-all-windows ()
  (let ((visible-windows (affoa-get-all-visible-windows)))
    (mapcar
     (lambda (window)
       (log-op "affoa-undim-all-windows: window: ~A~%~%" window)
       (affoa-undim-window window))
     visible-windows)))

(defun all-locks-held-p ()
  (or *affoa-window-dimming-lock* *affoa-window-undimming-lock*))

(defmacro with-lock ((lock-var) &body body)
  `(if ,lock-var
       (progn (log-op "with-lock: ~A already held~%" ',lock-var) nil)
       (unwind-protect
            (progn (setf ,lock-var t)
                   ,@body)
         (setf ,lock-var nil))))

(defun affoa-dim-window-on-stumpwm-message (&rest _lines)
  (declare (ignorable _lines))
  (log-op "affoa-dim-window-on-stumpwm-message: begin~%")
  (if (not (all-locks-held-p))
      (with-lock (*affoa-window-dimming-lock*)
		 (log-op "affoa-dim-window-on-stumpwm-message: alowed to dim~%~%")
		 (affoa-dim-all-windows))
      (log-op "affoa-dim-window-on-stumpwm-message: unable to dim lock held~%~%")))

(declaim (ftype function affoa-set-undimming-timer))

(defun affoa-undimming-timer ()
  (log-op "affoa-undimming-timer: begin~%")
  (setf *affoa-window-undim-timer* nil)
  (handler-case 
      (cond
       ;; Undimm the windows if no locks are held
       ((not (all-locks-held-p))
	(with-lock (*affoa-window-undimming-lock*)
		   (log-op "affoa-undimming-timer: allowed to undim~%")
		   (affoa-undim-all-windows)))
       ;; If the dimming lock is set we want to wait
       ;; Then run undimming once the dimming is done
       (*affoa-window-dimming-lock*
	(progn
	  (log-op "affoa-undimming-timer: resetting dimming timer~%")
	  (affoa-set-undimming-timer)))
       (t
	(progn
	  (log-op "affoa-undimming-timer: resetting dimming timer~%")
	  (affoa-set-undimming-timer))))
    (condition (c)
	       (log-op "affoa-undimmg-timer:~%affoa-undimmg-timer:~%affoa-undimmg-timer: ERROR~%affoa-undimmg-timer:~%~A~%" c))))

(defun affoa-set-undimming-timer ()
  (when *affoa-window-undim-timer*
    (log-op "affoa-set-undimming-timer: clearing dimming timer~%")
    (ignore-errors (cancel-timer *affoa-window-undim-timer*))
    (setf *affoa-window-undim-timer* nil))

  (log-op "affoa-set-undimming-timer: setting new timer~%")
  (let ((timer-to-run (run-with-timer *affoa-window-undim-timer-length*
				      nil
				      'affoa-undimming-timer)))
    (log-op "affoa-set-undimming-timer: timer: ~A~%" timer-to-run)
    (setf *affoa-window-undim-timer*
	  timer-to-run)))

(defun affoa-undim-all-windows-on-stumpwm-message-removal (&rest args)
  (declare (ignorable args))
  (affoa-undimming-timer))

(defun affoa-toggle-window-dimming ()
  (if (find #'affoa-dim-window-on-stumpwm-message stumpwm:*message-hook*)
      (progn
	(affoa-undim-all-windows)
	(stumpwm:remove-hook stumpwm:*message-hook* #'affoa-dim-window-on-stumpwm-message)
	(stumpwm:remove-hook stumpwm:*message-hide-hook* #'affoa-undim-all-windows-on-stumpwm-message-removal))
      (progn
	(stumpwm:add-hook stumpwm:*message-hook* #'affoa-dim-window-on-stumpwm-message)
	(stumpwm:add-hook stumpwm:*message-hide-hook* #'affoa-undim-all-windows-on-stumpwm-message-removal))))

(affoa-toggle-window-dimming)
