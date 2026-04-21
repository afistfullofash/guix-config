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

(defvar *net-wm-window-opacity-max* #xffffffff
  "Maximum _NET_WM_WINDOW_OPACITY value as defined by EWMH.")

(defun opacity-percent->cardinal (opacity)
  "Convert OPACITY percentage in [0, 100] to a _NET_WM_WINDOW_OPACITY CARDINAL/32 value."
  (check-type opacity (real 0 100))
  (round (* *net-wm-window-opacity-max* (/ opacity 100.0))))

(defun set-window-opacity (window opacity)
  "Set WINDOW opacity percentage to OPACITY using _NET_WM_WINDOW_OPACITY.
Requires a compositor that honors this EWMH property."
  (check-type window stumpwm:window)
  (handler-case
      (progn
	(log-message "trace" "set-window-opacity:~%    opacity: ~a~%    window: ~A~%" opacity window)
	(xlib:change-property (stumpwm:window-xwin window)
			      :_NET_WM_WINDOW_OPACITY
			      (list (opacity-percent->cardinal opacity))
			      :cardinal 32))
    (condition (c)
	       (log-message "trace" "~%~%~%set-window-opacity: handler-case-condition:~%~A~%~%~%" c)))
  window)

(defun clear-window-opacity (window)
  "Remove WINDOW's _NET_WM_WINDOW_OPACITY property."
  (check-type window stumpwm:window)
  (log-message "trace" "clear-window-opacity: ~A~%" window)
  (xlib:delete-property (stumpwm:window-xwin window) :_NET_WM_WINDOW_OPACITY)
  window)

(defun map-all-windows (fn &key (screen (stumpwm:current-screen)))
  "Call FN on each window across all groups on SCREEN."
  (dolist (g (stumpwm:screen-groups screen))
    (dolist (w (stumpwm:group-windows g))
      (funcall fn w))))

(defun get-all-visible-windows (&key (screen (stumpwm:current-screen)))
  (reduce
   'cons
   (mapcar
    (lambda (group)
      (remove-if-not
       (lambda (window)
	 (log-message "trace" "get-all-visible-windows: checking: ~A~%" window)
	 (if (stumpwm:window-visible-p window)
	     (progn
	       (log-message "trace" "get-visible-windows: visible~%~%")
	       t)
	     (progn
	       (log-message "trace" "get-visible-windows: invisible~%~%")
	       nil)))
       (stumpwm:group-windows group)))
    (stumpwm:screen-groups screen))))

(defun dim-window (window)
  (set-window-opacity window *window-dim-opacity*)
  (xlib:display-finish-output stumpwm:*display*))

(defun dim-all-windows ()
  (let ((visible-windows (get-all-visible-windows)))
    (mapcar (lambda (window)
	      (log-message "trace" "dim-all-windows: window: ~A~%~%" window)
	      (dim-window window))
	    visible-windows)))

(defun undim-window (window)
  (clear-window-opacity window)
  (xlib:display-finish-output stumpwm:*display*))

(defun undim-all-windows ()
  (let ((visible-windows (get-all-visible-windows)))
    (mapcar
     (lambda (window)
       (log-message "trace" "undim-all-windows: window: ~A~%~%" window)
       (undim-window window))
     visible-windows)))

(defun all-locks-held-p ()
  (or *window-dimming-lock* *window-undimming-lock*))

(defmacro with-lock ((lock-var) &body body)
  `(if ,lock-var
       (progn (log-message "trace"
			   "with-lock: ~A already held~%" ',lock-var) nil)
       (unwind-protect
            (progn (setf ,lock-var t)
                   ,@body)
         (setf ,lock-var nil))))

(defun dim-window-on-stumpwm-message (&rest _lines)
  (declare (ignorable _lines))
  (log-message "trace" "dim-window-on-stumpwm-message: begin~%")
  (if (not (all-locks-held-p))
      (with-lock (*window-dimming-lock*)
		 (log-message "trace" "dim-window-on-stumpwm-message: alowed to dim~%~%")
		 (dim-all-windows))
      (log-message "trace" "dim-window-on-stumpwm-message: unable to dim lock held~%~%")))

(declaim (ftype function set-undimming-timer))

(defun undimming-timer ()
  (log-message "trace" "undimming-timer: begin~%")
  (setf *window-undim-timer* nil)
  (handler-case 
      (cond
       ;; Undimm the windows if no locks are held
       ((not (all-locks-held-p))
	(with-lock (*window-undimming-lock*)
		   (log-message "trace" "undimming-timer: allowed to undim~%")
		   (undim-all-windows)))
       ;; If the dimming lock is set we want to wait
       ;; Then run undimming once the dimming is done
       (*window-dimming-lock*
	(progn
	  (log-message "trace" "undimming-timer: resetting dimming timer~%")
	  (set-undimming-timer)))
       (t
	(progn
	  (log-message "trace" "undimming-timer: resetting dimming timer~%")
	  (set-undimming-timer))))
    (condition (c)
	       (log-message "trace" "undimmg-timer:~%undimmg-timer:~%undimmg-timer: ERROR~%undimmg-timer:~%~A~%" c))))

(defun set-undimming-timer ()
  (when *window-undim-timer*
    (log-message "trace" "set-undimming-timer: clearing dimming timer~%")
    (ignore-errors (stumpwm:cancel-timer *window-undim-timer*))
    (setf *window-undim-timer* nil))

  (log-message "trace" "set-undimming-timer: setting new timer~%")
  (let ((timer-to-run (stumpwm:run-with-timer *window-undim-timer-length*
				      nil
				      'undimming-timer)))
    (log-message "trace" "set-undimming-timer: timer: ~A~%" timer-to-run)
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
