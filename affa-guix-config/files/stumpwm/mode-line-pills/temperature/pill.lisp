(in-package :stumpwm-pill-temperature)

(export '(temperature-pill))

(defun ml-read-file-as-integer (path)
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil)
		      (when s
			(let ((line (read-line s nil)))
			  (if line
			      (parse-integer (string-trim '(#\Newline #\Space) line))
			    0))))
    (sb-int:simple-stream-error () 0)
    (file-error () 0)))

(defun ml-read-file-as-string (path)
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil)
		      (when s
			(let ((line (read-line s nil)))
			  (if line
			      (string-trim '(#\Newline #\Space) line)
			    "unknown"))))
    (sb-int:simple-stream-error () "err")
    (file-error () "err")))

(defun ml-hwmon-directories ()
  (directory "/sys/class/hwmon/hwmon*"))

(defun ml-temperature-files (hwmon-dir)
  (directory (merge-pathnames "temp*_input" hwmon-dir)))

(defun ml-read-temperatures ()
  (loop for hwmon in (ml-hwmon-directories)
  	for name-path = (merge-pathnames "name" hwmon)
  	for chip-name = (when (probe-file name-path)
  			  (ml-read-file-as-string name-path))
  	append
  	(loop for temp-file in (ml-temperature-files hwmon)
  	      for milli-c = (ml-read-file-as-integer temp-file)
  	      collect
  	      (list :chip chip-name
  		    :sensor (pathname-name temp-file)
  		    :celsius (/ milli-c 1000.0)))))

(defun ml-get-hottest ()
  (let ((hottest (list :celsius 0)))
    (dolist (entry (ml-read-temperatures))
      (if (> (getf entry :celsius) (getf hottest :celsius))
       	  (setf hottest entry)))
    hottest))

(defun ml-get-human-name (sensors)
  (let ((chip-name (getf sensors :chip)))
    (cond ((equal chip-name "mt7921_phy0") "Wi-Fi")
       	  ((equal chip-name "amdgpu") "GPU")
       	  ((equal chip-name "nvme") "SSD")
       	  ((equal chip-name "k10temp") "CPU")
  	  ((equal chip-name "coretemp") "CPU")
       	  ((equal chip-name "acpitz") "ACPI")
       	  (t chip-name))))

(stumpwm:add-screen-mode-line-formatter #\H 'temperature-pill)
(defun temperature-pill (ml)
  (declare (ignorable ml))
  (let ((hottest (ml-get-hottest)))
    (if (> (getf hottest :celsius) 70)
  	(stumpwm-mode-line:priority-by-range-pill
  	 (format nil " ~,1fC ~a "
  		 (getf hottest :celsius)
  		 (ml-get-human-name hottest))
  	 (getf hottest :celsius) 70 80 90)
      "")))
