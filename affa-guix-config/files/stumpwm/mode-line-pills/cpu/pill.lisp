(in-package :stumpwm-pill-cpu)

(stumpwm:add-screen-mode-line-formatter #\C 'cpu-pill)
(defun cpu-pill (ml)
  (declare (ignorable ml))
  (let ((cpu-percent (* (cpu::current-cpu-usage) 100)))
    (if (> 70 cpu-percent)
  	(stumpwm-mode-line:priority-by-range-pill
  	 (format nil " ~,1f%% CPU " cpu-percent)
  	 cpu-percent 70 80 90))
    ""))
