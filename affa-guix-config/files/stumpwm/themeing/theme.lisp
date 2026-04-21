 ;; ** Themeing
;; This is a quick themeing system for stumpwm with a couple of theme options.

;; *** Basic StumpWM Themeing Sytem 
;; This gives us some very general functions to generate and apply /themes/ with.
(in-package :stumpwm-themeing)

;; Visual
(defvar *themes* '())
(defvar *current-theme* 'default)

(defun add-theme (name theme)
  (setf (getf *themes* name) theme))

(defun get-theme (name)
  (getf *themes* name))

      ;;; Colors based off spacemacs-dark-theme for emacs
(defclass theme ()
  ((name
    :initarg :name
    :initform "Undefined"
    :accessor :name
    :type string)
   (fg
    :initarg :fg
    :initform "White"
    :accessor theme-fg
    :type string)
   (bg
    :initarg :bg
    :initform "Black"
    :accessor theme-bg
    :type string)

   (border
    :initarg :border
    :initform "White"
    :accessor theme-border
    :type string)

   (focus
    :initarg :focus
    :initform "White"
    :accessor theme-focus
    :type string)
   (unfocus
    :initarg :unfocus
    :initform "Black"
    :accessor theme-unfocus
    :type string)

   (mode-line-fg
    :initarg :mode-line-fg
    :initform stumpwm:*mode-line-foreground-color*
    :accessor theme-mode-line-fg
    :type string)
   (mode-line-bg
    :initarg :mode-line-bg
    :initform stumpwm:*mode-line-background-color*
    :accessor theme-mode-line-bg
    :type string)
   (mode-line-border
    :initarg :mode-line-border
    :initform stumpwm:*mode-line-border-color*
    :accessor theme-mode-line-border
    :type string)
   
   ;; These are the default colors of *colors* in order
   (black
    :initarg :black
    :initform '("black" "black")
    :accessor theme-black
    :type list)
   (red
    :initarg :red
    :initform '("red" "red")
    :accessor theme-red
    :type list)
   (green
    :initarg :green
    :initform '("green" "green")
    :accessor theme-green
    :type list)
   (yellow
    :initarg :yellow
    :initform '("yellow" "yellow")
    :accessor theme-yellow
    :type list)
   (blue
    :initarg :blue
    :initform '("blue" "blue")
    :accessor theme-blue
    :type list)
   (magenta
    :initarg :magenta
    :initform '("magenta" "magenta")
    :accessor theme-magenta
    :type list)
   (cyan
    :initarg :cyan
    :initform '("cyan" "cyan")
    :accessor theme-cyan
    :type list)
   (white
    :initarg :white
    :initform '("white" "white")
    :accessor theme-white
    :type list)
   (custom-one
    :initarg :custom-one
    :initform '("black" "black")
    :accessor theme-custom-one
    :type list)
   (custom-two
    :initarg :custom-two
    :initform '("white" "white")
    :accessor theme-custom-two
    :type list)
   (low
    :initarg :low
    :accessor theme-low
    :type string)
   (medium
    :initarg :medium
    :accessor theme-medium
    :type string)
   (high
    :initarg :high
    :accessor theme-high
    :type string)
   (light-fg
    :initarg :light-fg
    :accessor theme-light-fg
    :type string)))

(defmethod initialize-instance :after ((theme theme) &key)
  ;; Default priorities to green, yellow, orange
  ;; Set Low to green
  (unless (slot-boundp theme 'low)
    (setf (slot-value theme 'low)
	  (car (slot-value theme 'green))))
  ;; Medium to yellow
  (unless (slot-boundp theme 'medium)
    (setf (slot-value theme 'medium)
	  (car (slot-value theme 'yellow))))
  ;; High to red
  (unless (slot-boundp theme 'high)
    (setf (slot-value theme 'high)
	  (car (slot-value theme 'red))))
  ;; Light foreground to fg
  (unless (slot-boundp theme 'light-fg)
    (setf (slot-value theme 'light-fg)
	  (slot-value theme 'fg))))

(defmethod print-object ((theme theme) stream)
      (print-unreadable-object (theme stream :type t)
        (with-slots (name
			 fg
			 bg
			 border
			 focus
			 unfocus
			
			 mode-line-fg
			 mode-line-bg
			 mode-line-border

			 black
			 red
			 green
			 yellow
			 blue
			 magenta
			 cyan
			 white
			
			 custom-one
			 custom-two)
            theme
          (format stream "~a~% fg: ~a~% bg: ~a~% border: ~a~% focus: ~a~% unfocus: ~a~% mode-line-fg: ~a~% mode-line-bg: ~a~% mode-line-border: ~a~% black: ~a~% red: ~a~% green: ~a~% yellow: ~a~% blue: ~a~% magenta: ~a~% cyan: ~a~% white: ~a~% custom-one: ~a~% custom-two: ~a~%" name fg bg border focus unfocus mode-line-fg mode-line-bg mode-line-border black red green yellow blue magenta cyan white custom-one custom-two))))

(defgeneric get-color (theme color)
  (:documentation "Get the color value for a given theme color"))

(defmethod get-color ((theme theme) color)
	   (ecase color
		  (:fg (theme-fg theme))
		  (:bg (theme-bg theme))
		  
		  (:border (theme-border theme))
		  (:focus  (theme-focus theme))
		  (:unfocus (theme-unfocus theme))
		  
		  (:mode-line-fg (theme-mode-line-fg theme))
		  (:mode-line-bg (theme-mode-line-bg theme))
		  (:mode-line-border (theme-mode-line-border theme))
		  
		  (:black (theme-black theme))
		  (:red (theme-red theme))
		  (:green (theme-green theme))
		  (:yellow (theme-yellow theme))
		  (:blue (theme-blue theme))
		  (:magenta (theme-magenta theme))
		  (:cyan (theme-cyan theme))
		  (:white (theme-white theme))
		  
		  (:custom-one (theme-custom-one theme))
		  (:custom-two (theme-custom-two theme))
		  
		  (:low (theme-low theme))
		  (:medium (theme-medium theme))
		  (:high (theme-high theme))
		  (:light-fg (theme-light-fg theme))))

(defgeneric set-key-seq-color (theme)
  (:documentation "Set the key sequence color"))

(defmethod set-key-seq-color ((theme theme))
	   (let ((key-seq-color (slot-value theme 'focus)))
	     (setf stumpwm:*key-seq-color* (foreground-color key-seq-color))
	     (setf stumpwm:*which-key-format*
		   (stumpwm:concat (foreground-color key-seq-color) "~5a^n ~a"))
	     key-seq-color))

(defgeneric apply-theme (theme)
	    (:documentation "Apply a themes color scheme to StumpWM"))

(defmethod apply-theme ((theme theme))
  (with-slots (fg
	       bg
	       border
	       focus
	       unfocus
	       
	       mode-line-fg
	       mode-line-bg
	       mode-line-border

	       black
	       red
	       green
	       yellow
	       blue
	       magenta
	       cyan
	       white
	       
	       custom-one
	       custom-two) theme
    (progn
      (stumpwm:set-fg-color fg)
      (stumpwm:set-bg-color bg)
      (stumpwm:set-border-color border)
      (stumpwm:set-focus-color focus)
      (stumpwm:set-unfocus-color unfocus)

      (setf stumpwm:*mode-line-foreground-color* mode-line-fg
	    stumpwm:*mode-line-background-color* mode-line-bg
	    stumpwm:*mode-line-border-color* mode-line-border)
      
      (setf stumpwm:*colors*
	    (list black
		  red
		  green
		  yellow
		  blue
		  magenta
		  cyan
		  white
		  custom-one
		  custom-two))))
  (stumpwm:update-color-map (stumpwm:current-screen))
  (stumpwm-utils:reload-modeline)
  (set-key-seq-color theme)
  
  t)

(defun set-theme (theme)
  (apply-theme (get-theme :default))
  (setf *current-theme* :default)
  (apply-theme (get-theme theme))
  (setf *current-theme* theme))

(defun with-current-theme ()
  (get-theme *current-theme*))
