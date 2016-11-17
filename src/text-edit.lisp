(in-package #:text-edit)

(defconstant +XRES+ 800)
(defconstant +YRES+ 600)

(defparameter *mod-shift* nil)

(defun init (renderer)
  (doc-init)
  (doc-load (asdf:system-relative-pathname 'text-edit "data/palwarp.c"))

  (setf *mod-shift* nil)
  
  (timer-reset-all)
  (font-init renderer)
  (cursor-init renderer)
  
  (app-repaint))

(defun paint (renderer)
  (app-when-repaint ()
    
    (sdl2:set-render-draw-color renderer 0 0 0 255)
    (sdl2:render-clear renderer)
    
    (font-color 255 255 255)
    (view-draw *doc-lines*)

    (cursor-draw)
    (sdl2:render-present renderer)))

(defmacro on-key-do ((key-var) &body forms)
  (let* ((key-value-var (gensym "key-value-var"))
	 (cond-forms (loop for def in forms
			collect (let ((scancode (first def))		     
				      (then-run (rest def)))
				  `((sdl2:scancode= ,key-value-var ,scancode)
				    (progn ,@then-run))))))

    `(let ((,key-value-var (sdl2:scancode-value ,key-var)))
       (cond ,@cond-forms))))

(defun key-down (sym)
  (on-key-do (sym)
    (:scancode-left      (cursor-move-left  *mod-shift*))
    (:scancode-right     (cursor-move-right *mod-shift*))
    (:scancode-down      (cursor-move-vertical +CURSOR-GO-DOWN+ *mod-shift*))
    (:scancode-up        (cursor-move-vertical +CURSOR-GO-UP+   *mod-shift*))
    (:scancode-backspace (cursor-backspace))
    (:scancode-return    (cursor-newline))
    (:scancode-lshift    (setf *mod-shift* t) (cursor-select-begin))
    (:scancode-rshift    (setf *mod-shift* t) (cursor-select-begin))
    (:scancode-escape    (cursor-select-finish))
    (:scancode-delete    (cursor-delete))
    (:scancode-home      (cursor-move-home *mod-shift*))
    (:scancode-end       (cursor-move-end  *mod-shift*))))

(defun key-up (sym)
  (on-key-do (sym)
    (:scancode-lshift (setf *mod-shift* nil))
    (:scancode-rshift (setf *mod-shift* nil))))

(defun main ()
  "entry point. run this."

  (log-init)
  (log-wr :info "text edit started")
  
  (sdl2:with-init (:video :events)
    
    (sdl2:with-window (window :w +XRES+
			      :h +YRES+
			      :title "SDL window"
			      :flags '(:shown))

      (sdl2:with-renderer (renderer window)

	(init renderer)
	(log-wr :info "entering main loop")

	(let ((last-time (sdl2:get-ticks)))
	  (sdl2:with-event-loop (:method :poll)
	    (:textinput (:text code-point)
			;;(log-wr :debug "text='~a'" text)
			(cursor-insert-text (string (code-char code-point)))
			)
	    
	    (:keydown (:keysym sym) (key-down sym))
	    (:keyup   (:keysym sym) (key-up   sym))
	    
	    (:quit ()
		   t)
	    
	    (:idle ()
		   (let ((time-now (sdl2:get-ticks)))
		     (timer-tick-all (* (- time-now last-time) 0.001))
		     (setf last-time time-now))

		   (paint renderer))
	    
	    ))))))

;; (sdl2:quit)
