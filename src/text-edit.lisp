(in-package #:text-edit)

(defconstant +XRES+ 800)
(defconstant +YRES+ 600)

(defun init ()
  )

(defun paint ()
  )


(defun key-down (sym unicode mod)
  (case sym
    (:sdl-key-return    (buffer-newline))
    (:sdl-key-backspace (buffer-backspace))
    (:sdl-key-left      (buffer-cursor-left))
    (:sdl-key-right     (buffer-cursor-right))
    (:sdl-key-up        (doc-cursor-up))
    (:sdl-key-down      (doc-cursor-down))
    (t (buffer-append unicode))))

(defmacro on-key-do ((key-var) &body forms)
  (let* ((key-value-var (gensym "key-value-var"))
	 (cond-forms (loop for def in forms
			collect (let ((scancode (first def))		     
				      (then-run (rest def)))
				  `((sdl2:scancode= ,key-value-var ,scancode)
				    (progn ,@then-run))))))

    `(let ((,key-value-var (sdl2:scancode-value ,key-var)))
       (cond ,@cond-forms))))

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

	(doc-init)
	(doc-load (asdf:system-relative-pathname 'text-edit "data/palwarp.c"))
	
	(timer-reset-all)
	(font-init renderer)
	(cursor-init renderer)
	
	(app-repaint)
	(log-wr :info "entering main loop")

	(let ((last-time (sdl2:get-ticks)))
	  (sdl2:with-event-loop (:method :poll)
	    (:textinput (:text code-point)
			;;(log-wr :debug "text='~a'" text)
			(doc-text (string (code-char code-point)))
			)
	    
	    (:keydown (:keysym sym)
		      ;;(log-wr "keydown sym=~d" sym)
		      (on-key-do (sym)
			(:scancode-left      (doc-cursor-left))
			(:scancode-right     (doc-cursor-right))
			(:scancode-down      (doc-cursor-down))
			(:scancode-up        (doc-cursor-up))
			(:scancode-backspace (doc-backspace))
			(:scancode-return    (doc-return))))
	    
	    (:quit ()
		   t)
	    
	    (:idle ()
		   (let ((time-now (sdl2:get-ticks)))
		     (timer-tick-all (* (- time-now last-time) 0.001))
		     (setf last-time time-now))
		   
		   (app-when-repaint ()
		     ;;(paint)
		     (sdl2:set-render-draw-color renderer 0 0 0 255)
		     (sdl2:render-clear renderer)
		     
		     (font-color 255 255 255)
		     (sdl2:set-render-draw-color renderer 255 255 255 255)
		     (doc-draw)

		     (cursor-draw)
		     (sdl2:render-present renderer)))
	    
	    ))))))

;; (sdl2:quit)
