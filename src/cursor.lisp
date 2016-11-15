(in-package :text-edit)

(defparameter *cursor-width* 9)
(defparameter *cursor-height* 16)
(defparameter *cursor-on* nil)
(defparameter *cursor-renderer* nil)

(defconstant +CURSOR-GO-UP+    -1)
(defconstant +CURSOR-GO-DOWN+   1)

(defstruct cursor
  (line-number 0 )
  (char-number 0))

(defparameter *current-cursor* nil)

(defmacro cursor-current-line-number ()
  `(cursor-line-number *current-cursor*))

(defmacro cursor-current-char-number ()
  `(cursor-char-number *current-cursor*))

(defun cursor-init (renderer)
  (setf *cursor-renderer* renderer
	*current-cursor* (make-cursor))
  
  (with-timer (:cursor-blink 0.5)
    (setf *cursor-on* (not *cursor-on*))
    (app-repaint)))

(defun cursor-draw ()
  (when *cursor-on*
    (let ((rect (sdl2:make-rect (* (cursor-current-char-number) *cursor-width*)
				(* (- (cursor-current-line-number)
				      *view-starts-at-pos*)
				   *cursor-height*)
				*cursor-width*
				*cursor-height*)))

      (sdl2:set-render-draw-color *cursor-renderer*
				  255
				  255
				  255
				  255)
      (sdl2:render-fill-rect *cursor-renderer* rect))))

;; oops

;;
;; editing
;;


(defun cursor-move-vertical (delta)
  (log-wr :info "cursor-move-vertical delta=~d" delta)

  (let ((new-line-number (+ (cursor-current-line-number) delta)))
    (when (doc-valid-line? new-line-number)
      
      (setf (cursor-current-line-number) new-line-number)

      (view-adjust-for-cursor-line new-line-number)
      
      (doc-setup-cursor-line new-line-number)
      
      (app-repaint))))

(defun cursor-move-left ()
  (log-wr :info "cursor-move-left")

  (if (buffer-cursor-at-start? (cursor-current-char-number))
      (progn
	(cursor-move-vertical +CURSOR-GO-UP+)
	(setf (cursor-current-char-number) (buffer-length)))
      (progn
	(decf (cursor-current-char-number))
	(app-repaint))))


(defun cursor-move-right ()
  (log-wr :info "cursor-move-right")
  
  (if (buffer-cursor-at-end? (cursor-current-char-number))
      (progn
	(cursor-move-vertical +CURSOR-GO-DOWN+)
	(setf (cursor-current-char-number) 0))
      (progn
	(incf (cursor-current-char-number))
	(app-repaint))))

(defun cursor-move-home ()
  (setf (cursor-current-char-number) 0)
  (app-repaint))

(defun cursor-move-end ()
  (setf (cursor-current-char-number) (buffer-length))
  (app-repaint))

(defun cursor-insert-text (text)
  (doc-insert-text (cursor-current-line-number)
		   (cursor-current-char-number)
		   text)
  (incf (cursor-current-char-number))
  ;;(select-clear)
  (app-repaint))

(defun cursor-backspace ()
  (if (buffer-cursor-at-start? (cursor-current-char-number))
      (when (> (cursor-current-line-number) 0)
	(let ((line (first (let ((pos (cursor-current-line-number)))
			     (doc-cut-lines pos (1+ pos))))))
	  (decf (cursor-current-line-number))
	  (doc-setup-cursor-line (cursor-current-line-number))
	  (cursor-move-end)
	  (doc-insert-text (cursor-current-line-number)
			   (buffer-length)
			   line)
	  (app-repaint)))

      ;; else
      (progn
	
	(doc-backspace (cursor-current-line-number)
		       (cursor-current-char-number))
	
	(decf (cursor-current-char-number))
	(app-repaint))))

(defun cursor-delete ()

  (if (buffer-cursor-at-end? (cursor-current-char-number))
      (when (< (cursor-current-line-number) (doc-length))

	(let ((line (let ((pos (1+ (cursor-current-line-number))))
		      (first
		       (doc-cut-lines pos (1+ pos))))))
	  
	  (doc-insert-text (cursor-current-line-number)
			   (buffer-length)
			   line)
	  (app-repaint)))

      ;; else
      (progn
	

	(doc-backspace (cursor-current-line-number)
		       (1+ (cursor-current-char-number)))
	
	(app-repaint))))


(defun cursor-newline ()
  (log-wr :info "cursor-newline")
  (let ((new-line (doc-split-to-end (cursor-current-line-number)
				    (cursor-current-char-number))))
    
    (doc-insert-lines (1+ (cursor-current-line-number)) new-line)    
    (cursor-move-vertical +CURSOR-GO-DOWN+)
    (cursor-move-home)))

