(in-package :text-edit)

(defparameter *cursor-width* 9)
(defparameter *cursor-height* 16)
(defparameter *cursor-on* nil)
(defparameter *cursor-renderer* nil)

(defconstant +CURSOR-GO-UP+    -1)
(defconstant +CURSOR-GO-DOWN+   1)

(defstruct cursor
  (line-number 0)
  (char-number 0)
  (char-last-number 0))

(defparameter *current-cursor* nil)

(defmacro cursor-current-line-number ()
  `(cursor-line-number *current-cursor*))

(defmacro cursor-current-char-number ()
  `(cursor-char-number *current-cursor*))

(defun cursor-set-current-char-number (pos)
  (setf (cursor-current-char-number) pos
	(cursor-char-last-number *current-cursor*) pos))

(defun cursor-inc-current-char-number (delta)
  (let ((new-pos (incf (cursor-current-char-number) delta)))
    (setf (cursor-char-last-number *current-cursor*) new-pos)))

  
(defun cursor-init (renderer)
  (setf *cursor-renderer* renderer
	*current-cursor* (make-cursor))
  
  (with-timer (:cursor-blink 0.5)
    (setf *cursor-on* (not *cursor-on*))
    (app-repaint)))

(defun cursor-draw ()
  (when *cursor-on*
    (let ((rect (sdl2:make-rect (* (+ (cursor-current-char-number) 4) *cursor-width*)
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

(defun cursor-update-select (mod-shift)
  (if mod-shift
      (if (select-active?)
	  (select-update *current-cursor*))
      
      (select-finish)))

;; oops

;;
;; editing
;;

(defun cursor-move-home (&optional mod-shift)
  (setf (cursor-current-char-number) 0)
  (cursor-update-select mod-shift)
  (app-repaint))

(defun cursor-move-end (&optional mod-shift)
  (setf (cursor-current-char-number) (buffer-length))
  (cursor-update-select mod-shift)
  (app-repaint))

(defun cursor-move-vertical (delta &optional mod-shift)
  (log-wr :info "cursor-move-vertical delta=~d" delta)

  (let ((new-line-number (+ (cursor-current-line-number) delta)))
    (when (doc-valid-line? new-line-number)
      
      (setf (cursor-current-line-number) new-line-number)

      (view-adjust-for-cursor-line new-line-number)
      
      (doc-setup-cursor-line new-line-number)

      (let ((char-num (cursor-char-last-number *current-cursor*))
	    (buflen (buffer-length)))

	(setf (cursor-current-char-number)      	
	      (if (< buflen char-num) buflen char-num)))
      
      (cursor-update-select mod-shift)
      (app-repaint))))

(defun cursor-move-left (mod-shift)
  (log-wr :info "cursor-move-left")

  (if (buffer-cursor-at-start? (cursor-current-char-number))
      (progn
	(cursor-move-vertical +CURSOR-GO-UP+ mod-shift)
	(cursor-move-end mod-shift))
      (progn
	(cursor-inc-current-char-number -1)
	(app-repaint)))
  
  (cursor-update-select mod-shift))



(defun cursor-move-right (mod-shift)
  (log-wr :info "cursor-move-right")
  
  (if (buffer-cursor-at-end? (cursor-current-char-number))
      (progn
	(cursor-move-vertical +CURSOR-GO-DOWN+ mod-shift)
	(cursor-move-home mod-shift))
      (progn
	(cursor-inc-current-char-number 1)
	(app-repaint)))
  
  (cursor-update-select mod-shift))



(defun cursor-insert-text (text)  
  (doc-insert-text (cursor-current-line-number)
		   (cursor-current-char-number)
		   text)
  (cursor-inc-current-char-number 1)

  (app-repaint)
  (select-finish))

(defun cursor-delete-selection ()
  (let ((sel-start (select-normalized)))
    (doc-delete)
    (app-repaint)
    (setf (cursor-current-line-number) (cursor-line-number sel-start)
	  (cursor-current-char-number) (cursor-char-number sel-start))))

(defun cursor-backspace ()
  (if (select-active?)
      (cursor-delete-selection)

      ;; else regular backspace
      (if (buffer-cursor-at-start? (cursor-current-char-number))
	  (when (> (cursor-current-line-number) 0)
	    (let ((line (first (let ((pos (cursor-current-line-number)))
				 (doc-cut-lines pos (1+ pos))))))
	      (decf (cursor-current-line-number))
	      (doc-setup-cursor-line (cursor-current-line-number))
	      (cursor-move-end nil)
	      (doc-insert-text (cursor-current-line-number)
			       (buffer-length)
			       line)
	      (app-repaint)))

	  ;; else
	  (progn
	    
	    (doc-backspace (cursor-current-line-number)
			   (cursor-current-char-number))
	    
	    (cursor-inc-current-char-number -1)
	    (app-repaint))))

  (select-finish))


(defun cursor-delete ()

  (if (select-active?)
      (cursor-delete-selection)
    
      ;; else no selection
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

  (select-finish))

(defun cursor-select-begin ()
  (unless (select-active?)
    (select-begin *current-cursor*)))

(defun cursor-select-finish ()
  (select-finish)
  (app-repaint))

(defun cursor-newline ()
  (log-wr :info "cursor-newline")
  (let ((new-line (doc-split-to-end (cursor-current-line-number)
				    (cursor-current-char-number))))
    
    (doc-insert-lines (1+ (cursor-current-line-number)) new-line)    
    (cursor-move-vertical +CURSOR-GO-DOWN+)
    (cursor-move-home))
  (select-finish))

(defun cursor-copy ()
  (when (select-active?)
    (log-wr :info "copy")
    (let ((doc (doc-copy)))
      (log-wr :info "~s" doc)
      (clipboard-set doc))))

(defun cursor-paste ()
  (when (clipboard-has-text?)
    (log-wr :info "paste")
    (let ((lines (clipboard-get)))
      (log-wr :info "~s" lines)
      (doc-paste (cursor-current-line-number)
		 (cursor-current-char-number)
		 lines)
      (incf (cursor-current-line-number) (length lines))
      (setf (cursor-current-char-number) (length (first (last lines))))
      (view-adjust-for-cursor-line (cursor-current-line-number))
      (app-repaint))))
