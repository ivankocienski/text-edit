(in-package :text-edit)

(defparameter *cursor-width* 9)
(defparameter *cursor-height* 16)
;;(defparameter *cursor-x* 0)
;;(defparameter *cursor-y* 0)
(defparameter *cursor-on* nil)
(defparameter *cursor-renderer* nil)

(defconstant +CURSOR-GO-UP+    -1)
(defconstant +CURSOR-GO-DOWN+   1)
;;(defconstant +CURSOR-GO-LEFT+  -1)
;;(defconstant +CURSOR-GO-RIGHT+  1)

(defstruct cursor
  (line-number 0 )
  (char-number 0))

(defparameter *current-cursor* nil)

;;(defun cursor-set-pos (x y)
;;  (setf *cursor-x* x
;;	*cursor-y* y))

(defmacro cursor-current-line-number ()
  `(cursor-line-number *current-cursor*))

(defmacro cursor-current-char-number ()
  `(cursor-char-number *current-cursor*))

(defun cursor-init (renderer)
  (setf *cursor-renderer* renderer
	*current-cursor* (make-cursor))
  
  (cursor-set-pos 0 0)
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


;;(defparameter *cursor-doc-pos* 0
;;  "position of cursor in document")
;;(defparameter *cursor-line-pos* 0
;;  "position of cursor on current line")

;;(defun doc-update-cursor ()
;;  (cursor-set-pos *buffer-cursor-pos*
;;		  *doc-cursor-offset*))

    
(defun cursor-move-vertical (delta)
  (log-wr :info "cursor-move-vertical delta=~d" delta)

  (let ((new-line-number (+ (cursor-current-line-number) delta)))
    (when (doc-valid-line? new-line-number)
      
      (setf (cursor-current-line-number) new-line-number)

      (view-adjust-for-cursor-line new-line-number)
      
      ;;(if (select-active?)
      ;;	(select-update)
      ;;	(select-clear))
      
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

  

#|
(defun cursor-go-down ()
  (log-wr :info "cursor-go-down")
  
  (when (< *doc-cursor-offset* *doc-num-lines*)
    (incf *doc-cursor-offset* line-count)
    (when (>= *doc-cursor-offset* *doc-num-lines*)
      (setf *doc-cursor-offset* *doc-num-lines*))

    (if (select-active?)
	(select-update)
	(select-clear))

    (buffer-setup (nth *doc-cursor-offset* *doc-lines*))
    (doc-adjust-view-for-cursor)
    (doc-update-cursor)
    (app-repaint)))
|#

;;(defun cursor-go-left ()
;;   (if (buffer-cursor-at-start?)
;;      (progn
;;	(doc-cursor-up)
;;	(buffer-cursor-go-end))
;;      (buffer-cursor-left))
  
;;  (doc-update-cursor)
  
;;  (if (select-active?)
;;      (select-update)
;;      (select-clear)))

;;(defun cursor-go-right ()
;;    (if (buffer-cursor-at-end?)
;;      (progn
;;	(doc-cursor-down)
;;	(buffer-cursor-go-home))
;;      (buffer-cursor-right))
  
;;  (doc-update-cursor)

;;  (if (select-active?)
;;      (select-update)
;;      (select-clear)))

