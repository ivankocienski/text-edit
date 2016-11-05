(in-package :text-edit)

(defparameter *buffer-line* nil)
(defparameter *buffer-cursor-pos* 0)
(defparameter *buffer-last-insert-pos* 0)

(defun buffer-setup (from-line)
  (setf *buffer-line* from-line
	*buffer-cursor-pos* *buffer-last-insert-pos*
	*buffer-cursor-pos* (let ((line-len (length from-line)))
			      (if (> *buffer-cursor-pos* line-len)
				  line-len
				  *buffer-cursor-pos*))))
			      

(defun buffer-backspace ()
  (format t "backspace~%")

  (cond
    ;; at end of line
    ((= *buffer-cursor-pos* (length *buffer-line*))
     (let ((newlen (1- (length *buffer-line*))))
       (when (> newlen -1)
	 (setf *buffer-line* (subseq *buffer-line* 0 newlen)
	       *buffer-cursor-pos* (1- *buffer-cursor-pos*)
	       *buffer-last-insert-pos* *buffer-cursor-pos*)
	 )))

    ;; in middle of line    
    ((> *buffer-cursor-pos* 0)
     (let ((pre-cursor (subseq *buffer-line* 0 (1- *buffer-cursor-pos*)))
	   (post-cursor (subseq *buffer-line* *buffer-cursor-pos* (length *buffer-line*))))
       
       (setf *buffer-line* (format nil "~a~a" pre-cursor post-cursor)
	     *buffer-cursor-pos* (1- *buffer-cursor-pos*)
	     *buffer-last-insert-pos* *buffer-cursor-pos*)))

    ;; TODO: at beginning of line 
    (t nil)))


  

(defun buffer-cursor-left ()
  (format t "buffer-cursor-left~%")
  (when (> *buffer-cursor-pos* 0)
    (setf *buffer-cursor-pos* (1- *buffer-cursor-pos*)
	  *buffer-last-insert-pos* *buffer-cursor-pos*)
    (app-repaint)))

(defun buffer-cursor-right ()
  (format t "buffer-cursor-right~%")
  (when (< *buffer-cursor-pos* (length *buffer-line*))
    (setf *buffer-cursor-pos* (1+ *buffer-cursor-pos*)
	  *buffer-last-insert-pos* *buffer-cursor-pos*)
    (app-repaint)))


(defun buffer-split-line-at-cursor ()
  (cond
    
    ;; at start of line
    ((= *buffer-cursor-pos* 0)
     (let ((old-line *buffer-line*))
       (setf *buffer-line* ""
	     *buffer-cursor-pos* 0)
       old-line))
    
    ;; at end of line
    ((= *buffer-cursor-pos* (length *buffer-line*))
     (setf *buffer-cursor-pos* 0)
     "")
    
    ;; in middle of line
    (t 
       (let ((pre-cursor  (subseq *buffer-line* 0 *buffer-cursor-pos*))
	     (post-cursor (subseq *buffer-line* *buffer-cursor-pos* (length *buffer-line*))))

	 (setf *buffer-line* pre-cursor
	       *buffer-cursor-pos* 0)
	 post-cursor))))

;;(defun buffer-return ()
;;  (format t "newline~%")
;;  )

(defun buffer-append (thing)
  (format t "buffer-append~%")
  (when (> (length thing) 0)
      (let ((pre-cursor (subseq *buffer-line* 0 *buffer-cursor-pos*))
	    (post-cursor (subseq *buffer-line* *buffer-cursor-pos* (length *buffer-line*))))
	
	(setf *buffer-line* (format nil "~a~a~a" pre-cursor thing post-cursor)
	      *buffer-cursor-pos* (1+ *buffer-cursor-pos*)
	      *buffer-last-insert-pos* *buffer-cursor-pos*))
      ))

;;(defun buffer-draw ()
;;  (font-draw-string 10 10 (format nil "~a" *line-buffer*))
;;  (cursor-draw-block *cursor-pos* 0))
