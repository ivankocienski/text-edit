(in-package :text-edit)

(defparameter *buffer-line* nil)

(defun buffer-setup (from-line)
  (setf *buffer-line* from-line))

(defun buffer-length ()
  (length *buffer-line*))

(defun buffer-cursor-at-start? (pos)
  (<= pos 0))

(defun buffer-cursor-at-end? (pos)
  (>= pos (length *buffer-line*)))

(defun buffer-backspace ()
  (format t "backspace~%")

  (cond
    ;; at end of line
    ((buffer-cursor-at-end?)
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

(defun buffer-cut-line (start-pos end-pos)

  (multiple-value-bind (remaining cut) (string-snip *buffer-line* start-pos end-pos)
    (setf *buffer-line* remaining)
    cut))

(defun buffer-split-line-at (pos)

  #|
  (buffer-cut-line *buffer-cursor-pos* (length *buffer-line*))
  |#
  
  (cond
    
    ;; at start of line
    ((= *buffer-cursor-pos* 0)
     (let ((old-line *buffer-line*))
       (setf *buffer-line* ""
	     *buffer-cursor-pos* 0
	     *buffer-last-insert-pos* 0)
       old-line))
    
    ;; at end of line
    ((= *buffer-cursor-pos* (length *buffer-line*))
     (setf *buffer-cursor-pos* 0
	   *buffer-last-insert-pos* 0)
     "")
    
    ;; in middle of line
    (t 
       (let ((pre-cursor  (subseq *buffer-line* 0 *buffer-cursor-pos*))
	     (post-cursor (subseq *buffer-line* *buffer-cursor-pos* (length *buffer-line*))))

	 (setf *buffer-line* pre-cursor
	       *buffer-cursor-pos* 0
	       *buffer-last-insert-pos* 0)
	 post-cursor))))

(defun buffer-append (thing)
  (format t "buffer-append~%")
  (when (> (length thing) 0)
      (let ((pre-cursor (subseq *buffer-line* 0 *buffer-cursor-pos*))
	    (post-cursor (subseq *buffer-line* *buffer-cursor-pos* (length *buffer-line*))))
	
	(setf *buffer-line* (format nil "~a~a~a" pre-cursor thing post-cursor)
	      *buffer-cursor-pos* (1+ *buffer-cursor-pos*)
	      *buffer-last-insert-pos* *buffer-cursor-pos*))
      ))

