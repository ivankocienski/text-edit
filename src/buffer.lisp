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

(defun buffer-backspace (cursor-pos)
  (format t "backspace~%")

  (cond
    ;; at end of line
    ((buffer-cursor-at-end? cursor-pos)
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

