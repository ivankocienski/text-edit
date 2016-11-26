(in-package :text-edit)

(defparameter *buffer-line* nil)

(defun buffer-setup (from-line)
  (setf *buffer-line* from-line))

(defun buffer-length (&optional (line *buffer-line*))
  (length line))

(defun buffer-cursor-at-start? (pos &optional (line *buffer-line*))
  (declare (ignore line))
  (<= pos 0))

(defun buffer-cursor-at-end? (pos &optional (line *buffer-line*))
  (>= pos (length line)))

(defmacro buffer-backspace (cursor-pos &optional (line *buffer-line*))
  `(unless (buffer-cursor-at-start? ,cursor-pos ,line)

     (setf ,line
	   (string-snip ,line (1- ,cursor-pos) ,cursor-pos))))

(defmacro buffer-append (pos text &optional (line *buffer-line*))
  `(setf ,line
	 (format nil "~a~a~a"
		 (subseq ,line 0 ,pos)
		 ,text
		 (subseq ,line ,pos (buffer-length ,line)))))


(defmacro buffer-cut-line (start-pos end-pos &optional (line *buffer-line*))

  `(multiple-value-bind (remaining cut) (string-snip ,line ,start-pos ,end-pos)
     (setf ,line remaining)
     cut))

(defun buffer-copy-line (start-char end-char &optional (line *buffer-line*))
  (let ((start-at (or start-char 0))
	(end-at   (or end-char (length line))))
    (subseq line start-at end-at)))
