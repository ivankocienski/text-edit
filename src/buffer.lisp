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
  (unless (buffer-cursor-at-start? cursor-pos)

    (setf *buffer-line*
	  (string-snip *buffer-line* (1- cursor-pos) cursor-pos))))

(defun buffer-append (pos text)
  (setf *buffer-line*
	(format nil "~a~a~a"
		(subseq *buffer-line* 0 pos)
		text
		(subseq *buffer-line* pos (buffer-length)))))


(defun buffer-cut-line (start-pos end-pos)

  (multiple-value-bind (remaining cut) (string-snip *buffer-line* start-pos end-pos)
    (setf *buffer-line* remaining)
    cut))

(defun buffer-copy-line (start-char end-char &optional (line *buffer-line*))
  (let ((start-at (or start-char 0))
	(end-at   (or end-char (length line))))
    (subseq line start-at end-at)))
