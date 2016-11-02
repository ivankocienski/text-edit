(in-package :text-edit)

(defparameter *buffer-line* nil)
(defparameter *buffer-cursor-pos* 0)
(defparameter *buffer-doc-pos* 0)
;;(defparameter *buffer-cursor-wants-pos* 0)

(defun buffer-setup (from-line doc-pos)
  (setf *buffer-line* from-line
	*buffer-doc-pos* doc-pos)
  (let ((line-len (length from-line)))
    (when (> *buffer-cursor-pos* line-len))
      (setf *buffer-cursor-pos* line-len)))

(defun buffer-backspace ()
  (format t "backspace~%")
  (let ((newlen (1- (length *buffer-line*))))
    (when (> newlen -1)
      (setf *buffer-line* (subseq *buffer-line* 0 newlen)
	    *buffer-cursor-pos* (1- *buffer-cursor-pos*))
      (doc-update-line *buffer-doc-pos* *buffer-line*)
      (app-repaint))))

(defun buffer-cursor-left ()
  (format t "buffer-cursor-left~%")
  (when (> *bufer-cursor-pos* 0)
    (setf *buffer-cursor-pos* (1- *buffer-cursor-pos*))
    (app-repaint)))

(defun buffer-cursor-right ()
  (format t "buffer-cursor-right~%")
  (when (< *buffer-cursor-pos* (length *buffer-line*))
    (setf *buffer-cursor-pos* (1+ *buffer-cursor-pos*))
    (app-repaint)))

(defun buffer-newline ()
  (format t "newline~%")
  )

(defun buffer-append (thing)
  (format t "buffer-append~%")
  (when (> thing 0)
      (let ((pre-cursor (subseq *buffer-line* 0 *buffer-cursor-pos*))
	    (post-cursor (subseq *buffer-line* *buffer-cursor-pos* (length *buffer-line*))))
	
	(setf *buffer-line* (format nil "~a~a~a" pre-cursor (code-char thing) post-cursor)
	      *buffer-cursor-pos* (1+ *buffer-cursor-pos*)))
      (doc-update-line *buffer-doc-pos* *buffer-line*)
      (app-repaint)))

;;(defun buffer-draw ()
;;  (font-draw-string 10 10 (format nil "~a" *line-buffer*))
;;  (cursor-draw-block *cursor-pos* 0))
