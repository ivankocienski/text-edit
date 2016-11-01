(in-package :text-edit)

(defparameter *doc-view-height* 70)
(defparameter *doc-lines* nil)
(defparameter *doc-cursor-offset* 0)
(defparameter *doc-view-starts-at* nil)
(defparameter *doc-view-offset* 0)
(defparameter *doc-num-lines* 0)

(defun doc-init ()
  )

(defun doc-load (path)
  (format t "doc-load: path=~a~%" path)
  (with-open-file (read-file path)
    (labels ((read-line-to-doc ()
	       (let ((line (read-line read-file nil)))
		 (when line
		   (cons line (read-line-to-doc))))))

      (setf *doc-lines* (read-line-to-doc))))
  (setf *doc-num-lines* (length *doc-lines*))
  (buffer-setup (first *doc-lines*) 0)
  (format t "lines=~d~%" (length *doc-lines*)))

(defun doc-update-line (pos line)
  (setf (nth pos *doc-lines*) line))

(defun doc-scroll-up ()
  (format t "doc-scroll-up~%")
  (when (> *doc-view-offset* 0)
    (decf *doc-view-offset*)
    (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))
    (app-repaint)))

(defun doc-scroll-down ()
  (format t "doc-scroll-down~%")
  (when (< *doc-view-offset* *doc-num-lines*)
    (incf *doc-view-offset*)
    (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))
    (app-repaint)))

(defun doc-cursor-up ()
  (format t "doc-cursor-up~%")
  (when (> *doc-cursor-offset* 0)
    (decf *doc-cursor-offset*)
    (buffer-setup (nth *doc-cursor-offset* *doc-lines*) *doc-cursor-offset*)
    (when (< (- *doc-cursor-offset* *doc-view-offset*) 0)
      (doc-scroll-up))
    (app-repaint)))
    

(defun doc-cursor-down ()
  (format t "doc-cursor-down~%")
  (when (< *doc-cursor-offset* *doc-num-lines*)
    (incf *doc-cursor-offset*)
    (buffer-setup (nth *doc-cursor-offset* *doc-lines*) *doc-cursor-offset*)
    (when (> (- *doc-cursor-offset* *doc-view-offset*) *doc-view-height*)
      (doc-scroll-down))
    (app-repaint)))
    

(defun doc-draw ()
  
  (loop for line in *doc-view-starts-at*
     and ypos from 0 by 8
     and lines-to-draw from *doc-view-height* above -1
     do (font-draw-string 0 ypos line))
  
  (cursor-draw-block *buffer-cursor-pos*
		     (- *doc-cursor-offset* *doc-view-offset*)))
