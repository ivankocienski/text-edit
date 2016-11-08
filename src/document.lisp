(in-package :text-edit)

(defparameter *doc-view-height* 35)
(defparameter *doc-lines* nil)
(defparameter *doc-cursor-offset* 0)
(defparameter *doc-view-starts-at* nil)
(defparameter *doc-view-offset* 0)
(defparameter *doc-num-lines* 0)

(defun doc-init ()
  )

(defun doc-update-cursor ()
  (cursor-set-pos *buffer-cursor-pos*
		  *doc-cursor-offset*))

(defun doc-load (path)
  (log-wr :info "doc-load: path=~a" path)
  (with-open-file (read-file path)
    (labels ((read-line-to-doc ()
	       (let ((line (read-line read-file nil)))
		 (when line
		   (cons line (read-line-to-doc))))))

      (setf *doc-lines* (read-line-to-doc))))
  (setf *doc-num-lines* (length *doc-lines*)
	*doc-view-starts-at* *doc-lines*)
  (buffer-setup (first *doc-lines*))
  (doc-update-cursor)
  (log-wr :info "lines=~d" (length *doc-lines*)))


(defun doc-update-line (pos line)
  (setf (nth pos *doc-lines*) line))

(defun doc-scroll-up ()
  ;;(format t "doc-scroll-up~%")
  (when (> *doc-view-offset* 0)
    (decf *doc-view-offset*)
    (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))
    (app-repaint)))

(defun doc-scroll-down ()
  ;;(format t "doc-scroll-down~%")
  (when (< *doc-view-offset* *doc-num-lines*)
    (incf *doc-view-offset*)
    (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))
    (app-repaint)))

(defun doc-adjust-view-for-cursor ()
  ;; do it this way so we can adjust all the places!
  (when (< (- *doc-cursor-offset* *doc-view-offset*) 0)
    (doc-scroll-up))
  (when (> (- *doc-cursor-offset* *doc-view-offset*) *doc-view-height*)
    (doc-scroll-down))
  )

(defun doc-cursor-up (&optional (line-count 1))
  (format t "doc-cursor-up~%")
  
  (when (> *doc-cursor-offset* 0)
    (decf *doc-cursor-offset* line-count)
    (when (< *doc-cursor-offset* 0)
      (setf *doc-cursor-offset* 0))
    
    (buffer-setup (nth *doc-cursor-offset* *doc-lines*))
    (doc-adjust-view-for-cursor)
    (doc-update-cursor)
    (app-repaint)))
    

(defun doc-cursor-down (&optional (line-count 1))
  (format t "doc-cursor-down~%")
  
  (when (< *doc-cursor-offset* *doc-num-lines*)
    (incf *doc-cursor-offset* line-count)
    (when (>= *doc-cursor-offset* *doc-num-lines*)
      (setf *doc-cursor-offset* *doc-num-lines*))
    
    (buffer-setup (nth *doc-cursor-offset* *doc-lines*))
    (doc-adjust-view-for-cursor)
    (doc-update-cursor)
    (app-repaint)))

(defun doc-cursor-left ()
  (if (buffer-cursor-at-start?)
      (progn
	(doc-cursor-up)
	(buffer-cursor-go-end))
      (buffer-cursor-left))
  (doc-update-cursor))


(defun doc-cursor-right ()
  (if (buffer-cursor-at-end?)
      (progn
	(doc-cursor-down)
	(buffer-cursor-go-home))
      (buffer-cursor-right))
  (doc-update-cursor))
      

(defun doc-backspace ()
  (buffer-backspace)
  (doc-update-line *doc-cursor-offset* *buffer-line*)
  (doc-update-cursor)
  (app-repaint))

(defun doc-text (text)
  (buffer-append text)
  (doc-update-line *doc-cursor-offset* *buffer-line*)
  (doc-update-cursor)
  (app-repaint))

(defun doc-return ()
  (log-wr :info "doc-return")
  (let ((new-line (buffer-split-line-at-cursor)))
    (doc-update-line *doc-cursor-offset* *buffer-line*)
    (setf *doc-lines* (insert-at *doc-lines* (1+ *doc-cursor-offset*) new-line)
	  *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))
    (incf *doc-cursor-offset*)
    (doc-update-cursor)
    (app-repaint)))

(defun doc-draw ()
  
  (loop for line in *doc-view-starts-at*
     and ypos from 0 by 16
     and lines-to-draw from *doc-view-height* above -1
     do (font-draw-string 0 ypos line)))
  
  ;;(cursor-draw-block *buffer-cursor-pos*
;;		     (- *doc-cursor-offset* *doc-view-offset*)))
