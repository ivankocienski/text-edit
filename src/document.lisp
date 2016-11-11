(in-package :text-edit)

;; height of all
(defparameter *doc-view-height* 35
  "The number of lines drawn on the screen")

(defparameter *doc-lines* nil
  "Actual document lines as loaded from file")
(defparameter *doc-num-lines* 0
  "Number of lines in document")

(defparameter *doc-cursor-offset* 0
  "Position of line number of cursor in document")

(defparameter *doc-view-starts-at* nil
  "the top (pointer) of the current view window in the document for drawing")
(defparameter *doc-view-offset* 0
  "line number of the top of the view window")


;; i should *really* re-factor this.

(defun doc-init ()
  (setf *doc-cursor-offset* 0
	*doc-view-offset* 0)
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

;;(defun doc-scroll-up ()
  ;;(format t "doc-scroll-up~%")
  
;;  (when (> *doc-view-offset* 0)
;;    (decf *doc-view-offset*)
;;    (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))))

(defun doc-scroll-down ()
  ;;(format t "doc-scroll-down~%")
  (when (< *doc-view-offset* *doc-num-lines*)
    (incf *doc-view-offset*)
    (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))))

(defun doc-adjust-view-for-cursor ()
  ;; do it this way so we can adjust all the places!
  
  ;;(when (< (- *doc-cursor-offset* *doc-view-offset*) 0)
  ;;  (doc-scroll-up))

  (let ((top-offset (- *doc-cursor-offset* *doc-view-offset*)))
    (cond
      ((< top-offset 0)
       (incf *doc-view-offset* top-offset)
       (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*)))

      ((> top-offset *doc-view-height*)
       (incf *doc-view-offset* (- top-offset *doc-view-height*))
       (setf *doc-view-starts-at* (nthcdr *doc-view-offset* *doc-lines*))))))
    
  
;;  (when (> (- *doc-cursor-offset* *doc-view-offset*) *doc-view-height*)
;;    (doc-scroll-down)))

(defun doc-cursor-up (&optional (line-count 1))
  (format t "doc-cursor-up~%")
  
  (when (> *doc-cursor-offset* 0)
    (decf *doc-cursor-offset* line-count)
    (when (< *doc-cursor-offset* 0)
      (setf *doc-cursor-offset* 0))

    (if (select-active?)
	(select-update)
	(select-clear))
    
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

    (if (select-active?)
	(select-update)
	(select-clear))

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
  
  (doc-update-cursor)
  
  (if (select-active?)
      (select-update)
      (select-clear)))

(defun doc-cursor-right ()
  (if (buffer-cursor-at-end?)
      (progn
	(doc-cursor-down)
	(buffer-cursor-go-home))
      (buffer-cursor-right))
  
  (doc-update-cursor)

  (if (select-active?)
      (select-update)
      (select-clear)))

(defun doc-backspace ()
  (buffer-backspace)
  (select-clear)
  (doc-update-line *doc-cursor-offset* *buffer-line*)
  (doc-update-cursor)
  (app-repaint))

(defun doc-delete ()

  (if (select-active?)

      ;; do a 'cut'
      (if (select-single-line?)
	  (multiple-value-bind (cut-text remaining)
	      (buffer-cut-line (select-current-start-char)
			       (select-current-end-char))
	    
	    (declare (ignore cut-text))
	    (doc-update-line *doc-cursor-offset* remaining)
	    (app-repaint))

	  ;; multi-line cut
	  (let* ((start-line-pos (select-current-start-line))
		 (end-line-pos   (select-current-end-line))
		 (start-line (nth start-line-pos *doc-lines*))
		 (end-line   (nth end-line-pos   *doc-lines*)))
	    
	    (multiple-value-bind (start-cut start-remaining)
		(string-snip start-line
			     (select-current-start-char)
			     (length start-line))

	      (declare (ignore start-cut))
		       
	      (multiple-value-bind (end-cut end-remaining)
		  (string-snip end-line
			       -1
			       (select-current-end-char))

		(declare (ignore end-cut))

		(let ((doc-head (subseq *doc-lines* 0 start-line-pos))
		      (doc-tail (subseq *doc-lines* end-line-pos (length *doc-lines*))))

		  (setf *doc-lines* (append doc-head
					    (list (format nil "~a~a" start-remaining end-remaining))
					    doc-tail)))
		(app-repaint)))))

      ;; do a delete char right
      (progn
	(doc-cursor-right)
	(doc-backspace))))

(defun doc-text (text)
  (buffer-append text)
  (select-clear)
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
    (select-clear)
    (doc-update-cursor)
    (app-repaint)))

(defun doc-draw ()
  
  (loop for line in *doc-view-starts-at*
     and ypos from 0 by 16
     and line-no from *doc-view-offset* by 1
     and lines-to-draw from *doc-view-height* above -1
     ;;do (log-wr :debug "line-no=~d" line-no)
     do (font-draw-string 0
			  ypos
			  line
			  (select-highlight-for-line line-no))))
  
  ;;(cursor-draw-block *buffer-cursor-pos*
;;		     (- *doc-cursor-offset* *doc-view-offset*)))
