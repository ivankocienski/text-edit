(in-package :text-edit)

(defparameter *view-window-height* 35
  "The number of lines drawn on the screen")
(defparameter *view-starts-at* nil
  "the top (pointer) of the current view window in the document for drawing")
(defparameter *view-starts-at-pos* 0
  "line number of the top of the view window")


(defun view-draw ()
  
  (loop for line in *doc-view-starts-at*
     and ypos from 0 by 16
     and line-no from *doc-view-offset* by 1
     and lines-to-draw from *doc-view-height* above -1
     ;;do (log-wr :debug "line-no=~d" line-no)
     do (font-draw-string 0
			  ypos
			  line
			  (select-highlight-for-line line-no))))

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

(defun view-adjust-for-cursor-line (doc-line-number)
  )
