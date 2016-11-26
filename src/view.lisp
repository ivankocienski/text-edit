(in-package :text-edit)

(defparameter *view-window-height* 34
  "The number of lines drawn on the screen")

(defparameter *view-starts-at-pos* 0
  "line number of the top of the view window")

(defun view-draw (doc-lines)

  (let ((view-start (nthcdr *view-starts-at-pos* doc-lines)))
    (loop for line in view-start
       and ypos from 0 by 16
       and line-no from *view-starts-at-pos* by 1
       and lines-to-draw from *view-window-height* above -1
       do (font-draw-string 0
			    ypos
			    (format nil "~3d" (1+ line-no)))
       do (font-draw-string 32
			    ypos
			    line
			    (select-highlight-for-line line-no)))))

(defun view-draw-status-line (cursor-char cursor-line filename)
  (font-draw-string 0
		    (* (1+ *view-window-height*) 16)
		    (format nil
			    "C:~d  L:~d  F:~a"
			    cursor-char
			    cursor-line
			    filename)))
  
(defun view-adjust-for-cursor-line (line-number)
  
  (let ((top-offset (- line-number *view-starts-at-pos*)))
    (cond
      ((< top-offset 0)
       (incf *view-starts-at-pos* top-offset))
      
      ((> top-offset *view-window-height*)
       (incf *view-starts-at-pos* (- top-offset *view-window-height*))))))
