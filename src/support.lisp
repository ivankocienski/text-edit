(in-package :text-edit)

(defparameter true t)
(defparameter false nil)

(defun insert-at (lst index newelt)
  (labels ((find-and-insert (c head tail)
	     (if tail
		 (if (> c 0)
		     (find-and-insert (1- c) (append head (list (car tail))) (rest tail))
		     (append head (list newelt) tail))
		 
		 (if (zerop c)
		     (append lst (list newelt))
		     lst))))
    
    (find-and-insert index nil lst)))
