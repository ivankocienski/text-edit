(asdf:defsystem #:text-edit
  :description "because emacs"
  :author "Ivan K."
  :license ""
  :serial t
  :depends-on (sdl2 sdl2-ttf)
  :components ((:file "src/package")
	       (:file "src/support")
	       (:file "src/log")
	       (:file "src/font")
	       (:file "src/repaint")
	       (:file "src/timer")
	       (:file "src/cursor")
	       (:file "src/buffer")
	       (:file "src/document")
	       (:file "src/text-edit")))

