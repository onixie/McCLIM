#+(or win32 windows)
(defsystem #:mcclim-w32
  :depends-on (#:clim
	       #:mcclim-multi-mirrored-standard
	       #+(or cmu ecl) (:require #:w32api)
	       #+(or sbcl clozure ecl clisp allegro) #:w32api)
  :components
  ((:file "package")
   (:file "port" :depends-on ("package"))
   (:file "medium" :depends-on ("port" "package"))
   (:file "graft" :depends-on ("port" "package"))
   (:file "frame-manager" :depends-on ("medium" "port" "package"))
   (:file "region" :depends-on ("package"))))
