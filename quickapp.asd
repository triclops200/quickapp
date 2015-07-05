;;;; quickapp.asd

(asdf:defsystem #:quickapp
  :description "A utility library to automate much of the app creation process"
  :author "Bryan Hoyle"
  :license "Modified BSD License"
  :serial t
  :depends-on ()
  :pathname "./"
  :components ((:file "package")
			   (:file "quickapp")
			   (:file "argument-parsing")))

