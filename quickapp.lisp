(in-package #:quickapp)

(defun dispatch-stream-rewrite (c1 c2 input-string dispatch-function)
  (let ((fstr (make-array '(0) :element-type 'base-char
						  :fill-pointer 0 :adjustable t)))
	(with-output-to-string (s fstr)
	  (with-input-from-string (f input-string)
		(labels
			((recur (last-char)
			   (if (listen f)
				   (progn (if (and (eql last-char c1)
								   (eql (peek-char nil f) c2))
							  (format s "~a" (funcall dispatch-function f))
							  (write-char last-char s))
						  (recur (read-char f)))
				   (write-char last-char s))))
		  (recur (read-char f)))
		fstr))))

(defun eval-template-string (string)
  (dispatch-stream-rewrite #\# #\( string (lambda (f)
											(eval (read-preserving-whitespace f)))))

(defun slurp-file (filename)
  (with-open-file (stream filename)
	(let ((seq (make-string (file-length stream))))
	  (read-sequence seq stream)
	  seq)))

(defun spit-file (filename string)
  (with-open-file (str filename
					   :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create)
	(format str "~a" string)))


(defun eval-template-file (file)
  (eval-template-string (slurp-file file)))


(defun get-project ()
  *project-name*)

(defun get-executable ()
  *executable-name*)

(defun get-project-description ()
  *project-description*)

(defun get-project-author ()
  *project-author*)

(defun get-dependencies ()
  (format nil "~{:~a~%~}" *project-dependencies*))

(defvar *project-name* "test")
(defvar *executable-name* "test")
(defvar *project-description* "test")
(defvar *project-author* "test")
(defvar *project-dependencies* nil)
(defvar *my-directory* (asdf:system-source-directory :quickapp))

(defun from-my-path (filename)
  (merge-pathnames filename *my-directory*))

(defun quickapp (project-path &key
								(project-name nil pp-p)
								(executable-name nil en-p)
								(project-description "INSERT PROJECT DESCRIPTION HERE")
								(project-author "INSERT PROJECT AUTHOR HERE")
								(dependencies nil))
  (let ((project-path (pathname-directory (directory-namestring (concatenate 'string project-path "/")))))
	(if pp-p
		(setf *project-name* project-name)
		(setf *project-name* (car (last project-path))))
	(if en-p
		(setf *executable-name* executable-name)
		(setf *executable-name* *project-name*))
	(setf *project-description* project-description)
	(setf *project-author* project-author)
	(setf *project-dependencies* dependencies)
	(ensure-directories-exist (make-pathname :directory
											 project-path))
	(let ((makefile (eval-template-file (from-my-path "Makefile.template")))
		  (package  (eval-template-file (from-my-path "package.lisp.template")))
		  (projectasd (eval-template-file (from-my-path "project.asd.template")))
		  (project  (eval-template-file (from-my-path "project.lisp.template")))
		  (slime (eval-template-file (from-my-path "slime.lisp.template"))))
	  (spit-file (make-pathname :directory project-path
								:name "Makefile")
				 makefile)
	  (spit-file (make-pathname :directory project-path
								:name "package"
								:type "lisp")
				 package)
	  (spit-file (make-pathname :directory project-path
								:name *project-name*
								:type "asd")
				 projectasd)
	  (spit-file (make-pathname :directory project-path
								:name *project-name*
								:type "lisp")
				 project)
	  (spit-file (make-pathname :directory project-path
								:name "slime"
								:type "lisp")
				 slime))))

