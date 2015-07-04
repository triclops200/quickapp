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

(defmacro loop-rec (bindings &body body)
  "Similar to the clojure loop macro"
  (let ((names (mapcar #'car bindings))
		(init-vals (mapcar #'cadr bindings)))
	`(labels ((rec ,names
				,@body))
	   (rec ,@init-vals))))

(defun reduce-full (f &rest args)
  "A reducer that allows f to control the iteration as well as the
accumulator"
  (if (= (length args) 1)
	  (reduce-full f (caar args) (cdar args))
	  (loop-rec ((acc (list (car args) (cadr args))))
		 (if (cadr acc)
			 (rec (apply f acc))
			 (car acc)))))


(defmacro with-string-stream (stream &body body)
  "A macro to allow one to write to a variable string stream and get
the body result out"
  (let ((st-name (gensym))
		(res-name (gensym)))
	`(let ((,st-name (make-array '(0) :element-type 'base-char
								 :fill-pointer 0 :adjustable t)))
	   (let ((,res-name (with-output-to-string (,stream ,st-name)
						  ,@body)))
		 (values ,st-name ,res-name)))))

(defun subseq-rel (seq fst &optional (l 0 l-p))
  "A relative subsequence function"
  (if l-p
	  (subseq seq fst (+ fst l))
	  (subseq seq fst)))

(defun split-string-first (seperator str)
  "Returns a value pair representing the first split in the string
matching the seperator string"
  (let ((l (length seperator))
		(el (length str)))
	(with-string-stream st 
	  (loop-rec ((i 0)) ;; Loop over indicies
		 (cond ((> (+ i l) el) ;; If we have hit the end
				(write-string (subseq-rel str i (1- l)) st) ;; Write the rest of the string to the stream
				"") ;;There is nothing to seperate, return an empty string for the second half
			   ((string= seperator (subseq-rel str i l)) ;; We have found the seperator
				(subseq str (+ i l))) ;; Return the rest of the string as the second half
			   (t (progn ;; We need to keep going
					(write-char (char str i) st) ;; Copy the current char to the stream
					(rec (1+ i))))))))) ;; Iterate to the next string

(defun parse-arg (acc arg-list)
  "Parses one argument/argument pair into the named/unnamed lists
appropriately"
  (destructuring-bind (unnamed named) acc ;; keep track of the unnamed and named args
	(let* ((x (car arg-list)) ;; let x be the first argument
		   (l (length x))) ;; let l be the next argument
	  (cond
		((and (>= l 2) (string= "--" (subseq x 0 2))) ;; If we have a fully-named arg
		 (multiple-value-bind (fst snd) (split-string-first "=" x) ;; split the arg on "="
		   (list (list unnamed (cons (cons (subseq fst 2) snd) named)) ;;add to the named list
				 (cdr arg-list)))) 
		((string= "-" (subseq x 0 1)) ;; if we have an abbreviated arg
		 (cond
		   ((= l 2)
			(list (list unnamed (cons (cons (subseq x 1) (cadr arg-list)) named))
				  (cddr arg-list)))
		   ((= l 1)
			(list (list (cons "-" unnamed) named)
				  (cdr arg-list)))
		   (t
			(list (list unnamed (cons (cons (subseq-rel x 1 1) (subseq x 2)) named)) ;;add to the named list
				  (cdr arg-list))))) 
		(t ;;otherwise, this is an unnamed argument
		 (list (list (cons x unnamed) named) (cdr arg-list)))))))

(defun parse-unfixed-args (args)
  "Parse an argument list into the appropriate list"
  (destructuring-bind (unnamed named) (reduce-full #'parse-arg (list nil nil) args)
	(list (reverse unnamed) named)))

(defun fix-named-arg (arg-names arg)
  (let ((x (assoc (first arg) arg-names :test #'string=)))
	(if x
		(cons (second x) (cdr arg))
		arg)))

(defun fix-named-args (arg-names args)
  (let ((named-args (cadr args)))
	(list (first args)
		  (mapcar (lambda (arg) (fix-named-arg arg-names arg)) named-args))))

(defun parse-args (arg-defs args)
  (let ((parsed-args (parse-unfixed-args args)))
	(fix-named-args arg-defs parsed-args)))

(defun fill-string (str fill-char n)
  (with-string-stream st
	(write-string str st)
	(loop-rec ((n (- n (length str))))
	   (unless (<= n 0)
		 (write-char fill-char st)
		 (rec (1- n))))))

(defun fix-argdef (argdef)
  (if (> (length argdef) 3)
	  (list (if (> (length (first argdef)) 0)
				(concatenate 'string "  -"  (first argdef))
				"  ")
			(concatenate 'string "--" (second argdef) "=" (third argdef))
			(nth 3 argdef))
	  (list (if (> (length (first argdef)) 0)
				(concatenate 'string "  -" (first argdef))
				"  ")
			(concatenate 'string "--" (second argdef))
			(nth 2 argdef))))

(defun generate-flag-string (arg-defs)
  "Generate the part of the help string that documents the options
from the arg definition"
  (let ((l (+ 2 (loop for x in arg-defs
				   maximizing (length (second (fix-argdef x))) into l
				   finally (return l)))))
	(format nil "~{~a~%~}"
			(mapcar (lambda (argdef)
					  (apply #'concatenate 'string
							 (list (fill-string (first argdef) #\Space 6)
								   (fill-string (second argdef) #\Space l)
								   (car (last argdef)))))
					(mapcar #'fix-argdef arg-defs)))))

