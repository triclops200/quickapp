(in-package :quickapp)

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
             (rec (multiple-value-list (apply f acc)))
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

(defun split-string-first (seperator str &optional (default-value ""))
  "Returns a value pair representing the first split in the string
matching the seperator string"
  (let ((l (length seperator))
        (el (length str)))
    (with-string-stream st 
      (loop-rec ((i 0))        ;; Loop over indicies
         (cond ((> (+ i l) el) ;; If we have hit the end
                (write-string (subseq-rel str i (1- l)) st)
                                        ; Write the rest of the string
                                        ; to the stream
                default-value)
                                        ; There is nothing to seperate, return
                                        ; an empty string for the second half
               ((string= seperator (subseq-rel str i l))
                                        ; We have found the seperator
                (subseq str (+ i l)))
                                        ; Return the rest of the
                                        ; string as the second half
               (t (progn
                                        ; We need to keep going
                    (write-char (char str i) st)
                                        ; Copy the current char to the
                                        ; stream
                    (rec (1+ i))))))))) ; Iterate to the next string

(defun split-string (seperator str)
  "Fully split a string by seperator."
  (loop-rec ((acc nil)
             (str str))
     (if (not str)
         (nreverse acc)
         (multiple-value-bind (fst snd) (split-string-first seperator str nil)
           (rec (cons fst acc) snd)))))

(defun parse-arg (acc arg-list)
  "Parses one argument/argument pair into the named/unnamed lists
appropriately"
  (destructuring-bind (unnamed named) acc ;; keep track of the unnamed and named args
    (let* ((x (car arg-list)) ;; let x be the first argument
           (l (length x))) ;; let l be the next argument
      (cond
        ((and (>= l 2) (string= "--" (subseq x 0 2))) ;; If we have a fully-named arg
         (multiple-value-bind (fst snd) (split-string-first "=" x) ;; split the arg on "="
           (values
            (list unnamed (cons (cons (subseq fst 2) snd) named)) ;;add to the named list
            (cdr arg-list)))) 
        ((string= "-" (subseq x 0 1)) ;; if we have an abbreviated arg
         (cond
           ((= l 2) ;; The form '-a'
            (values
             (list unnamed (cons (cons (subseq x 1) (cadr arg-list)) named)) ;; add to the named list
             (cddr arg-list)))
           ((= l 1) ;; The form '-'
            (values
             (list (cons "-" unnamed) named) ;; Add "-" to the unnamed list
             (cdr arg-list)))
           (t ;; The form '-m"This is a commit message"'
            (values
             (list unnamed (cons (cons (subseq-rel x 1 1) (subseq x 2)) named)) ;;add to the named list
             (cdr arg-list))))) 
        (t ;;otherwise, this is an unnamed argument
         (values
          (list (cons x unnamed) named)
          (cdr arg-list)))))))

(defun parse-unfixed-args (args)
  "Parse an argument list into the appropriate list"
  (destructuring-bind (unnamed named) (reduce-full #'parse-arg (list nil nil) args)
    (list (nreverse unnamed) named)))

(defun fix-named-arg (arg-names arg)
  "This unifies the named arguments to use the long form."
  (let ((x (assoc (first arg) arg-names :test #'string=)))
    (if x
        (cons (second x) (cdr arg))
        arg)))

(defun fix-named-args (arg-names args)
  "Fix all of the named args to use the long form"
  (let ((named-args (cadr args)))
    (list (first args)
          (mapcar (lambda (arg) (fix-named-arg arg-names arg)) named-args))))

(defun fill-string (str fill-char n)
  "Fill a string with a character until it is at least as long as n
specifies"
  (with-string-stream st
    (write-string str st)
    (loop-rec ((n (- n (length str))))
       (unless (<= n 0)
         (write-char fill-char st)
         (rec (1- n))))))

(defun fix-argdef (argdef)
  "This is a formatter for an argdef to display properly"
  (if (> (length argdef) 3) ;; If we have an assignment type
      (list (if (> (length (first argdef)) 0)
                (concatenate 'string "  -"  (first argdef))
                "  ")
            (concatenate 'string "--" (second argdef) "=" (third argdef))
                                        ; Use the name given as a =
            (nth 3 argdef))
      (list (if (> (length (first argdef)) 0)
                                        ; otherwise, we have a boolean
                                        ; flag
                (concatenate 'string "  -" (first argdef))
                "  ")
            (concatenate 'string "--" (second argdef))
            (nth 2 argdef))))

(defun generate-flag-string (arg-defs)
  "Generate the part of the help string that documents the options
from the arg definition"
  (let ((l (loop for x in arg-defs
              maximizing (length (second (fix-argdef x))) into l
              finally (return l))))
                                        ; Get the maximum length of
                                        ; the argdef longform names
    (format nil "~{~a~%~}"
                                        ; Combine the argdef strings
            (mapcar (lambda (argdef)
                      (apply #'concatenate 'string 
                             (list
                              (fill-string (first argdef) #\Space 6)
                                        ; Two spaces before, two in
                                        ; length, two spaces after
                              (fill-string (second argdef) #\Space (+ 2 l))
                                        ; l is the maxlength, so we
                                        ; get 2 spaces at least
                              (car (last argdef)))))
                                        ; car (last argdef) is the
                                        ; description
					(mapcar #'fix-argdef arg-defs)))))

(defun parse-args (arg-defs args)
  "Actually calls all the functions to parse the args into the correct
form."
  (let ((parsed-args (parse-unfixed-args args)))
    (fix-named-args arg-defs parsed-args)))
