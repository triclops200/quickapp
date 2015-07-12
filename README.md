# quickapp
A common lisp project for generating template projects that use sbcl and buildapp

See https://github.com/triclops200/quickapp-cli for the command line standalone utility.

# Exmaple usage
```lisp
(ql:quickload :quickapp)
(quickapp:quickapp
          "src/lisp/test-project"
          :project-name "test-project" 
          :executable-name "test.out" 
          :project-description "This is a sample test project" 
          :project-author "YOUR NAME HERE" 
          :dependencies '(:sdl2 :cl-openal))
```
This creates the needed files and Makefile as well as a template project.

For easy interactive development in slime, just do (assuming one is in their generated project directory):
```lisp
(load "slime.lisp")
(in-package :<YOUR PACKAGE NAME>)
```

# Arg parsing utilities
This library also contains two functions for dealing with argument handling for the generated application: `(quickapp:parse-args)` and `(quickapp:generate-flag-string)`.

An example usage is shown below

```lisp
(defun -main (&optional args)
  "Entry point"
  (let* ((arg-defs '(("h" "help" "Display this help menu")
	          ("d" "dependencies" "(:dep1 [:dep2 ...])" "The dependencies")
	          ("p" "project-name" "NAME" "The project name")
	          ("a" "project-author" "NAME" "The name of the author")
	          ("s" "project-description" "DESCRIPTION" "The project description")
	          ("e" "executable-name" "NAME" "The executable name")))
         (parsed-args (quickapp:parse-args arg-defs (cdr args))))
	  (if (or (/= (length (first parsed-args)) 1)
	          (assoc "help" (second parsed-args) :test #'string=))
		  (progn (format t "Usage: ~a PROJECT-PATH [OPTIONS]~%OPTIONS:~%~a~%~a~%~a~a~%~a~%~a~%"
			       (first args)
			       (quickapp:generate-flag-string arg-defs)
			       "Example Usage: " (first args) " test-project \\"
			       "  -d\"(:sdl2 :cl-opengl)\" \\"
			       "  --project-author=cluser"))
		  (format t "~a~%" parsed-args))))
```

Running that application with the --help flag results in:
```
Usage: ./quickapp PROJECT-PATH [OPTIONS]
OPTIONS:
  -h  --help                              Display this help menu
  -d  --dependencies=(:dep1 [:dep2 ...])  The dependencies
  -p  --project-name=NAME                 The project name
  -a  --project-author=NAME               The name of the author
  -s  --project-description=DESCRIPTION   The project description
  -e  --executable-name=NAME              The executable name

Example Usage: 
./quickapp test-project \
  -d"(:sdl2 :cl-opengl)" \
  --project-author=cluser
  ```
Running this command: `./quickapp test-project -d"(:sdl2 :cl-opengl)" --project-author=cluser`
results in this list returned as parsed-args
```lisp
(("test-project")
 (("project-author" . "cluser") ("dependencies" . "(:sdl2 :cl-opengl)")))
```


#License
Licensed under Modified BSD License.

See License.txt for more details.
