# quickapp
A common lisp project for generating template projects that use sbcl and buildapp

Exmaple usage
```lisp
(quickapp:quickapp
          "src/lisp/test-project"
          :project-name "test-project" 
          :executable-name "test.out" 
          :project-description "This is a sample test project" 
          :project-author "YOUR NAME HERE" 
          :dependencies '(:sdl2 :cl-openal))
```
This creates the needed files and Makefile as well as a template project.

For easy interactive development, just load the generated "slime.lisp" file then do
```lisp
(in-package :<YOUR PACKAGE NAME>)
```


#License
Licensed under Modified BSD License.

See License.txt for more details.
