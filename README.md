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

#License
Licensed under Modified BSD License.

See License.txt for more details.
