;;;; package.lisp

(defpackage #:quickapp
  (:use #:cl)
  (:export quickapp)
  (:export get-project)
  (:export get-executable)
  (:export get-project-description)
  (:export get-project-author)
  (:export get-dependencies))
