;;; -*- Mode: Lisp -*-

(defpackage :blitz.math.project-euler
  (:use :common-lisp :iterate))
(in-package :blitz.math.project-euler)

(defun package-data-file (name)
  "Merge `name' with the source directory of this ASDF package."
  (merge-pathnames 
   name
   (asdf:component-pathname
    (asdf:find-system :project-euler))))

;;; EOF
