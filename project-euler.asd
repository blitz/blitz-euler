;;; -*- Mode: Lisp -*-

(defsystem #:project-euler
  :author "Julian Stecklina"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "small-problems"))
  :depends-on (#:iterate))

;;; EOF
