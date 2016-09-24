;;; -*- lisp -*-
;;;
;;; pretzil.asd
;;;
;;; system definition for pretzil
;;;

(asdf:defsystem :pretzil
  :depends-on (:mcclim)
  :serial t
  :components
  ((:file "package")
   (:file "test")
   (:file "gui")
   (:file "instructions")))

;;; EOF
