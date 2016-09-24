;;;
;;; package.lisp
;;;
;;; package definition for pretzil
;;;

(make-package :zc-test :use '(:cl))

(make-package :zc-gui :use '(:clim-lisp :clim :zc-test))

;;; EOF
