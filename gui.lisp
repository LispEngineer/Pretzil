;;;
;;; gui.lisp
;;;
;;; user interface.
;;;

(in-package :zc-gui)

(defclass statusbar-pane (application-pane) ())

(defmethod compose-space ((pane statusbar-pane) &key width height)
  (declare (ignore width height))  
  (let ((height (text-style-height (medium-text-style pane) pane)))
    (make-space-requirement
     :height height :min-height height
     :max-height height)))

(define-application-frame zcode ()
  ()
  (:panes
   (window-1 :interactor 
	     :background +black+
	     :foreground +white+
	     :width 700 :height 350 #|:scroll-bars t|#)
   (window-2 :application :height 0 :scroll-bars nil)
   (status-bar (make-pane 'statusbar-pane)))
  (:top-level (zcode-top-level))
  (:layouts
   (default
       (vertically (:scroll-bars nil)
	 status-bar
	 window-1))
   (no-status
    (vertically (:scroll-bars nil)
      window-1))
   (two-window
    (vertically (:scroll-bars nil)
      status-bar
      window-2
      window-1))
   (two-window-no-status
    (vertically (:scroll-bars nil)
      window-2
      window-1))))

(defun zcode-top-level (frame)
  "Replacement top level. We do this because we don't want to use the normal command loop at all."
  (let ((*standard-output* (frame-standard-input frame))
	(*standard-input* (frame-standard-input frame)))
    (format t "~A~%" (frame-current-layout frame))
    (when (and (not (eq (frame-current-layout frame) 'no-status))
	       (logbitp 1 (aref zc-test::*game-image* 1)))
      (format t "Fnord!~%")
;      (accept 'string :default "")
      (setf (frame-current-layout frame) 'no-status))
    (zc-test::zcode-run)
    (format t "Execution halted, press any key to exit.")
    (read-char)))

(defun zc-test::update-status-bar ()
  (when (or (>= zc-test::*machine-version* 4)
	    (and (= zc-test::*machine-version* 3)
		 (logbitp 1 (aref zc-test::*game-image* 1))))
    (return-from zc-test::update-status-bar))
  (let* ((status-bar (get-frame-pane *application-frame* 'status-bar))
	 (*standard-output* status-bar))
    (window-clear status-bar)

    (zc-test::print-z-string
     (1+ (zc-test::object-property-table
	  (zc-test::zcode-read-variable 16))))

    (format t "~50T")

    (format t "Score: ~D  Turns: ~D"
	    (zc-test::zcode-read-variable 17)
	    (zc-test::zcode-read-variable 18))))

(defun run-zcode-demo (filename)
  (zc-test::test filename)
  (setf (aref zc-test::*dynamic-memory* 1)
	(logior #x20 (aref zc-test::*dynamic-memory* 1)))
  (let ((frame (make-application-frame 'zcode)))
    (when (or (>= zc-test::*machine-version* 4)
	      (logbitp 1 (aref zc-test::*game-image* 1)))
      (setf (frame-current-layout frame) 'no-status))
    (run-frame-top-level frame)))

#|
(run-zcode-demo "/home/nyef/src/games/if/zcode/games/zork1.dat")
(run-zcode-demo "/home/nyef/src/games/if/zcode/games/hitchhik.dat")
(run-zcode-demo "/home/nyef/src/games/if/zcode/games/wishbrin.dat")
(run-zcode-demo "/home/nyef/src/games/if/zcode/games/leather.dat")
(run-zcode-demo "/home/nyef/src/games/if/zcode/games/amfv.dat")
|#

;;; EOF
