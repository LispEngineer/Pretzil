;;;
;;; instructions.lisp
;;;
;;; Z-machine instruction emulation.
;;;

(in-package :zc-test)


;;; Instruction Helpers

(defun do-call (store-result &optional (result-var 0))
  (let* ((argcount (fill-pointer *operands*))
	 (function-address (aref *operands* 0))
	 (args-supplied (1- argcount))
	 (local-vars 0)
	 (return-pc *current-pc*))
    (if (zerop function-address)
	(zcode-write-variable result-var 0)
	(progn
	  (setf *current-pc* (unpack-code-address function-address))
	  (setf local-vars (zcode-fetch-byte))
	  (push (make-stack-frame :return-pc return-pc
				  :discard-result (if store-result 0 1)
				  :local-vars local-vars
				  :result-variable-number result-var
				  :args-supplied args-supplied
				  :frame-pointer (fill-pointer *data-stack*))
		*call-stack*)
	  (dotimes (i 4)
	    (vector-push-extend 0 *data-stack*))
	  (dotimes (i local-vars)
	    (vector-push-extend (zcode-fetch-word) *data-stack*))
	  (dotimes (i (min local-vars args-supplied))
	    (zcode-write-variable (1+ i) (aref *operands* (1+ i))))))))

(defun do-ret (value)
  (let ((result-location (stack-frame-result-variable-number (first *call-stack*))))
    (setf *current-pc* (stack-frame-return-pc (first *call-stack*)))
    (setf (fill-pointer *data-stack*) (stack-frame-frame-pointer (first *call-stack*)))
    (setf *call-stack* (cdr *call-stack*))
    (zcode-write-variable result-location value)))

(defun do-branch (condition-true)
  (let* ((branch-byte (zcode-fetch-byte))
	 (branch-offset (logand #x3f branch-byte)))
    (when (zerop (logand #x40 branch-byte))
      (setf branch-offset
	    (dpb branch-offset (byte 6 8)
		 (zcode-fetch-byte))))
    (unless (zerop (logand #x2000 branch-offset))
      (setf branch-offset (dpb branch-offset (byte 14 0) -1)))
    (when (eq condition-true
	      (not (zerop (logand #x80 branch-byte))))
      (case branch-offset
	(0 (do-ret 0))
	(1 (do-ret 1))
	(t (incf *current-pc* (- branch-offset 2)))))))

(defmacro define-instruction
    (name (opcode-type index &key (version :base) store branch
		       arg0 arg1 arg2 arg3
		       (check-argcount nil check-argcount-p) argcount)
     &body forms)
  ;; FIXME: Should probably think of something clever to do with the name.
  ;; There's almost enough info here to build a disassembler.
  (declare (ignorable name branch))
  (unless check-argcount-p
    (setf check-argcount
	  (case opcode-type
	    (0op 0)
	    (1op 1)
	    (2op 2)
	    (var nil))))
  (unless (listp version)
    (setf version (list version)))
  (let ((assert-form
	 (if check-argcount
	     `((assert (= ,check-argcount (fill-pointer *operands*))))))
	(argcount-var (if argcount `((,argcount (fill-pointer *operands*)))))
	(store-var (if store `((,store (zcode-fetch-byte)))))
	(arg0-var (if arg0 `((,arg0 (aref *operands* 0)))))
	(arg1-var (if arg1 `((,arg1 (aref *operands* 1)))))
	(arg2-var (if arg2 `((,arg2 (aref *operands* 2)))))
	(arg3-var (if arg3 `((,arg3 (aref *operands* 3)))))
	(setf-forms
	 (mapcar (lambda (ver)
		   (let ((table (intern (concatenate 'string "*"
						     (symbol-name ver) "-"
						     (symbol-name opcode-type)
						     "-TABLE*"))))
		     `(setf (aref ,table ,index) handler)))
		 version)))
    `(let ((handler (lambda (&aux ,@argcount-var ,@store-var
			     ,@arg0-var ,@arg1-var ,@arg2-var ,@arg3-var)
		      ,@assert-form
		      ,@forms)))
       ,@setf-forms)))


;;; Instruction Emulation

;; 0op instructions.

(define-instruction rtrue (0op #x00)
  (do-ret 1))

(define-instruction rfalse (0op #x01)
  (do-ret 0))

(define-instruction print (0op #x02)
  (incf *current-pc* (print-z-string *current-pc*)))

(define-instruction print-rtrue (0op #x03)
  (print-z-string *current-pc*)
  (write-char #\Newline)
  (do-ret 1))

;; 0op #x04 is NOP, never used.

;; 0op #x05 is SAVE, a branch in v1-3, a store in v4, and bogus in v5+.

;; 0op #x06 is RESTORE, a branch in v1-3, a store in v4, and bogus in v5+.

(define-instruction restart (0op #x07)
  (restart-game))

(define-instruction ret-pulled (0op #x08)
  (do-ret (zcode-read-variable 0)))

;; 0op #x09 is POP in v1-4 and CATCH in v5+, never used.
;(define-instruction pop (0op #x09 :version :v4)
;  (zcode-read-variable 0))

(define-instruction quit (0op #x0a)
  ;; We cheat by just aborting, which causes zcode-run to stop.
  (invoke-restart 'abort-instruction))

(define-instruction new-line (0op #x0b)
  (write-char #\Newline))

(define-instruction show-status (0op #x0c :version :v3)
  (update-status-bar))

;; 0op #x0d is VERIFY, a branch, v3+.

;; 0op #x0e is the EXT prefix, v5+.

;; 0op #x0f is PIRACY, a branch, v5+, never used.


;; 1op instructions.

(define-instruction jz (1op #x00 :arg0 value :branch t)
  (do-branch (zerop value)))

(define-instruction get-sibling
    (1op #x01 :store result-var :arg0 object :branch t)
  (let ((sibling (object-sibling object)))
    (zcode-write-variable result-var sibling)
    (do-branch (not (zerop sibling)))))

(define-instruction get-child
    (1op #x02 :store result-var :arg0 object :branch t)
  (let ((child (object-child object)))
    (zcode-write-variable result-var child)
    (do-branch (not (zerop child)))))

(define-instruction get-parent
    (1op #x03 :store result-var :arg0 object)
  (zcode-write-variable result-var (object-parent object)))

(define-instruction get-prop-len
    (1op #x04 :store result-var :arg0 property)
  (zcode-write-variable result-var (property-length property)))

(define-instruction inc (1op #x05 :arg0 variable)
  (let ((value (zcode-read-variable variable)))
    (zcode-write-variable variable (logand #xffff (1+ value)))))

(define-instruction dec (1op #x06 :arg0 variable)
  (let ((value (zcode-read-variable variable)))
    (zcode-write-variable variable (logand #xffff (1- value)))))

(define-instruction print-addr (1op #x07 :arg0 address)
  (print-z-string address))

(define-instruction call-f0 (1op #x08 :version :v4 :store result-var)
  (do-call t result-var))

(define-instruction remove-obj (1op #x09 :arg0 object)
  (object-delink-from-parent object)
  (setf (object-parent object) 0)
  (setf (object-sibling object) 0))

(define-instruction print-obj (1op #x0a :arg0 object)
  (print-z-string (1+ (object-property-table object))))

(define-instruction ret (1op #x0b :arg0 value)
  (do-ret value))

(define-instruction jump (1op #x0c :arg0 offset)
  (incf *current-pc* (- (u16->s16 offset) 2)))

(define-instruction print-paddr (1op #x0d :arg0 packed-address)
  (print-z-string (unpack-string-address packed-address)))

(define-instruction load (1op #x0e :store result-var :arg0 variable)
  (zcode-write-variable result-var (zcode-read-variable variable)))

;; 1op #x0f is NOT in v1-4 (total bitwise negation), and a call in v5+.

;; 2op instructions.

;; 2op #x00 doesn't exist.

(define-instruction je
    (2op #x01 :branch t :check-argcount nil :argcount num-args
		 :arg0 value :arg1 match-1 :arg2 match-2 :arg3 match-3)
  ;; This is the only 2OP defined to take other than 2 operands.
  ;; Dunno if we can be passed 0 operands.
  ;; If we are passed 1 operand, we don't branch.
  ;; If we are passed 2 or more operands, we branch if the first operand is equal to any of the other operands.
  (assert (<= 1 num-args 4))
  (do-branch (or (and (>= num-args 2)
		      (= value match-1))
		 (and (>= num-args 3)
		      (= value match-2))
		 (and (>= num-args 4)
		      (= value match-3)))))

(define-instruction jl (2op #x02 :arg0 value-1 :arg1 value-2)
  (do-branch (< (u16->s16 value-1)
		(u16->s16 value-2))))

(define-instruction jg (2op #x03 :arg0 value-1 :arg1 value-2)
  (do-branch (> (u16->s16 value-1)
		(u16->s16 value-2))))

(define-instruction dec-jl
    (2op #x04 :branch t :arg0 variable :arg1 value)
  (let ((variable-value (zcode-read-variable variable)))
    (setf variable-value (logand #xffff (1- variable-value)))
    (zcode-write-variable variable variable-value)
    (do-branch (< (u16->s16 variable-value) (u16->s16 value)))))

(define-instruction inc-jg
    (2op #x05 :branch t :arg0 variable :arg1 value)
  (let ((variable-value (zcode-read-variable variable)))
    (setf variable-value (logand #xffff (1+ variable-value)))
    (zcode-write-variable variable variable-value)
    (do-branch (> (u16->s16 variable-value) (u16->s16 value)))))

(define-instruction jin
    (2op #x06 :arg0 object :arg1 container)
  (do-branch (= (object-parent object) container)))

(define-instruction test
    (2op #x07 :branch t :arg0 value :arg1 bitmask)
  (do-branch (= bitmask (logand value bitmask))))

(define-instruction or
    (2op #x08 :store result-var :arg0 value-0 :arg1 value-1)
  (zcode-write-variable result-var (logior value-0 value-1)))

(define-instruction and
    (2op #x09 :store result-var :arg0 value-0 :arg1 value-1)
  (zcode-write-variable result-var (logand value-0 value-1)))

(define-instruction test-attr
    (2op #x0a :arg0 object :arg1 attribute)
  (do-branch (not (zerop (object-attr object attribute)))))

(define-instruction set-attr
    (2op #x0b :arg0 object :arg1 attribute)
  (setf (object-attr object attribute) 1))

(define-instruction clear-attr
    (2op #x0c :arg0 object :arg1 attribute)
  (setf (object-attr object attribute) 0))

(define-instruction store
    (2op #x0d :arg0 variable :arg1 value)
  (zcode-write-variable variable value))

(define-instruction insert-obj
    (2op #x0e :arg0 child :arg1 new-parent)

  ;; Delink the object from the sibling chain on its parent.
  (object-delink-from-parent child)

  ;; Change the object parent.
  (setf (object-parent child) new-parent)

  ;; Link the object into the sibling chain.
  (setf (object-sibling child)
	(object-child new-parent))
  (setf (object-child new-parent) child))

(define-instruction loadw
    (2op #x0f :store result-var :arg0 array :arg1 word-index)
  (zcode-write-variable result-var
			(read-z-word (+ array (ash word-index 1)))))

(define-instruction loadb
    (2op #x10 :store result-var :arg0 array :arg1 byte-index)
  (zcode-write-variable result-var
			(read-z-byte (+ array byte-index))))

(define-instruction get-prop
    (2op #x11 :store result-var :arg0 object :arg1 property-number)
  (let ((propaddr (object-property-addr object property-number)))
    (zcode-write-variable
     result-var
     (if (zerop propaddr)
	 (default-property-value property-number)
	 (if (= 1 (property-length propaddr))
	     (read-z-byte propaddr)
	     (read-z-word propaddr))))))

(define-instruction get-prop-addr
    (2op #x12 :store result-var :arg0 object :arg1 property-number)
  (let ((propaddr (object-property-addr object property-number)))
    (zcode-write-variable result-var propaddr)))

(define-instruction get-next-prop
    (2op #x13 :store result-var :arg0 object :arg1 property-number)
  (zcode-write-variable
   result-var
   (if (zerop property-number)
       (object-first-property object)
       (object-next-property object property-number))))

(define-instruction add
    (2op #x14 :store result-var :arg0 augend :arg1 addend)
  (zcode-write-variable
   result-var (logand #xffff (+ augend addend))))

(define-instruction sub
    (2op #x15 :store result-var :arg0 minuend :arg1 subtrahend)
  (zcode-write-variable
   result-var (logand #xffff (- minuend subtrahend))))

(define-instruction mul
    (2op #x16 :store result-var :arg0 multiplicand :arg1 multiplier)
  (zcode-write-variable
   result-var (logand #xffff (* (u16->s16 multiplicand)
				(u16->s16 multiplier)))))

(define-instruction div
    (2op #x17 :store result-var :arg0 dividend :arg1 divisor)
  (zcode-write-variable
   result-var (logand #xffff (floor (u16->s16 dividend)
				    (u16->s16 divisor)))))

(define-instruction call-f1 (2op #x19 :version :v4 :store result-var)
  (do-call t result-var))


;; Var instructions.

(define-instruction call
    (var #x00 :store result-var)
  (do-call t result-var))

(define-instruction storew
    (var #x01 :check-argcount 3
		 :arg0 array :arg1 word-index :arg2 value)
  (write-z-word (+ array (ash word-index 1)) value))

(define-instruction storeb
    (var #x02 :check-argcount 3
		 :arg0 array :arg1 byte-index :arg2 value)
  (write-z-byte (+ array byte-index) value))

(define-instruction put-prop
    (var #x03 :check-argcount 3
		 :arg0 object :arg1 property :arg2 value)
  (let ((prop-addr (object-property-addr object property)))
    (when (zerop prop-addr)
      (format t "object ~D doesn't have property ~D in instruction put-prop~%" object property)
      (invoke-restart 'abort-instruction))
    (if (= 1 (property-length prop-addr))
	(write-z-byte prop-addr (logand #xff value))
	(write-z-word prop-addr value))))

(define-instruction read
    (var #x04 :version (:v3 :v4) :check-argcount 2 :arg0 text-buffer :arg1 token-buffer)
  (if (<= *machine-version* 3)
      (update-status-bar))
  
  ;; Read input from user using CLIM:ACCEPT and stuff it in the text buffer.
  (let* ((input (clim:accept 'string :prompt nil :default ""))
	 (buflen (read-z-byte text-buffer)))
    (dotimes (i (min buflen (length input)))
      (write-z-byte (+ text-buffer i 1)
		    (char-code (char-downcase (aref input i)))))
    (write-z-byte (+ text-buffer (min buflen (length input)) 1) 0)
    
    ;; Special "it's messed up" abort hook. May disable this later.
    (if (string= input ":abort") (invoke-restart 'abort-instruction)))
  
  (tokenize-input text-buffer token-buffer))

(define-instruction print-char
    (var #x05 :check-argcount 1 :arg0 character)
  (write-char (code-char character)))

(define-instruction print-num
    (var #x06 :check-argcount 1 :arg0 value)
  (format t "~D" (u16->s16 value)))

(define-instruction random
    (var #x07 :check-argcount 1 :store result-var :arg0 range)
  (if (<= range 0)
      ;; Set random seed, result is 0.
      ;; FIXME: We currently ignore seeds.
      (zcode-write-variable result-var 0)
      ;; Generate random number between 1 and range, inclusive.
      (zcode-write-variable result-var (1+ (random range)))))

(define-instruction push (var #x08 :check-argcount 1 :arg0 value)
  (zcode-write-variable 0 value))

(define-instruction pull (var #x09 :check-argcount 1 :arg0 variable)
  (zcode-write-variable variable (zcode-read-variable 0)))

(define-instruction split-screen
    (var #x0a :version (:v4) :check-argcount 1 :arg0 n)
  (format t "[split-screen ~D]~%" n))

(define-instruction set-window
    (var #x0b :version (:v4) :check-argcount 1 :arg0 window)
  (format t "[set-window ~D]~%" window))

(define-instruction erase-window
    (var #x0d :version :v4 :check-argcount 1 :arg0 window)
  (format t "[erase-window ~D]~%" window))

(define-instruction set-cursor
    (var #x0f :version :v4 :check-argcount 2 :arg0 s :arg1 x)
  (format t "[set-cursor ~D ~D]~%" s x))

(define-instruction set-text-style
    (var #x11 :version (:v4) :check-argcount 1 :arg0 n)
  (format t "[set-text-style ~D]~%" n))

(define-instruction buffer-mode
    (var #x12 :version :v4 :check-argcount 1 :arg0 bit)
  (format t "[buffer-mode ~D]~%" bit))

(define-instruction output-stream
    (var #x13 :version (:v4) :check-argcount 1 :arg0 stream)
  (format t "[output-stream ~D]~%" stream))

(define-instruction read-char
    (var #x16 :version :v4 :store result-var
	 :arg0 unknown :check-argcount 1)
  (assert (= 1 unknown))
  (zcode-write-variable result-var (char-code (read-char))))

;;; EOF
