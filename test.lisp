;;;
;;; test.lisp
;;;
;;; first cut at a zcode interpreter.
;;;
;;; Alastair Bridgewater, late February 2005.
;;;

(in-package :zc-test)

#|
Okay, here's the deal. This is a somewhat cleaned up first try at
building a version 3 zcode interpreter. It's still missing stuff
like a proper user interface and a status bar. It assumes that it
is being run on a CLIM stream (uses clim:accept for input), so it
might be best to run it from the McCLIM listener.

To use this, call TEST and pass the path to a version 3 game file
(tested with zork1, wishbringer and hitchhikers, should work with
zork2, zork3, planetfall, and leather goddesses). The games are
almost certain to be unwinnable, and may be unplayable. After you
load the game file with TEST, call ZCODE-RUN with no parameters
to start the game.

The obvious next steps are fixing any emulation bugs, adding a
proper user interface (including a status bar), and adding support
for version 4 and 5 Z-Machines (version 4 for AMFV, version 5 for
all of the games written by the IF community).
|#

;;; Interpreter State

(defvar *game-image* nil)

(defvar *dynamic-memory* nil)
(defvar *data-stack* nil)
(defvar *call-stack* nil)
(defvar *current-pc* 0)

(defvar *machine-version* 0)

(defvar *dictionary-start* 0)
(defvar *object-start* 0)
(defvar *global-start* 0)
(defvar *static-start* 0)
(defvar *abbreviation-start* 0)

(defvar *code-offset* 0)
(defvar *code-shift* 0)
(defvar *string-offset* 0)
(defvar *string-shift* 0)

(defparameter *operands*
  (make-array 4 :element-type '(unsigned-byte 16) :fill-pointer t))

(defstruct (stack-frame (:type list))
  return-pc
  discard-result
  local-vars
  result-variable-number
  args-supplied
  frame-pointer)


;;; Opcode Tables

(defparameter *0op-table* (make-array 16 :initial-element nil))
(defparameter *1op-table* (make-array 16 :initial-element nil))
(defparameter *2op-table* (make-array 32 :initial-element nil))
(defparameter *var-table* (make-array 32 :initial-element nil))

(defparameter *base-0op-table* (make-array 16 :initial-element nil))
(defparameter *base-1op-table* (make-array 16 :initial-element nil))
(defparameter *base-2op-table* (make-array 32 :initial-element nil))
(defparameter *base-var-table* (make-array 32 :initial-element nil))

(defparameter *v3-0op-table* (make-array 16 :initial-element nil))
(defparameter *v3-1op-table* (make-array 16 :initial-element nil))
(defparameter *v3-2op-table* (make-array 32 :initial-element nil))
(defparameter *v3-var-table* (make-array 32 :initial-element nil))

(defparameter *v4-0op-table* (make-array 16 :initial-element nil))
(defparameter *v4-1op-table* (make-array 16 :initial-element nil))
(defparameter *v4-2op-table* (make-array 32 :initial-element nil))
(defparameter *v4-var-table* (make-array 32 :initial-element nil))

(defun machine-0op-table (&optional (version *machine-version*))
  (case version
    (3 *v3-0op-table*)
    (4 *v4-0op-table*)
    (t *base-0op-table*)))

(defun machine-1op-table (&optional (version *machine-version*))
  (case version
    (3 *v3-1op-table*)
    (4 *v4-1op-table*)
    (t *base-1op-table*)))

(defun machine-2op-table (&optional (version *machine-version*))
  (case version
    (3 *v3-2op-table*)
    (4 *v4-2op-table*)
    (t *base-2op-table*)))

(defun machine-var-table (&optional (version *machine-version*))
  (case version
    (3 *v3-var-table*)
    (4 *v4-var-table*)
    (t *base-var-table*)))


;;; Misc Utilities

(defun load-gamefile (filename)
  "Read up the contents of FILENAME into a new array of (unsigned-byte 8) sized to fit, returning the arrray."
  (with-open-file (file filename
		   :direction :input
		   :element-type '(unsigned-byte 8))
    (let* ((length (file-length file))
	   (retval (make-array length :element-type '(unsigned-byte 8))))
      ;; FIXME: Might, just might, be slightly system dependant.
      ;; I hear READ-SEQUENCE is good for this stuff.
      (sb-sys:read-n-bytes file retval 0 length)
      retval)))


;;; Number Conversion

(defun u8->s8 (x)
  "Convert an (unsigned-byte 8) to a (signed-byte 8)."
  (if (logbitp 7 x)
      (- x #x100)
    x))

(defun u16->s16 (x)
  "Convert an (unsigned-byte 16) to a (signed-byte 16)."
  (if (logbitp 15 x)
      (- x #x10000)
    x))


;;; Memory Access

(defun read-z-word (byte-address)
  (let ((image (if (< byte-address *static-start*)
		   *dynamic-memory* *game-image*))) 
    (dpb (aref image byte-address)
	 (byte 8 8)
	 (aref image (1+ byte-address)))))

(defun read-z-byte (byte-address)
  (let ((image (if (< byte-address *static-start*)
		   *dynamic-memory* *game-image*))) 
    (aref image byte-address)))

(defun write-z-word (byte-address value)
  (setf (aref *dynamic-memory* byte-address) (ldb (byte 8 8) value))
  (setf (aref *dynamic-memory* (1+ byte-address)) (ldb (byte 8 0) value)))

(defun write-z-byte (byte-address value)
  (setf (aref *dynamic-memory* byte-address) value))

(defun zcode-fetch-byte ()
  (let ((retval (read-z-byte *current-pc*)))
    (incf *current-pc*)
    retval))

(defun zcode-fetch-word ()
  (let ((hi (zcode-fetch-byte))
	(lo (zcode-fetch-byte)))
    (dpb hi (byte 8 8) lo)))

(defun unpack-code-address (address)
  (+ *code-offset* (ash address *code-shift*)))

(defun unpack-string-address (address)
  (+ *string-offset* (ash address *string-shift*)))

(defun zcode-read-variable (number)
  (cond
    ((zerop number)
     (assert (>= (fill-pointer *data-stack*)
		 (+ 4 (stack-frame-frame-pointer (first *call-stack*))
		    (stack-frame-local-vars (first *call-stack*)))))
     (vector-pop *data-stack*))
    ((< number 16)
     (assert (<= number (stack-frame-local-vars (first *call-stack*))))
     (aref *data-stack* (+ 3 number (stack-frame-frame-pointer (first *call-stack*)))))
    (t (read-z-word (+ *global-start* number number -32)))))

(defun zcode-write-variable (number value)
  (cond
    ((zerop number) (vector-push value *data-stack*))
    ((< number 16)
     (assert (<= number (stack-frame-local-vars (first *call-stack*))))
     (setf (aref *data-stack* (+ 3 number (stack-frame-frame-pointer (first *call-stack*)))) value))
    (t (write-z-word (+ *global-start* number number -32) value))))


;;; ZSCII Decoding

(defun find-abbrev-address (abbrev-number)
  (ash (read-z-word (+ *abbreviation-start* (ash abbrev-number 1))) 1))

(defparameter *a0-table* #(#\Space 1 2 3 4 5 #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(defparameter *a1-table* #(#\Space 1 2 3 4 5 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(defparameter *a2-table* #(#\Space 1 2 3 4 5 6 #\Newline #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\, #\! #\? #\_ #\# #\' #\" #\/ #\\ #\- #\: #\( #\)))

(defun print-z-string (address)
  "Print a Z-string starting at ADDRESS. Returns string length in bytes."
  (let ((current-table *a0-table*)
	(next-table *a0-table*)
	(position 0)
	(word 0)
	(abbrev-mode nil)
	(literal-mode nil)
	(literal-value 0))
    (loop
       (setf word (read-z-word (+ address position)))
       (incf position 2)
       (dotimes (i 3)
	 (setf current-table next-table)
	 (setf next-table *a0-table*)

	 (let* ((entry-num (ldb (byte 5 (* 5 (- 2 i))) word))
		(entry (aref current-table entry-num)))
	   (cond
	     (abbrev-mode
	      (progn
		(print-z-string (find-abbrev-address (dpb (1- abbrev-mode) (byte 2 5) entry-num)))
		(setf abbrev-mode nil)))
	     (literal-mode
	      (setf literal-value (dpb entry-num (byte 5 (* 5 (1- literal-mode))) literal-value))
	      (when (zerop (decf literal-mode))
		(setf literal-mode nil)
		(write-char (code-char literal-value))))
	     (t (case entry
		  ((1 2 3) (setf abbrev-mode entry))
		  (4 (setf next-table *a1-table*))
		  (5 (setf next-table *a2-table*))
		  (6 (setf literal-mode 2) (setf literal-value 0))
		  (t (write-char entry)))))))
       (when (not (zerop (logand #x8000 word)))
	 (return-from print-z-string position)))))


;;; Object Manipulation

(defun object-address (objnum)
  (unless (<= 1 objnum (if (>= *machine-version* 4) 65535 255))
    (format t "bogus objnum ~D in object-address.~%" objnum)
    (invoke-restart 'abort-instruction))
  (if (>= *machine-version* 4)
      (+ *object-start* 126 (* (1- objnum) 14))
      (+ *object-start* 62 (* (1- objnum) 9))))

;; Attributes.

(defun object-attr (objnum attrnum)
  (unless (<= 0 attrnum (if (>= *machine-version* 4) 47 31))
    (format t "bogus attrnum ~D in object-attr.~%" objnum)
    (invoke-restart 'abort-instruction))
  (let ((attr-address (+ (object-address objnum)
			 (ldb (byte 3 3) attrnum)))
	(attr-bit (- 7 (logand attrnum 7))))
    (ldb (byte 1 attr-bit) (read-z-byte attr-address))))

(defun (setf object-attr) (value objnum attrnum)
  (unless (<= 0 attrnum (if (>= *machine-version* 4) 47 31))
    (format t "bogus attrnum ~D in (setf object-attr).~%" objnum)
    (invoke-restart 'abort-instruction))
  (let ((attr-address (+ (object-address objnum)
			 (ldb (byte 3 3) attrnum)))
	(attr-bit (- 7 (logand attrnum 7))))
    (write-z-byte attr-address
		  (dpb value (byte 1 attr-bit)
		       (read-z-byte attr-address)))))

;; Parent-child relations.

(defun object-parent (objnum)
  (if (>= *machine-version* 4)
      (read-z-word (+ (object-address objnum) 6))
      (read-z-byte (+ (object-address objnum) 4))))

(defun (setf object-parent) (parent objnum)
  (if (>= *machine-version* 4)
      (write-z-word (+ (object-address objnum) 6) parent)
      (write-z-byte (+ (object-address objnum) 4) parent)))

(defun object-sibling (objnum)
  (if (>= *machine-version* 4)
      (read-z-word (+ (object-address objnum) 8))
      (read-z-byte (+ (object-address objnum) 5))))

(defun (setf object-sibling) (sibling objnum)
  (if (>= *machine-version* 4)
      (write-z-word (+ (object-address objnum) 8) sibling)
      (write-z-byte (+ (object-address objnum) 5) sibling)))

(defun object-child (objnum)
  (if (>= *machine-version* 4)
      (read-z-word (+ (object-address objnum) 10))
      (read-z-byte (+ (object-address objnum) 6))))

(defun (setf object-child) (child objnum)
  (if (>= *machine-version* 4)
      (write-z-word (+ (object-address objnum) 10) child)
      (write-z-byte (+ (object-address objnum) 6) child)))

(defun object-delink-from-parent (child-objnum)
  "Delink CHILD-OBJNUM from its parent. Will fail an assert if CHILD-OBJNUM is not on its parents child list. Will leave CHILD-OBJNUM with bogus parent and sibling values, but its former parent with a valid child list."
  (let ((parent-objnum (object-parent child-objnum)))
    (when (zerop parent-objnum) (return-from object-delink-from-parent))
    (if (= child-objnum (object-child parent-objnum))
	;; Easy case first, this is the first child.
	(setf (object-child parent-objnum) (object-sibling child-objnum))
	;; Not-so-easy case, we have to walk the sibling list.
	(let ((sibling-objnum (object-child parent-objnum)))
	  (loop
	     (assert (not (zerop sibling-objnum)))
	     (when (= child-objnum (object-sibling sibling-objnum))
	       (setf (object-sibling sibling-objnum)
		     (object-sibling child-objnum))
	       (return-from object-delink-from-parent))
	     (setf sibling-objnum (object-sibling sibling-objnum)))))))

;; Properties.

(defun object-property-table (objnum)
  (if (>= *machine-version* 4)
      (read-z-word (+ (object-address objnum) 12))
      (read-z-word (+ (object-address objnum) 7))))

(defun property-length (propaddr)
  "Given the address of a property data field, return its length in bytes."
  (assert (not (zerop propaddr)))
  (if (>= *machine-version* 4)
      (let ((size-byte (read-z-byte (1- propaddr))))
	(if (zerop (logand #x80 size-byte))
	    (1+ (ldb (byte 1 6) size-byte))
	    (logand #x3f size-byte)))
      (1+ (ldb (byte 3 5) (read-z-byte (1- propaddr))))))

(defun property-number (propaddr)
  "Given the address of a property data field, return its property number."
  (assert (not (zerop propaddr)))
  (if (>= *machine-version* 4)
      (let ((size-byte (read-z-byte (1- propaddr))))
	(if (zerop (logand #x80 size-byte))
	    (logand #x3f size-byte)
	    (logand #x3f (read-z-byte (- propaddr 2)))))
      (ldb (byte 5 0) (read-z-byte (1- propaddr)))))

(defun object-first-property (objnum)
  (let* ((property-table (object-property-table objnum))
	 (name-length (read-z-byte property-table))
	 (first-property (+ property-table name-length name-length 1)))
    (if (>= *machine-version* 4)
	(logand #x3f (read-z-byte first-property))
	(ldb (byte 5 0) (read-z-byte first-property)))))

(defun object-property-addr (objnum propnum)
  "Return the address just past the length byte of the property PROPNUM on object OBJNUM or 0 if there is no such property."
  (let* ((property-table (object-property-table objnum))
	 (name-length (read-z-byte property-table))
	 (property-addr (+ property-table name-length name-length 1)))
    (if (>= *machine-version* 4)
	(loop
	   (let* ((size-byte (read-z-byte property-addr))
		  (property-number (logand #x3f size-byte)))
	     (when (= property-number propnum)
	       (return-from object-property-addr
		 (if (zerop (logand #x80 size-byte))
		     (1+ property-addr)
		     (+ 2 property-addr))))
	     (when (> propnum property-number)
	       (return-from object-property-addr 0))
	     (incf property-addr
		   (if (zerop (logand #x80 size-byte))
		       (+ 2 (ldb (byte 1 6) size-byte))
		       (+ 3 (logand #x3f (read-z-byte (1+ property-addr))))))))
	(loop
	   (let* ((size-byte (read-z-byte property-addr))
		  (property-number (ldb (byte 5 0) size-byte)))
	     (when (= property-number propnum)
	       (return-from object-property-addr (1+ property-addr)))
	     (when (> propnum property-number)
	       (return-from object-property-addr 0))
	     (incf property-addr (+ 2 (ldb (byte 3 5) size-byte))))))))

(defun object-next-property (objnum property)
  (let* ((property-addr (object-property-addr objnum property))
	 (property-len (property-length property-addr))
	 (next-property (+ property-addr property-len)))
    (if (>= *machine-version* 4)
	(logand #x3f (read-z-byte next-property))
	(ldb (byte 5 0) (read-z-byte next-property)))))

(defun default-property-value (propnum)
  (read-z-word (+ *object-start* propnum propnum -2)))


;;; Diagnostic utilities

#|
;; Only works on V1-3 objects.
(defun dump-object-data (objnum)
  (let ((object-address (object-address objnum)))
    (format t "Object ~D (" objnum)
    (print-z-string (1+ (object-property-table objnum)))
    (format t ")~%  Attribute bitmap: ~4,'0X~4,'0X~%"
	    (read-z-word object-address)
	    (read-z-word (+ object-address 2)))
    (format t "  Parent ~D~%  Sibling ~D~%  Child ~D~%"
	    (object-parent objnum) (object-sibling objnum)
	    (object-child objnum)))

  (let* ((property-table (object-property-table objnum))
	 (name-length (read-z-byte property-table))
	 (property-addr (+ property-table name-length name-length 1)))
    (loop
	 (let* ((size-byte (read-z-byte property-addr))
		(property-number (ldb (byte 5 0) size-byte)))
	   (when (= property-number 0) (return))
	   (format t "  Property ~D~%" property-number)
	   (cyclotron-gui::hexdump *dynamic-memory* (1+ property-addr) 0 (1+ (ldb (byte 3 5) size-byte)))
	   (incf property-addr (+ 2 (ldb (byte 3 5) size-byte)))))))
|#

;;; Dictionary Manipulation

(defun is-break-char (char-code)
  (let ((num-break-codes (read-z-byte *dictionary-start*)))
    (dotimes (i num-break-codes)
      (when (= char-code (read-z-byte (+ *dictionary-start* i 1)))
	(return-from is-break-char t))))
  nil)

(defun encode-word (word-start word-length)
  (let ((word-1 0)
	(word-2 #x8000)
	(input-position 0)
	(output-position 0)
	(char 0))

    (flet ((output-val (value)
	     (when (< output-position 6)
	       (if (>= output-position 3)
		   (setf word-2 (dpb value
				     (byte 5 (* 5 (- 5 output-position)))
				     word-2))
		   (setf word-1 (dpb value
				     (byte 5 (* 5 (- 2 output-position)))
				     word-1)))
	       (incf output-position))))
      
      (loop
	 (when (= output-position 6)
	   (return))
	 
	 (if (> word-length input-position)
	     (progn
	       (setf char (read-z-byte (+ word-start input-position)))
	       (incf input-position))
	     (setf char -1))
	 
	 (if (or (= char -1) (<= #x61 char #x7a))
	     (let ((value (if (= -1 char) 5 (+ 6 (- char #x61)))))
	       (output-val value))
	     (progn
	       ;; Must be an a2 character or special.
	       (output-val 5)
	       (let ((position (position (code-char char) *a2-table*)))
		 (if position
		     ;; In A2, just put it out.
		     (output-val position)
		     ;; Special, output escape and char code.
		     (progn
		       (output-val 6)
		       (output-val (ldb (byte 3 5) char))
		       (output-val (ldb (byte 5 0) char)))))))))
    (values word-1 word-2)))

(defun find-dictionary-entry (word-start num-chars)
  (multiple-value-bind (word-1 word-2) (encode-word word-start num-chars)

    ;; Phase 2: Look through the dictionary to find a match.
    (let* ((current-entry (+ *dictionary-start* 4
			     (read-z-byte *dictionary-start*)))
	   (entry-size (read-z-byte (- current-entry 3)))
	   (num-entries (read-z-word (- current-entry 2))))

      (dotimes (i num-entries)
		(when (and (= (read-z-word current-entry) word-1)
			   (= (read-z-word (+ current-entry 2)) word-2))
		  (return-from find-dictionary-entry current-entry))
	(incf current-entry entry-size))))
  
  ;; Didn't find an entry, so return 0.
  0)

(defun tokenize-input (text-buffer token-buffer)
  ;; FIXME: I'm still not happy with this function. Not sure why, either.
  (let ((input-buffer (1+ text-buffer))
	(output-buffer (+ token-buffer 2))
	(num-words 0)
	(word-start nil))
    
    (flet
	((store-word (word-start word-length)
	   (let ((token (+ output-buffer (* 4 num-words))))
	     (write-z-byte (+ token 2) word-length)
	     (write-z-byte (+ token 3) (- word-start text-buffer))
	     (write-z-word token (find-dictionary-entry
				  word-start word-length)))
	   (incf num-words)))
      
      (loop
	 (let* ((char-code (read-z-byte input-buffer))
		(break-char (is-break-char char-code))
		(word-break (or break-char (zerop char-code)
				(= 32 char-code))))

	   (when (and word-break word-start)
	     (store-word word-start (- input-buffer word-start)))

	   (when break-char (store-word input-buffer 1))

	   (when word-break (setf word-start nil))

	   (unless (or word-start word-break) (setf word-start input-buffer))

	   (when (zerop char-code) (return)))	 
	 (incf input-buffer)))
    (write-z-byte (1+ token-buffer) num-words))
  #+(or)
  (cyclotron-gui::hexdump *dynamic-memory* token-buffer 0 32))


;;; Instruction Decode and Top-Level Dispatch

(defun read-one-operand (type)
  (case type
    (0 (zcode-fetch-word))
    (1 (zcode-fetch-byte))
    (2 (zcode-read-variable (zcode-fetch-byte)))
    (3 nil)))

(defun opcode-instruction (opcode)
  "Returns the instruction number part of OPCODE."
  (if (= #x80 (logand opcode #xc0))
      ;; 0op or 1op (short format).
      (logand opcode #x0f)
      ;; 2op or var (long or variable format).
      (logand opcode #x1f)))

(defun opcode-table (opcode)
  "Returns the appropriate decode table for OPCODE."
  (cond
    ((zerop (logand opcode #x80)) *2op-table*)
    ((= #xc0 (logand opcode #xe0)) *2op-table*)
    ((= #xe0 (logand opcode #xe0)) *var-table*)
    ((= #xb0 (logand opcode #xf0)) *0op-table*)
    (t *1op-table*)))

(defun parse-operand-type-byte ()
  (let ((type-byte (zcode-fetch-byte)))
    (dotimes (i 4)
      (let ((type-code (ldb (byte 2 (ash (- 3 i) 1)) type-byte)))
	(when (/= type-code 3)
	  (assert (= (fill-pointer *operands*) i))
	  (vector-push (read-one-operand type-code) *operands*))))))

(defun parse-instruction-operands (opcode)
  (setf (fill-pointer *operands*) 0)

  (cond
    ;; long format instruction.
    ((zerop (logand opcode #x80))
     (vector-push (read-one-operand (1+ (ldb (byte 1 6) opcode))) *operands*)
     (vector-push (read-one-operand (1+ (ldb (byte 1 5) opcode))) *operands*))

    ;; variable format instruction.
    ((= #xc0 (logand opcode #xc0))
     (parse-operand-type-byte))

    ;; short format instruction with operands.
    ((/= #xb0 (logand opcode #xf0))
     (vector-push (read-one-operand (ldb (byte 2 4) opcode)) *operands*))))

(defun zcode-step ()
  (let* ((instruction-pc *current-pc*)
	 (instruction-sp (fill-pointer *data-stack*))
	 (opcode (zcode-fetch-byte))
	 (instruction (opcode-instruction opcode))
	 (table (opcode-table opcode)))

    (parse-instruction-operands opcode)

    #+(or)
    (format t "Addr: ~X, instr: ~X, args ~A~%"
	    instruction-pc opcode *operands*)

    (restart-case
	(progn
	  (let ((handler (aref table instruction)))
	    (when (null handler)
	      (format t "Instruction ~(~2,'0X~) not implemented.~%" opcode)
	      (invoke-restart 'abort-instruction))
	    (funcall handler))
	  t)
      (abort-instruction ()
	:report "Abort current instruction, restoring PC and stack pointer."
	(setf *current-pc* instruction-pc)
	(setf (fill-pointer *data-stack*) instruction-sp)
	nil))))

(defun zcode-run ()
  (loop while (zcode-step)))


;;; Initialization (needs to be here for restart instruction).

(defun initialize-instruction-tables ()
  (let ((0op-table (machine-0op-table))
	(1op-table (machine-1op-table))
	(2op-table (machine-2op-table))
	(var-table (machine-var-table)))
    (dotimes (i #x10)
      (setf (aref *0op-table* i)
	    (or (aref 0op-table i) (aref *base-0op-table* i)))
      (setf (aref *1op-table* i)
	    (or (aref 1op-table i) (aref *base-1op-table* i))))
    
    (dotimes (i #x20)
      (setf (aref *2op-table* i)
	    (or (aref 2op-table i) (aref *base-2op-table* i)))
      (setf (aref *var-table* i)
	    (or (aref var-table i) (aref *base-var-table* i))))))

(defun restart-game ()
  ;; Set *static-start* to 0 so read-z-word works from *game-image* while we read the header.
  (setf *static-start* 0)

  ;; Set up the instruction tables.
  (setf *machine-version* (read-z-byte #x00))
  (initialize-instruction-tables)

  ;; Find the basic memory spaces.
  (setf *dictionary-start* (read-z-word #x08))
  (setf *object-start* (read-z-word #x0a))
  (setf *global-start* (read-z-word #x0c))
  (setf *abbreviation-start* (read-z-word #x18))
  (setf *static-start* (read-z-word #x0e))

  ;; Initialize dynamic memory.
  (setf *dynamic-memory* (subseq *game-image* 0 *static-start*))

  ;; Initialize interpreter state.
  (setf *current-pc* (read-z-word #x06))
  (setf *call-stack* (list (make-stack-frame :return-pc 0 :discard-result 0 :local-vars 0 :result-variable-number 0 :args-supplied 0 :frame-pointer 0)))
  (setf *data-stack* (make-array 1024 :element-type '(unsigned-byte 16)
				 :adjustable t :fill-pointer 4))

  (setf *code-offset* 0)
  (setf *code-shift* (case *machine-version*
		       ((1 2 3) 1)
		       ((4 5 6 7) 2)
		       (8 3)))
  (setf *string-offset* 0)
  (setf *string-shift* *code-shift*)
  
  #+(or)
  (let ((dynamic-size (read-z-word 4)))
    (format t "Dynamic Size: ~(~X~)~%" dynamic-size)
    (format t "Starting PC: ~(~X~)~%" *current-pc*)
    (format t "First instruction: ~(~2,'0X~)~%" (read-z-byte *current-pc*))))


;;; Status bar.

#|
(defun update-status-bar ()
  )
|#

;;; Initialization

(defun test (imagefile)
  (setf *game-image* (load-gamefile imagefile))
  (restart-game))

#|
(test "/home/nyef/src/games/if/zcode/specs/minizork.z3")
(test "/home/nyef/p/sampler1_R26.z3")
(test "/home/nyef/src/games/if/zcode/games/wishbrin.dat")
(test "/home/nyef/src/games/if/zcode/games/zork1.dat")
(test "/home/nyef/src/games/if/zcode/games/planetfa.dat")
(test "/home/nyef/src/games/if/zcode/games/hitchhik.dat")
(test "/home/nyef/src/games/if/zcode/games/leather.dat")
(test "/home/nyef/src/games/if/zcode/games/amfv.dat")
|#

;;; EOF
