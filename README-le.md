# Pretzil by DPF

Slightly modified to run without CLIM.

1. Create the zc-test package, per package.lisp
2. Load test.lisp then instructions.lisp
3. Switch to ZC-TEST package
4. Run commands:

```lisp
(make-package :zc-test :use '(:cl))
(load "test.lisp")
(load "instructions.lisp")
(in-package :zc-test)
(test "~/src/genera/genera-src/gczm/zork1.z3")
(zcode-run)
open mailbox
quit
y
(with-open-file (s "~/pc-log.l" :direction :output :if-exists :supersede) (write (reverse *pc-log*) :stream s))
```

Slightly modified to log which instruction addresses are executed.

(May need to change random number generator to return a predefined
sequence.)
