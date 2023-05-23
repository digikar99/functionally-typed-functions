# functionally-typed-functions

> WARNING: This is an experiment.

Lisps are really free to compute at compile time, then why not use functions to compute the return (or argument) types themselves - both at run-time or compile-time?

The current system provides functionally-typed-functions for Common Lisp through CLTL2 and CLOSER-MOP.

This does have issues since CL does not have principal types; types are too arbitrary. For instance, see the ugly mess that is `vector-type-element-type` and `vector-type-length` below.

### The Idea

A `functionally-typed-function` is defined using

```lisp
(def-typed-fun name lambda-list
    type-computing-form
  ...body...)
```

#### type-lambda

With each `functionally-typed-function`, we associate a `type-lambda` that takes in the types of the arguments that the function was called or compiled with, and returns the intended return type. The intended return type can be `NIL` indicating that the function should never have been called with those arguments.

At run time, the arguments to `type-lambda` are in the form of `` `(eql ,arg) ``, but at compile time, they can be any appropriate types.

#### compiler-macro

A `functionally-typed-function` is also associated with a compiler macro, which compiles to just the `body` if the `type-lambda` returns a non-`NIL` value at compile time. This may be avoided by declaring or declaiming the function to be `notinline`.

#### type-declaration-propagation

Using CLTL2, declarations are extracted from the environment at compile time, and these are further propagated inside the inlined-lambda. This requires that the parameters of a `functionally-typed-function` should be treated as constants, not doing so will result in undefined behavior.

TODO: Add a simple but comprehensive example about this.

### Examples

```lisp

(in-package :functionally-typed-functions)

;; These could be better, perhaps much better!
(defun vector-type-element-type (vector-type &optional (simplify-and-type t))
  (optima:ematch (typexpand vector-type)
    ((list* 'specializing 'array element-type _)
     element-type)
    ((list 'eql vector)
     (array-element-type vector))
    ((list 'member vector)
     (array-element-type vector))
    ((list* 'and _)
     (if simplify-and-type
         (vector-type-element-type (simplify-and-type vector-type) nil)
         (error "Tried simplifying already"))))
    ((list* 'or _)
     (if simplify-type
         (vector-type-element-type (simplify-or-type vector-type) nil)
         (error "Tried simplifying already"))))

(defun vector-type-length (vector-type &optional (simplify-and-type t))
  (optima:ematch (typexpand vector-type)
    ((list* 'specializing 'array _ _ (list length) _)
     length)
    ((list 'eql vector)
     (array-total-size vector))
    ((list 'member vector)
     (array-total-size vector))
    ((list* 'and _)
     (if simplify-and-type
         (vector-type-length (simplify-and-type vector-type) nil)
         (error "Tried simplifying already")))
    ((list* 'or _)
     (if simplify-type
         (vector-type-element-type (simplify-or-type vector-type) nil)
         (error "Tried simplifying already")))))

(def-typed-fun 1+vector (a)
    (if (subtypep a 'vector)
        (ignore-errors `(vector ,(vector-type-element-type a) ,(vector-type-length a)))
        nil)
  (let ((out (make-array (array-total-size a) :element-type (array-element-type a))))
    (loop :for i :below (array-total-size out)
          :do (setf (row-major-aref out i) (1+ (row-major-aref a i))))
    out))

(def-typed-fun vector-copy (from to)
    (if (and (subtypep from 'vector)
             (subtypep to 'vector))
        (ignore-errors (if (and (type= (vector-type-element-type from)
                                       (vector-type-element-type to))
                                (eql (vector-type-length from)
                                     (vector-type-length to))
                                (not (eq 'cl:* (vector-type-length from))))
                           `(vector ,(vector-type-element-type from)
                                    ,(vector-type-length to))
                           nil))
        nil)
  (loop :for i :of-type fixnum :below (array-total-size from)
        :do (setf (row-major-aref to i) (row-major-aref from i)))
  to)
```

#### In the REPL

```
FUNCTIONALLY-TYPED-FUNCTIONS> (1+vector '(1 2 3))
; Evaluation aborted on #<SIMPLE-TYPE-ERROR "Arguments~%  ~S~%do not satisfy type signature~%  ~S" {10099FF643}>.
Arguments
  ((1 2 3))
do not satisfy type signature
  (IF (SUBTYPEP A 'VECTOR)
      (IGNORE-ERRORS
       `(VECTOR ,(VECTOR-TYPE-ELEMENT-TYPE A)
                ,(VECTOR-TYPE-LENGTH A)))
      NIL)
FUNCTIONALLY-TYPED-FUNCTIONS> (1+vector #(1 2 3))
#(2 3 4)
FUNCTIONALLY-TYPED-FUNCTIONS> (cl-form-types:nth-form-type
                               `(lambda (x)
                                  (declare (type (vector single-float 10) x))
                                  (1+vector x))
                               nil 0 t t)
(COMMON-LISP:FUNCTION ((VECTOR SINGLE-FLOAT 10)) (VECTOR SINGLE-FLOAT 10))
FUNCTIONALLY-TYPED-FUNCTIONS> (disassemble (lambda (x)
                                            (declare (type (vector double-float 10) x)
                                                     (optimize speed))
                                            (vector-copy x x)))
; disassembly for (LAMBDA (X))
; Size: 104 bytes. Origin: #x55F71226                         ; (LAMBDA (X))
; 26:       4531C0           XOR R8D, R8D
; 29:       EB51             JMP L1
; 2B:       0F1F440000       NOP
; 30: L0:   4C8945F0         MOV [RBP-16], R8
; 34:       4D8BC8           MOV R9, R8
; 37:       4C894DE8         MOV [RBP-24], R9
; 3B:       498BF8           MOV RDI, R8
; 3E:       4883EC10         SUB RSP, 16
; 42:       488B55F8         MOV RDX, [RBP-8]
; 46:       48892C24         MOV [RSP], RBP
; 4A:       488BEC           MOV RBP, RSP
; 4D:       FF1425D0040050   CALL [#x500004D0]                ; SB-KERNEL:HAIRY-DATA-VECTOR-REF
; 54:       4C8B4DE8         MOV R9, [RBP-24]
; 58:       488BF2           MOV RSI, RDX
; 5B:       4883EC10         SUB RSP, 16
; 5F:       488B55F8         MOV RDX, [RBP-8]
; 63:       498BF9           MOV RDI, R9
; 66:       48892C24         MOV [RSP], RBP
; 6A:       488BEC           MOV RBP, RSP
; 6D:       FF1425C8040050   CALL [#x500004C8]                ; SB-KERNEL:HAIRY-DATA-VECTOR-SET
; 74:       4C8B45F0         MOV R8, [RBP-16]
; 78:       4983C002         ADD R8, 2
; 7C: L1:   4183F814         CMP R8D, 20
; 80:       7CAE             JL L0
; 82:       488B55F8         MOV RDX, [RBP-8]
; 86:       488BE5           MOV RSP, RBP
; 89:       F8               CLC
; 8A:       5D               POP RBP
; 8B:       C3               RET
; 8C:       CC10             INT3 16                          ; Invalid argument count trap
NIL
FUNCTIONALLY-TYPED-FUNCTIONS> (cl-form-types:nth-form-type
                               `(lambda (x y)
                                  (declare (type (vector single-float 10) x y))
                                  (vector-copy (1+vector x) y))
                               nil 0 t t)
(COMMON-LISP:FUNCTION ((VECTOR SINGLE-FLOAT 10) (VECTOR SINGLE-FLOAT 10))
 (VECTOR SINGLE-FLOAT 10))
```

### TODO

- [ ] Signal-ing compiler-macro-notes

### Dependencies

![./ftf.png](./ftf.png)
