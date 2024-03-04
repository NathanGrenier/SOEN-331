;; Define is-memberp function
(defun is-memberp (element set)
  "Check if ELEMENT is a member of SET."
  (cond
    ((null set) nil)                     ; If empty set, return nil.
    ((equal element (car set)) t)        ; If the first element of the set equals ELEMENT, return true.
    (t (is-memberp element (cdr set))))) ; Otherwise, recursively check the rest of the set.

;; Demonstrate the behavior of the functions
;; Test is-memberp function
(print (is-memberp 'Ruby '(Python Ruby Lisp))) ; This will output T
(print (is-memberp 'Go '(Python Ruby Lisp)))   ; This will output NIL