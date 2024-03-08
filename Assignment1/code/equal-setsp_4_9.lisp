;; Define equal-setsp function
(defun equal-setsp (set1 set2)
  "Check if SET1 and SET2 are equal sets."
  (and (subsetp-func set1 set2) (subsetp-func set2 set1)))

;; Define subsetp-func helper function
(defun subsetp-func (subset set)
  "Check if SUBSET is a subset of SET using a functional approach."
  (cond
    ((null subset) t)                 ; If the subset is empty, it is a subset.
    ((is-memberp (car subset) set)    ; If the first element of the subset is in the set,
     (subsetp-func (cdr subset) set)) ; continue checking the rest of the subset.
    (t nil)))                         ; Otherwise, it's not a subset.

;; Define is-memberp helper function
(defun is-memberp (element set)
  "Check if ELEMENT is a member of SET."
  (cond
    ((null set) nil)                     ; If empty set, return nil.
    ((equal element (car set)) t)        ; If the first element of the set equals ELEMENT, return true.
    (t (is-memberp element (cdr set))))) ; Otherwise, recursively check the rest of the set.

;; Test equal-setsp function
(print (equal-setsp '(Python Ruby Lisp) '(Lisp Python Ruby))) ; This will output T
(print (equal-setsp '(Python Ruby) '(Lisp Python Ruby)))      ; This will output NIL