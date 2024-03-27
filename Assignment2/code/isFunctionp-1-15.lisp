(defun isfunctionp (variable)
  "Check if the given variable corresponds to a function."
  (and (listp variable)                ; Check if variable is a list
       (every #'consp variable)        ; Check if all elements are cons cells
       (every (lambda (pair)           ; Check if each cons cell is a valid key-value pair
                (and (consp pair)
                     (symbolp (car pair))
                     (listp (cdr pair))))
              variable)))

;; Example 1: Variable corresponds to a function
(setq *connections-function*
      '((Montreal . (Ottawa Kingston Quebec Halifax))
        (Ottawa . (Montreal Toronto))
        (Toronto . (Montreal Ottawa))
        (Halifax . (Montreal Quebec))
        (Quebec . (Montreal Halifax))
        (Kingston . (Montreal))))

;; Test the function for *connections-function*
(if (isfunctionp *connections-function*)
    (format t "The variable *connections-function* corresponds to a function.~%")
    (format t "The variable *connections-function* does not correspond to a function.~%"))

;; Example 2: Variable does not correspond to a function
(setq *connections-nonfunction* '(Montreal Ottawa Toronto))

;; Test the function for *connections-nonfunction*
(if (isfunctionp *connections-nonfunction*)
    (format t "The variable *connections-nonfunction* corresponds to a function.~%")
    (format t "The variable *connections-nonfunction* does not correspond to a function.~%"))
