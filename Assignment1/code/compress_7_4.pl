(defun compress (lst)
  (if (null lst) ; Base case: empty list
      nil
      (let ((rest (compress (cdr lst)))) ; Recursive call on the rest of the list
        (if (or (null rest) ; If the rest is empty or the current element is different
                (not (equal (car lst) (car rest))))
            (cons (car lst) rest) ; Include the current element in the result
            rest)))) ; Skip the current element and continue compressing the rest