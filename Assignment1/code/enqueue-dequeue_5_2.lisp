(defparameter stack1 '()) ; Define stack1 as an empty list
(defparameter stack2 '()) ; Define stack2 as an empty list

(defun enqueue (element) ; Define the enqueue function
  (push element stack1)) ; Push the element onto stack1

(defun dequeue () ; Define the dequeue function
  (if (null stack2) ; Check if stack2 is empty
      (progn ; If stack2 is empty
        (setf stack2 (reverse stack1)) ; Transfer elements from stack1 to stack2 and reverse their order
        (setf stack1 '()))) ; Reset stack1 to an empty list
  (pop stack2)) ; Pop and return the top element from stack2

; Test enqueue and dequeue operations
(enqueue 1) ; Enqueue element 1
(enqueue 2) ; Enqueue element 2
(enqueue 3) ; Enqueue element 3

(format t "Dequeued element: ~a~%" (dequeue)) ; Dequeue and print 1
(format t "Dequeued element: ~a~%" (dequeue)) ; Dequeue and print 2
(enqueue 4) ; Enqueue element 4
(format t "Dequeued element: ~a~%" (dequeue)) ; Dequeue and print 3