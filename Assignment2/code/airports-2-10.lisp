;; Initialize the hash table
(defvar *monitored* (make-hash-table :test 'equal))

;; Add some data to the hash table
(setf (gethash 'YUL *monitored*) 'Montreal)
(setf (gethash 'LCY *monitored*) 'London)
(setf (gethash 'LHR *monitored*) 'London)
(setf (gethash 'MIL *monitored*) 'Milan)
(setf (gethash 'SFO *monitored*) 'San_Francisco)
(setf (gethash 'SDQ *monitored*) 'Santo_Domingo)

(defun UpdateAirport (airport city)
    (cond ((not (key-present airport *monitored*)) (format t "Airport ~a does not exist in hash-table ~a.~%" airport (symbol-name `*monitored*)))
          ((equal city (gethash airport *monitored*)) (format t "Record (~a -> ~a) already exists.~%" airport city))
          (t (format t "Updated (~a -> ~a) to: ~a -> ~a.~%" airport (gethash airport *monitored*) airport city) (setf (gethash airport *monitored*) city))  
        ))

;; Get both values of gethash, bind them to value and found, then return found. The second value returned by gethash is a boolean representing if the key was found.
(defun key-present (key hash-table)
    (multiple-value-bind (_ found) (gethash key hash-table)
    (declare (ignore _)) ;; This line is to suppress the warning about the unused variable _
    found))

(format t "Before Update:~%")
;; Display the initial contents of the hash table
(maphash (lambda (key value) 
                (format t "~a ~a~%" key value))
          *monitored*)

;; Test the function
(UpdateAirport 'YUL 'Montreal)
(UpdateAirport 'ZZZ 'Toronto)
(UpdateAirport 'LCY 'Glasgow)

(format t "After Update:~%")
;; Display the initial contents of the hash table
(maphash (lambda (key value) 
                (format t "~a ~a~%" key value))
          *monitored*)