;; Initialize the hash table
(defvar *monitored* (make-hash-table :test 'equal))

;; Add some data to the hash table
(setf (gethash 'YUL *monitored*) 'Montreal)
(setf (gethash 'LCY *monitored*) 'London)
(setf (gethash 'LHR *monitored*) 'London)
(setf (gethash 'MIL *monitored*) 'Milan)
(setf (gethash 'SFO *monitored*) 'San_Francisco)
(setf (gethash 'SDQ *monitored*) 'Santo_Domingo)

(defun AddAirport (airport city)
    (cond ((key-present airport *monitored*) (format t "Airport ~a already exists.~%" airport))
          (t (setf (gethash airport *monitored*) city) (format t "Added Airport: ~a.~%" airport))))

;; Get both values of gethash, bind them to value and found, then return found. The second value returned by gethash is a boolean representing if the key was found.
(defun key-present (key hash-table)
    (multiple-value-bind (_ found) (gethash key hash-table)
    (declare (ignore _)) ;; This line is to suppress the warning about the unused variable _
    found))

;; Display the initial contents of the hash table
(maphash (lambda (key value) 
                (format t "~a ~a~%" key value))
          *monitored*)
;; Test the function
(AddAirport 'YUL 'Montreal)
(AddAirport 'LUY 'Montreal)
(AddAirport 'UYL 'Toronto)