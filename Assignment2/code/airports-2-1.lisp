;; Initialize the hash table
(defvar *airports* (make-hash-table :test 'equal))

;; Add some data to the hash table
(setf (gethash 'YUL *airports*) 'Montreal)
(setf (gethash 'LCY *airports*) 'London)
(setf (gethash 'LHR *airports*) 'London)
(setf (gethash 'MIL *airports*) 'Milan)
(setf (gethash 'SFO *airports*) 'San_Francisco)
(setf (gethash 'SDQ *airports*) 'Santo_Domingo)

;; Display the contents of the hash table
(maphash (lambda (key value) 
                (format t "~a ~a~%" key value))
          *airports*)