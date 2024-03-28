;; Initialize the hash table
(defvar *airports* (make-hash-table :test 'equal))

;; Add some data to the hash table
(when (zerop (hash-table-count *airports*)) ;; This line makes sure that the hash table is empty before adding data
    (setf (gethash 'YUL *airports*) 'Montreal)
    (setf (gethash 'LCY *airports*) 'London)
    (setf (gethash 'LHR *airports*) 'London)
    (setf (gethash 'MIL *airports*) 'Milan)
    (setf (gethash 'SFO *airports*) 'San_Francisco)
    (setf (gethash 'SDQ *airports*) 'Santo_Domingo))

;; Initialize the monitored airports
(defvar *monitored* (list `YUL `LCY `LHR `MIL `SFO `SDQ))

(defun DeleteAirport (airport)
    (cond ((not (includes-airport airport *monitored*)) (format t "Airport ~a does not exist in list ~a.~%" airport (symbol-name `*monitored*)))
          (t (progn (format t "Deleted element ~a -> ~a.~%" airport (gethash airport *airports*)) 
                    (remhash airport *airports*)
                    (setf *monitored* (remove airport *monitored*))
                    ))))

(defun includes-airport (airport monitored)
    (member airport monitored))

(defun display-contents (list title)
    (format t "~a:~%" title)
    (case (type-of list)
        (hash-table (maphash (lambda (key value) (format t "~a ~a~%" key value)) list))
        (cons (mapcar (lambda (value) (format t "~a~%" value)) list))))

(display-contents *airports* "Value of *airports* Before Delete")

;; Test the function
(DeleteAirport 'ZZZ)
(DeleteAirport 'YUL)

(display-contents *airports* "Values of *airports* After Delete")
(display-contents *monitored* "Values of *monitored* After Delete")