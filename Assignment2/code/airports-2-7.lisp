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

(defun AddAirport (airport city)
    (cond ((includes-airport airport *monitored*) 
              (format t "Airport ~a already exists.~%" airport))
        (t (progn (setf (gethash airport *airports*) city) 
                    (push airport *monitored*)
                    (format t "Added Airport: ~a -> ~a.~%" airport city)
                    ))))

(defun includes-airport (airport monitored)
    (member airport monitored))

(defun display-contents (list title)
    (format t "~a:~%" title)
    (case (type-of list)
        (hash-table (maphash (lambda (key value) (format t "~a ~a~%" key value)) list))
        (cons (mapcar (lambda (value) (format t "~a~%" value)) list))))

;; Display the initial contents of the hash table
(display-contents *airports* "Initial Values of *airports*")

;; Test the function
(AddAirport 'YUL 'Montreal)
(AddAirport 'LUY 'Montreal)
(AddAirport 'UYL 'Toronto)

(display-contents *airports* "Values of *airports* After AddAirport(s)")
(display-contents *monitored* "Values of *monitored* After AddAirport(s)")