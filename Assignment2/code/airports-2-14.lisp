;; Initialize the hash table
(defvar *monitored* (make-hash-table :test 'equal))

;; Add some data to the hash table
(setf (gethash 'YUL *monitored*) 'Montreal)
(setf (gethash 'LCY *monitored*) 'London)
(setf (gethash 'LHR *monitored*) 'London)
(setf (gethash 'MIL *monitored*) 'Milan)
(setf (gethash 'SFO *monitored*) 'San_Francisco)
(setf (gethash 'SDQ *monitored*) 'Santo_Domingo)

(defun GetAllAirports (city)
    (format t "Getting all airports for city ~a:~%" city)
    (cond ((not (member city (get-values *monitored*))) (format t "City ~a does not exist in hash-table ~a.~%" city (symbol-name `*monitored*)))
          (t (mapcar (lambda (airport) (format t "~a -> ~a.~%" airport city)) (get-keys-from-value city *monitored*)))))

;; Get a list of all values in the hash table
(defun get-values (hash-table)
    (let ((values '()))
        (maphash (lambda (key value) 
                     (declare (ignore key)) ;; Ignore the key (removes warning)
                     (push value values)) 
                 hash-table)
        values))

;; Get a list of all keys in the hash table that have a specific value
(defun get-keys-from-value (value hash-table)
    (let ((keys '()))
        (maphash (lambda (key val) (if (equal val value) (push key keys))) hash-table)
        keys))

(format t "Initial Elements:~%")
;; Display the initial contents of the hash table
(maphash (lambda (key value) 
                (format t "~a ~a~%" key value))
          *monitored*)

;; Test the function
(GetAllAirports 'Montreal)
(GetAllAirports 'London)