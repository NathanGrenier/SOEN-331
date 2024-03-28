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

(defun GetAllAirports (city)
    (format t "Getting all airports for city ~a:~%" city)
    (cond ((not (member city (get-values *airports*))) (format t "City ~a does not exist in hash-table ~a.~%" city (symbol-name `*airports*)))
          (t (mapcar (lambda (airport) (format t "~a -> ~a.~%" airport city)) (get-keys-from-value city *airports*)))))

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

(defun display-contents (list title)
    (format t "~a:~%" title)
    (case (type-of list)
        (hash-table (maphash (lambda (key value) (format t "~a ~a~%" key value)) list))
        (cons (mapcar (lambda (value) (format t "~a~%" value)) list))))

(display-contents *airports* "Initial Elements in *airports*")

;; Test the function
(GetAllAirports 'Montreal)
(GetAllAirports 'London)