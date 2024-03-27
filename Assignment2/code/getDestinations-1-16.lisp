(defun getDestinations (city connections)
  "Retrieve the destinations for a given city from the connections."
  (cdr (assoc city connections)))

;; Example usage:
(defvar *connections*
  '((Montreal . (Ottawa Kingston Quebec Halifax))
    (Ottawa . (Montreal Toronto))
    (Toronto . (Montreal Ottawa))
    (Halifax . (Montreal Quebec))
    (Quebec . (Montreal Halifax))
    (Kingston . (Montreal))))

;; Get destinations for Montreal
(format t "Destinations for Montreal: ~a~%" (getDestinations 'Montreal *connections*))

;; Get destinations for Ottawa
(format t "Destinations for Ottawa: ~a~%" (getDestinations 'Ottawa *connections*))
