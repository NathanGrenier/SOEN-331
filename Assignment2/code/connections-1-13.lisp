(defvar *connections* '())

(defun add-connection (source destinations)
  (push (cons source destinations) *connections*))

(defun initialize-connections ()
  (setq *connections*
        '((Montreal . (Ottawa Kingston Quebec Halifax))
          (Ottawa . (Montreal Toronto))
          (Toronto . (Montreal Ottawa))
          (Halifax . (Montreal Quebec))
          (Quebec . (Montreal Halifax))
          (Kingston . (Montreal)))))

;; Initialize connections
(initialize-connections)

;; Function to display connections with arrows
(defun display-connections-with-arrows ()
  (format t "Connections:~%")
  (dolist (connection *connections*)
    (let ((source (car connection))
          (destinations (cdr connection)))
      (format t "~a -> " source)
      (dolist (destination destinations)
        (format t "~a, " destination))
      (terpri))))

;; Display the contents of connections with arrows
(display-connections-with-arrows)
