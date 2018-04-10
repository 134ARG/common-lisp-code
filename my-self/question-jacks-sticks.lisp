(defun filter-length (length lst)
  (labels ((iter (current lst)
             (cond ((zerop current)
                    (if (null lst) length (iter length lst)))
                   (t (let ((it (max->= current lst)))
			(when it (iter (- current it) (remove it lst :count 1))))))))
    (iter length lst)))

(defun max->= (elt lst)
  (find-if #'(lambda (x) (<= x elt)) lst))

(defun generate-guesses (lst)
  (let ((sum (apply #'+ lst))
        (max (car lst)))
    (loop for i from (ceiling (/ sum max)) downto 1
          collect (/ sum i))))

(defun solve-jacks-sticks (sticks)
  (let ((sticks-desc (sort sticks #'>)))
    (car (mapcar #'(lambda (x) (filter-length x sticks-desc))
                 (generate-guesses sticks-desc)))))

