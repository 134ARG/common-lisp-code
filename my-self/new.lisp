(defun filter-length (lengths lst)
  (labels ((satisfy? (current lst)
             (cond ((zerop current)
                    (if (null lst) length (iter length lst)))
                   (t (aif (max->= current lst)
			   (iter (- current it) (remove it lst :count 1)
				 nil)))))
     (iter length lst))))

(defmacro aif (condition then-part &optional else-part)
  `(let ((it ,condition))
     (if it ,then-part ,else-part)))

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

