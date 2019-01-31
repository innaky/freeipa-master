(defun my-map (fn lst)
  "my `map' function."
  (if (equal lst nil)
      nil
      (cons (funcall fn (car lst))
	    (my-map fn (cdr lst)))))

(my-map #'(lambda (x)
	    (* x x)) '(0 1 2 3 4 5 6 7 8 9))
