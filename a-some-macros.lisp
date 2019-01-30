(defmacro nlet (n letargs &rest body)
  "A let of scheme."
  `(labels ((,n ,(mapcar #'car letargs)
	      ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defmacro when (test &rest body)
  `(if ,test
       (progn ,@body)))

(defmacro unless (test &rest body)
  `(if (not ,test)
       (progn ,@body)))
