(defmacro nle (n letargs &rest body)
  "A let of scheme."
  `(labels ((,n ,(mapcar #'car letargs)
	      ,@body))
     (,n ,@(mapcar #'cadr letargs))))
