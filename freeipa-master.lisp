;;; with SBCL
;;(uiop:define-package :scripts/freeipa-master
;;    (:use :cl :uiop :optima :optima.ppcre :binascii :cl-scripting
;;	  :inferior-shell :command-line-arguments :ip-interfaces)
;;  (:export ))

;;#!/usr/bin/sbcl --script

(ql:quickload '(uiop optima optima.ppcre binascii cl-scripting
		inferior-shell command-line-arguments ip-interfaces
		alexandria))

(defpackage :ipa-master
  (:use :cl :ipa-master :uiop :optima :alexandria))
(:export #:main))

(in-package :ipa-master)

;; General asignations
(defparameter *input-device-name* nil)
(defparameter *input-gateway* nil)
(defparameter *input-netmask* nil)


(defun check-root ()
  "Check if you are the root"
  (if (not (equal (sb-posix:geteuid) 0))
      (progn
	(princ "This script run only with root privileges.")
	(sb-ext:exit))))

(defun ip-p (string)
  "AUX: Verify if the input match with IPv4."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "^([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})$"
       string)
    (let ((numbers (mapcar #'read-from-string (coerce groups 'list))))
      (and match
	   (every #'(lambda (num)
		      (<= 0 num 255))
		  numbers)))))

(defun get-hostname ()
  "Print the host name, if the entry is not empty, change the name of the host by the entered."
  (format t "Input the hostname: [~A]~%" (inferior-shell:run/ss "hostname -s"))
  (let ((hostname (read-line)))
    (if (not (equal hostname ""))
	(inferior-shell:run/ss `(hostnamectl set-hostname ,hostname)))))

(defun range (start end)
  "AUX: Create a list with values between `start' and `end'"
  (if (equal start end)
      nil
      (cons start (range (1+ start) end))))

(defun vector-to-lst-dot (vec)
  "AUX: Return a list with sublist. This sublist contains the vector element in string format and a dot in string format."
  (let ((veclst (concatenate 'list vec)))
    (if (equal veclst nil)
	nil
	(cons
	 (cons (write-to-string (car
				 (mapcar (lambda (x)
					   (elt veclst x))
					 (range 0 (length veclst)))))
	       (cons "." nil))
	 (vector-to-lst-dot (cdr veclst))))))

(defun vec-to-dot-string (vec)
  "Transform a vector in a list. The elements of the vectors in string format and among them, a dot in string format."
  (let ((flat-lst (alexandria:flatten (vector-to-lst-dot vec))))
    (reverse
     (cdr (reverse flat-lst)))))

(defun vec-to-ipv4str (vec)
  "Transform a vector (with a IPv4) in a IPv4 string."
  (format nil "~{~A~}" (vec-to-dot-sting vec)))

(defun combine-cars (lst1 lst2)
  "Return a list with sublist, the sublist contain the cars (recursively) of the `lst1' and `lst2'"
  (if (equal nil lst1)
      nil
      (cons (cons (car lst1)
		  (cons (car lst2) nil))
	    (combine-cars (cdr lst1)
			  (cdr lst2)))))

(defun interface-names ()
  "Return a list with the interface names, in string format."
  (let ((interfaces (ip-interfaces:get-ip-interfaces)))
    (mapcar (lambda (x)
	      (ip-interfaces:ip-interface-name
	       (nth x interfaces)))
	    (range 0 (length interfaces)))))

(defun interface-address ()
  "Return a list with vector elements, in ipv4 primitive format."
  (let ((interfaces (ip-interfaces:get-ip-interfaces)))
    (mapcar (lambda (x)
	      (ip-interfaces:ip-interface-address
	       (nth x interfaces)))
	    (range 0 (length interfaces)))))

(defun your-devices-and-ipv4 ()
  "Return a list with sublist, this contains the devicename and the ipv4."
  (combine-cars (interface-names)
		(mapcar #'vec-to-ipv4str (interface-address))))

(defun extract-ips (lst)
  "Return a readable format the interface names and IPv4."
  (if (equal nil lst)
      nil
      (cons (let ((element (car lst)))
	      (format t "~a: ~a ~%" (car element) (cadr element)))
	    (extract-ips (cdr lst)))))

;; Verify if a interface exist.
(defun match? (elem lst)
  "Return a list with boolean values, verify if `elem' exist in `lst'."
  (if (equal nil lst)
      nil
      (cons (equal elem (car lst))
	    (match? elem (cdr lst)))))

(defun add-or (lst)
  "Add `or' in the end of the list."
  (if (equal nil lst)
      (cons 'or nil)
      (cons (car lst)
	    (add-or (cdr lst)))))

(defmacro first-true (elem lst)
  "Verify if `elem' exist in `lst', return a boolean."
  `(eval (reverse
	  (add-or
	   (match? ,elem ,lst)))))

;; Check the device IPv4
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun input-device-name ()
  "Receives a input from the user and check if the device exist, else return egain the question. This function modify the global variable *input-device-name*"
  (format t "Input the name of the net device.~%")
  (let ((capture (read-line)))
    (if (first-true capture (interface-names))
	(setf *input-device-name* capture))
    (while (not (first-true capture (interface-names)))
      (format t "Please, Input a valid net device name.~%")
      (extract-ips (your-devices-and-ipv4))
      (let ((nw-capture (read-line)))
	(setf capture nw-capture)
	(setf *input-device-name* nw-capture)))))

(defun input-gateway ()
  "Verify if the input IPv4 is valid, else return egain the question. This function modify the global variable *input-gateway*"
  (format t "Input the IPv4 of the gateway.~%")
  (let ((capture (read-line)))
    (if (ip-p capture)
	(setf *input-gateway* capture))
    (while (not (ip-p capture))
      (format t "Please, input a valid IPv4.~%")
      (let ((nw-capture (read-line)))
	(setf capture nw-capture)
	(setf *input-gateway* nw-capture)))))

(defun input-netmask ()
  (format t "Ingrese la IPv4 del gateway.~%")
  (let ((capture (read-line)))
    (if (ip-p capture)
	(setf *input-netmask* capture))
    (while (not (ip-p capture))
      (format t "Por favor ingrese una IPv4 válida.~%")
      (let ((nw-capture (read-line)))
	(setf capture nw-capture)
	(setf *input-netmask* nw-capture)))))

;; Calculate reverse zone

(defun add-concatenate (lst)
  "Add `string' and the function `concatenate' in the end of the `lst'."
  (if (equal nil lst)
      (cons ''string
	    (cons 'concatenate nil))
      (cons (car lst)
	    (add-concatenate (cdr lst)))))

(defun add-dot (lst)
  "Add dots between the elements of the `lst'."
  (if (equal nil lst)
      nil
    (cons
     (cons (car lst)
	   (cons "." nil))
     (add-dot (cdr lst)))))

(defmacro first-oct-ipv4 (ipv4-str)
  "Extract the first three elements of a IPv4."
  `(mapcar #'(lambda (x)
	       (elt (reverse
		     (cdr
		      (reverse
		       (alexandria:flatten
			(add-dot
			 (cl-ppcre:split "\\." ,ipv4-str))))))
		    x))
	   (range 0 5)))

(defmacro reverse-zone (ipv4-str)
  "Return the reverse zone for the input `ipv4-str'"
  `(eval
    (reverse
     (add-concatenate
      (reverse
       (alexandria:flatten
	(list
	 (reverse (first-oct-ipv4 ,ipv4-str))
	 '(".")
	 '("in-addr.arpa."))))))))
