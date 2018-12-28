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

;; asignaciones generales
(defparameter *input-device-name* nil)
(defparameter *input-gateway* nil)
(defparameter *input-netmask* nil)


(defun check-root ()
  "Verifica si eres usuario root"
  (if (not (equal (sb-posix:geteuid) 0))
      (progn
	(princ "Este script funciona sólo con el usuario root.")
	(sb-ext:exit))))

(defun ip-p (string)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "^([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})$"
       string)
    (let ((numbers (mapcar #'read-from-string (coerce groups 'list))))
      (and match
	   (every #'(lambda (num)
		      (<= 0 num 255))
		  numbers)))))

(defparameter *nics* (ip-interfaces:get-ip-interfaces))

(defun get-hostname ()
  "Presenta el hostname, si la elección es vacía, se cambia el nombre del host por el
ingresado."
  (format t "Ingrese el nombre del host: [~A]~%" (inferior-shell:run/ss "hostname -s"))
  (let ((hostname (read-line)))
    (if (not (equal hostname ""))
	(inferior-shell:run/ss `(hostnamectl set-hostname ,hostname)))))

(defun your-devices-and-ipv4 ()
  (combine-cars (interface-names)
		(mapcar #'vec-to-ipv4str (interface-address))))

(defun extract-ips (lst)
  "Ingresa una lista con sublistas, imprimo el valor de las sublistas
de forma editada (ver `format')"
  (if (equal nil lst)
      nil
      (cons (let ((element (car lst)))
	      (format t "~a: ~a ~%" (car element) (cadr element)))
	    (extract-ips (cdr lst)))))

(defun range (start end)
  "Imprime una lista con números entre `start' y `end'."
  (if (equal start end)
      nil
      (cons start (range (1+ start) end))))

(defun interface-names ()
  "Imprime los nombres de las interfaces de red."
  (let ((interfaces (ip-interfaces:get-ip-interfaces)))
    (mapcar (lambda (x)
	      (ip-interfaces:ip-interface-name
	       (nth x interfaces)))
	    (range 0 (length interfaces)))))

(defun interface-address ()
  "Imprime la dirección de las interfaces de red."
  (let ((interfaces (ip-interfaces:get-ip-interfaces)))
    (mapcar (lambda (x)
	      (ip-interfaces:ip-interface-address
	       (nth x interfaces)))
	    (range 0 (length interfaces)))))

(defun combine-cars (lst1 lst2)
  "Combina el `car' de dos listas recursivamente y
retorna una lista."
  (if (equal nil lst1)
      nil
      (cons (cons (car lst1)
		  (cons (car lst2) nil))
	    (combine-cars (cdr lst1)
			  (cdr lst2)))))

;; Vector to string IPv4
;; De entrada asumiré que el vector corresponde a una IPv4
(defun vector-to-lst-dot (vec)
  "Ingresa un vector y retorna una lista con sublistas del elemento
más un punto (.)."
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
  "Retorna una lista de strings de los elementos del vector con puntos
entre ellos."
  (let ((flat-lst (alexandria:flatten (vector-to-lst-dot vec))))
    (reverse
     (cdr (reverse flat-lst)))))

(defun vec-to-ipv4str (vec)
  "Ingresa un vector con una dirección IPv4 y retorna un string del mismo."
  (format nil "~{~A~}" (vec-to-dot-string vec)))

;; Para verificar si una interfaz de red ingresada existe o no.
(defun match? (elem lst)
  "Verifica si el elemento `elem' es `equal' a alguno de los elementos de
`lst'"
  (if (equal nil lst)
      nil
      (cons (equal elem (car lst))
	    (match? elem (cdr lst)))))

(defun add-or (lst)
  "Agrega un `or' al final de la lista."
  (if (equal nil lst)
      (cons 'or nil)
      (cons (car lst)
	    (add-or (cdr lst)))))

(defmacro first-true (elem lst)
  "Verifica si `elem' está contenido en `lst', retornando un booleano."
  `(eval (reverse
	  (add-or
	   (match? ,elem ,lst)))))

;; Chequeo de device
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun input-device-name ()
  "Verifica si el dispositivo ingresado es válido sino pregunta nuevamente
y modifica la variable global *input-device-name*."
  (format t "Ingrese el nombre del dispositivo de red.~%")
  (let ((capture (read-line)))
    (if (first-true capture (interface-names))
	(setf *input-device-name* capture))
    (while (not (first-true capture (interface-names)))
      (format t "Por favor ingrese un nombre de interfaz de red existente en su equipo.~%")
      (extract-ips (your-devices-and-ipv4))
      (let ((nw-capture (read-line)))
	(setf capture nw-capture)
	(setf *input-device-name* nw-capture)))))

(defun input-gateway ()
  "Verifica si la IPv4 ingresada es válida, sino pregunta nuevamente y modifica
la variable global *input-gateway*."
  (format t "Ingrese la IPv4 del gateway.~%")
  (let ((capture (read-line)))
    (if (ip-p capture)
	(setf *input-gateway* capture))
    (while (not (ip-p capture))
      (format t "Por favor ingrese una IPv4 válida.~%")
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
  "AUX: Agrega `string' y `concatenate' al final de la lista"
  (if (equal nil lst)
      (cons ''string
	    (cons 'concatenate nil))
      (cons (car lst)
	    (add-concatenate (cdr lst)))))

(defun add-dot (lst)
  "AUX: Agrega un punto entre los elementos de la lista."
  (if (equal nil lst)
      nil
    (cons
     (cons (car lst)
	   (cons "." nil))
     (add-dot (cdr lst)))))

(defmacro reverse-zone (ipv4-str)
  "Evalúa la función `concatenate' agregada como dato
y genera la zona inversa."
  `(eval
    (reverse
     (add-concatenate
      (nconc '("in-addr.arpa.") '(".")
	     (mapcar #'(lambda (x)
			 (elt (reverse
			       (cdr
				(reverse
				 (alexandria:flatten
				  (add-dot
				   (cl-ppcre:split "\\." ,ipv4-str))))))
			      x))
		     (range 0 5)))))))
