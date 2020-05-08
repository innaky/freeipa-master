;(in-package :cl-user)
(defpackage freeipa-master
  (:use :cl)
  (:export :main))

(in-package :freeipa-master)

;; General asignations
(defparameter *input-device-name* nil)
(defparameter *input-gateway* nil)
(defparameter *input-netmask* nil)
(defparameter *ip-server* nil)
(defparameter *domain-realm* nil)
(defparameter *input-hostname* nil)
(defparameter *ipa-password* nil)
(defparameter *ipa-parameters* nil)
(defparameter *dns-forward* nil)


(defun check-root ()
  "Check if you are the root"
  (if (not (equal (sb-posix:geteuid) 0))
      (progn
	(princ "This script run only with root privileges.")
	(sb-ext:exit))))

(defun ip-p (string)
  "Verify if the input match with IPv4."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "^([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})\.([0-9]{0,3})$"
       string)
    (let ((numbers (mapcar #'read-from-string (coerce groups 'list))))
      (and match
	   (every #'(lambda (num)
		      (<= 0 num 255))
		  numbers)))))

(defun shell (lst)
  (mapcar (lambda (command)
	    (inferior-shell:run/ss
	     command))
	  lst))

(defun set-hostname ()
  (format t "Setting the hostname~%")
  (shell
   `((hostnamectl set-hostname ,*input-hostname*))))

(defun get-hostname ()
  "Print the host name, if the entry is not empty, change the name of the host by the entered."
  (format t "Input the hostname: [~A]~%" (inferior-shell:run/ss "hostname"))
  (let ((hostname (read-line)))
    (if (not (equal hostname ""))
	(progn
	  (setf *input-hostname* hostname)
	  (set-hostname))
	(progn
	  (setf *input-hostname* (inferior-shell:run/ss "hostname"))
	  (set-hostname)))))

(defun range (start end)
  "Create a list with values between `start' and `end'"
  (if (equal start end)
      nil
      (cons start (range (1+ start) end))))

(defun vector-to-lst-dot (vec)
  "Return a list with sublist. This sublist contains the vector element in string format and a dot in string format."
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
  (format nil "~{~A~}" (vec-to-dot-string vec)))

(defun combine-cars (lst lst1)
  (mapcar #'(lambda (a b)
	      (list a b)) lst lst1))

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

(defun or-match? (elem lst)
  (car
   (or (alexandria:flatten (match? elem lst)))))

(defun concat (&rest strings)
  "Concatenate multiple strings."
  (apply #'concatenate 'string strings))

(defun elem-dot (lst)
  "Transform `lst' in sublists, that contains element and
a point in string format."
  (if (equal nil lst)
      nil
    (cons
     (cons (car lst)
	   (cons "." nil))
     (elem-dot (cdr lst)))))

(defun add-dot (lst)
  "Add dots between the elements of the `lst'."
  (alexandria:flatten
   (elem-dot lst)))

(defun delete-last-elem (lst)
  "Generates a new list, without the last element."
  (reverse
   (cdr (reverse lst))))

(defun subnet (str)
  "Input a IPv4 and output an IPv4 with the last octet set to 0."
  (apply #'concat
	 (alexandria:flatten
	  (concatenate 'list
		       (add-dot
			(delete-last-elem
			 (cl-ppcre:split "\\." str)))
		       '("0")))))

;; Check the device IPv4
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun input-device-name ()
  "Receives a input from the user and check if the device exist,
   else return egain the question. This function modify the
   global variable *input-device-name*"
  (format t "Input the name of the net device:~%")
  (let ((capture (read-line)))
    (if (or-match? capture (interface-names))
	(setf *input-device-name* capture))
    (while (not (or-match? capture (interface-names)))
      (format t "Please, Input a valid net device name:~%")
      (extract-ips (your-devices-and-ipv4))
      (let ((nw-capture (read-line)))
	(setf capture nw-capture)
	(setf *input-device-name* nw-capture)))))

(defmacro net-conf (first-string generic-var)
  "A abstraction for capture network configs."
  `(let ((capture (read-line)))
     (if (ip-p capture)
	 (setf ,generic-var capture))
     (while (not (ip-p capture))
       (format t (concat ,first-string "~%"))
       (let ((nw-capture (read-line)))
	 (setf capture nw-capture)
	 (setf ,generic-var nw-capture)))))

(defun ipv4-config ()
  "Main for IPv4 configuration."
  (format t "################### IPv4 configuration ################~%")
  (format t "That is your devices and IPv4 configuration~%")
  (extract-ips (your-devices-and-ipv4))
  (input-device-name))

;; domain and realm

(defun domain ()
  "Capture the domain and set *domain-realm*."
  (format t "Input the domain name: ~%")
  (let ((capture (read-line)))
    (if (stringp capture)
	(setq *domain-realm* capture))))

;; Calculate reverse zone

(defmacro first-oct-ipv4 (ipv4-str)
  "Input a IPv4 in string format, and output a list
   with the first three elements of the Ipv4."
  `(mapcar #'(lambda (x)
	       (elt
		(delete-last-elem
		  (add-dot (cl-ppcre:split "\\." ,ipv4-str)))
		x))
	   (range 0 5)))

(defun reverse-zone (ipv4)
  "Calculate the reverse zone of a IPv4."
  (apply #'concatenate 'string
	 (alexandria:flatten
	  (concatenate 'list
		       (reverse
			(first-oct-ipv4 ipv4))
		       '(".")
		       '("in-addr.arpa.")))))

;; add in the last files

(defun add-str-in-last-file (filename str)
  "Add `str' in the end of the `filename'."
  (with-open-file (stream filename :direction :output
			  :if-exists :append)
    (format stream "~A~%" str)))

(defun hosts ()
  "Set /etc/hosts"
  (add-str-in-last-file "/etc/hosts"
			(concat *ip-server* " " *input-hostname*)))

(defun ntp-set-file ()
  "Set the /etc/ntp.conf"
  (add-str-in-last-file "/etc/ntp.conf"
			(concat (subnet *ip-server*) " netmask " *input-netmask*)))

(defun resolv-set-file ()
  "Set the /etc/resolv.conf"
  (with-open-file (str "/etc/resolv.conf"
		       :direction :output
		       :if-exists :supersede)
    (format str "~A~%" (concat "search " *domain-realm*))
    (format str "~A~%" (concat "nameserver " *ip-server*))))

;; system commands

(defun update-centos ()
  (format t "Updating CentOS 7...~%")
  (shell '((yum -y update)
	   (yum -y upgrade))))

(defun firewall-conf (lst-ports)
  "`x' it's equal to (for example) --add-port=53/tcp, lst-ports contain multiple
from them"
  (mapcar (lambda (x)
	    (shell
	     `((firewall-cmd --permanent ,x))))
	  lst-ports))

(defun firewall-conf-ports (ports-protocol)
  "To reduce the input this function concatenates the
string --add-port= with `ports-protocol'"
  (mapcar (lambda (port)
	    (concat "--add-port=" port))
	  ports-protocol))

(defun firewall-conf-services (services)
  "To reduce the input this function concatenates the
string --add-service with `services'"
  (mapcar (lambda (service)
	    (concat "--add-service=" service))
	  services))

(defun os-install-ipa ()
  (format t "Installing FreeIPA OS packages...~%")
  (shell
   '((yum -y update)
     (yum -y install ipa-server ipa-server-dns bind bind-utils bind-dyndb-ldap rng-tools vim))))

(defun nss-edit ()
  (progn
    (sb-ext:run-program
     "/bin/sed" '("-i" "-e" "s/^NSSProtocol.*/NSSProtocol TLSv1\.0,TLSv1\.1/g"
		  "/etc/httpd/conf.d/nss.conf"))
    (shell
     '((systemctl restart httpd)))))

(defun set-reverse-zone ()
  (shell
   `((ipa dnszone-mod ,(reverse-zone *ip-server*) --allow-sync-ptr=TRUE))))

(defun rngd-serv ()
  (shell
   '((systemctl start rngd)
     (systemctl enable rngd))))

(defun ntpd-serv ()
  (shell
   '((systemctl start ntpd)
     (systemctl enable ntpd))))

(defun ipa-password ()
  (format t "Input the password for IPA admin: ~%")
  (let ((capture (read-line)))
    (if (stringp capture)
	(setq *ipa-password* capture))))

(defun dns-forwarder ()
  (format t "Input the DNS forwarder for IPA: ~%")
  (let ((capture (read-line)))
    (if (ip-p capture)
	(setq *dns-forward* capture))))

(defun ipa-install ()
  (let* ((params (list *ipa-password* *domain-realm* *input-hostname* *dns-forward*))
	(dspass (concat "--ds-password=" (car params)))
	(adminpass (concat "--admin-password=" (car params)))
	(domain (concat "--domain=" (cadr params)))
	(realm (concat "--realm=" (string-upcase (cadr params))))
	(lhost (concat "--hostname=" (third params)))
	(forward (concat "--forwarder=" (car (last params)))))
    (sb-ext:run-program
     "/usr/sbin/ipa-server-install"
     (list dspass adminpass domain realm lhost "--setup-dns" "--auto-reverse" forward "--unattended") :output t)))

(defun ipa-dnszone ()
  (let ((param (list *domain-realm* *ip-server*)))
    (sb-ext:run-program
     "/usr/bin/ipa"
     `("dnszone-mod" ,(reverse-zone (cadr param)) "--allow-sync-ptr=TRUE") :output t)))

(defun dnsconfig-mod ()
  (sb-ext:run-program
   "/usr/bin/ipa" `("dnsconfig-mod" "--allow-sync-ptr=TRUE") :output t))

(defun ticket ()
  "Generate a kerberos and run for config DNS parameters."
  (let ((pass *ipa-password*))
    (inferior-shell:run
     `(inferior-shell:pipe
       (echo ,pass)
       (kinit admin)))
    (ipa-dnszone)
    (dnsconfig-mod)))

(defun main ()
  (check-root)
  (get-hostname)
  (domain)
  (ipv4-config)
  ;; Capture IPv4-server
  (format t "Input the IPv4 for your previously select device: ~%")
  (net-conf "Please, input a valid IPv4: " *ip-server*)
  ;; IPv4 gateway conf
  (format t "Input the IPv4 of the gateway: ~%")
  (net-conf "Please, input a valid IPv4 for the gateway: " *input-gateway*)
  ;; IPv4 netmask conf
  (format t "Input the IPv4 of the netmask: ~%")
  (net-conf "Please, input a valid IPv4 for the netmask: " *input-netmask*)
  (hosts)
  (format t "Setting firewall-cmd...~%")
  (firewall-conf
   (firewall-conf-ports '("53/tcp" "80/tcp" "111/tcp" "389/tcp"
			  "443/tcp" "464/tcp" "636/tcp" "2049/tcp"
			  "20048/tcp" "53/udp" "88/udp" "111/udp"
			  "123/udp" "464/udp" "2049/udp" "20048/udp")))
  (firewall-conf
   (firewall-conf-services '("http" "https" "ldap" "ldaps" "kerberos"
			     "dns" "ntp" "nfs" "mountd")))
  (shell '((firewall-cmd --reload)))
  (update-centos)
  (os-install-ipa)
  (ntp-set-file)
  (ntpd-serv)
  (rngd-serv)
  (ipa-password)
  (dns-forwarder)
  (ipa-install)
  (nss-edit)
  (shell
   '((systemctl restart httpd)))
  (resolv-set-file)
  (ticket))
