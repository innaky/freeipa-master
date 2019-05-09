#|
  This file is a part of utils project.
  Copyright (c) 2019 Innaky (innaky@protonmail.com)
|#

#|
  Author: Innaky (innaky@protonmail.com)
|#

(defsystem "freeipa-master"
    :version "0.1.1"
    :author "Innaky"
    :license "GPLv3"
    :depends-on (:cl-launch
		 :cl-scripting
		 :inferior-shell
		 :ip-interfaces
		 :alexandria
		 :cl-ppcre)
    :components ((:module "src"
			  :components
			  ((:file "freeipa-master"))))
    :description "Install FreeIPA"
    :long-description
    #.(read-file-string
       (subpathname *load-pathname* "README.md")))
