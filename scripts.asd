#-asdf3.1 (error "ASDF 3.1 or bust!")

(defsystem "scripts"
  :version "0.1.0"
  :description "Small scripts from bash or python to common lisp"
  :license "GPL-3.0"
  :author "Innaky"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
	       (:version "inferior-shell" "2.0.3.3")
	       (:version "fare-utils" "1.0.0.5")
	       (:version "ip-interfaces" "0.2.0")
	       "scripts/freeipa-master"))
