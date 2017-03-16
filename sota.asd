;;;; sota.asd

(asdf:defsystem #:sota
  :description "Screen-scrapes the SOTA web site in place of an API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
	       #:bordeaux-threads
	       #:read-number
	       #:local-time
	       #:split-sequence
               #:cl-html-parse)
  :serial t
  :components ((:file "package")
               (:file "sota")))

