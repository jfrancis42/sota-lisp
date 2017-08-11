;;;; sota.asd

(asdf:defsystem #:sota
  :description "Screen-scrapes the SOTA web site in place of an API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
	       #:rss
	       #:bordeaux-threads
	       #:read-number
	       #:local-time
	       #:cl-store
	       #:aviation-formulary
	       #:split-sequence
               #:cl-html-parse)
  :serial t
  :components ((:file "package")
               (:file "sota")))

