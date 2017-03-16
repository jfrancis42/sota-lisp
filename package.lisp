;;;; package.lisp

(defpackage #:sota
  (:use #:cl)
  (:export :get-peak
	   :get-associations
	   :make-sota-spot
	   :make-sota-peak
	   :*sleep-between-fetches*
	   :*spots*
	   :*spot-lock*
	   :*spot-thread*
	   :age
	   :area
	   :start-spotter
	   :processed
	   :show-all-spots
	   :show-spots
	   :spot-hash-key
	   :pp))
