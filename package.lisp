;;;; package.lisp

(defpackage #:sota
  (:use #:cl)
  (:export :get-peak
	   :show-association
	   :get-association-list
	   :get-association-alist
	   :get-associations
	   :make-sota-spot
	   :make-sota-peak
	   :*sleep-between-fetches*
	   :*spots*
	   :*spot-lock*
	   :*spot-thread*
	   :timestamp
	   :comment
	   :callsign
	   :summit
	   :summit-url
	   :area
	   :freq
	   :band
	   :mode
	   :processed
	   :age
	   :start-spotter
	   :spot-hash-key
	   :pp))
