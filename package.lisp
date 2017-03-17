;;;; package.lisp

(defpackage #:sota
  (:use #:cl)
  (:export :get-peak
	   :get-association-list
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
	   :show-all-spots
	   :show-spots
	   :spot-hash-key
	   :pp))
