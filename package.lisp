;;;; package.lisp

(defpackage #:sota
  (:use #:cl)
  (:export :get-peak
	   :last-fetch-age
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
	   :spotter-state
	   :start-spotter
	   :stop-spotter
	   :spot-hash-key
	   :pp))
