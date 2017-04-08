;;;; package.lisp

(defpackage #:sota
  (:use #:cl)
  (:import-from :aviation-formulary
		:point-serial-number
		:point-creation-time
		:point-name
		:point-lat
		:point-lon
		:point-datum)
  (:export :get-peak
	   :last-fetch-age
	   :show-association
	   :get-association-list
	   :get-association-alist
	   :get-associations
	   :make-sota-spot
	   :make-sota-peak
	   :get-sota-peak
	   :*sleep-between-fetches*
	   :*spots*
	   :*peaks*
	   :*peak-lock*
	   :*spot-lock*
	   :*spot-thread*
	   :get-peak-by-designator
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
	   :spot-state
	   :peak-state
	   :start-spotter
	   :stop-threads
	   :spot-hash-key
	   :pp))
