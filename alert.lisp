(ql:quickload :sota)
(ql:quickload :creds)
(ql:quickload :pushover)

; These are paremeters you might want to fiddle with. The first is
; probably fine as-is (how far back in time to go for the first
; pass). The second parameter (*favorites*), you'll probably want to
; tweak to your taste. With my crappy antenna, I limit results to
; these regions because that's all I can hear, anyway. YMMV. You can
; get a list of all the regions by loading the sota package and
; running (sota:get-associations)."
(defparameter *how-far-back* 3600)
(defparameter *favorites* (list "VE7" "W0C" "W5N" "W6" "W7I" "W7M" "W7N" "W7O" "W7U" "W7Y" "W7A" "KLA" "KLF" "KLS"))

; Load up the necessary credentials to send pushover messages. The two
; we care about are 'potoken' and 'pouser'.
(creds:load-creds)

; This "holds" the alert thread that runs forever.
(defparameter *alert-thread* nil)

(defun print-spot (spot)
  "Print a spot object."
  (sota:pp spot))

(defun get-my-spots (max-age regions &optional (process t))
  "Get a list of the hash keys corresponding to the spots I care
about (given the supplied list of associations). Note that a lock must
be held on sota:*spots* whenever this hash is accessed (the lock is
named sota:*spot-lock*). In the case of this code, that lock is held
by the function that calls this function (named send-new-spots). Note
that if you use this get-my-spots function in your own code, you'll
need to wrap it in a (bt:with-lock-held (sota:*spot-lock*) ), else you
risk inconsistent results. If you specify an optional third argument
as nil, this function will not set the processed flag in the spot
objects (useful for taking a peek at the data)."
  (remove nil
	  (mapcar (lambda (n)
		    (when (and
			   (member (sota:area (gethash n sota:*spots*)) regions :test 'equal)
			   (<= (sota:age (gethash n sota:*spots*)) max-age)
			   (not (sota:processed (gethash n sota:*spots*))))
		      (when process (setf (sota:processed (gethash n sota:*spots*)) t))
		      n))	    
		  (let ((keys nil)) (maphash (lambda (key value) (setf keys (cons key keys))) sota:*spots*) keys))))

(defun send-pushover-message (message)
  "Send the actual message to the user."
  (pushover:send-pushover
   (creds:get-cred "potoken")
   (creds:get-cred "pouser")
   message
   :sound :spacealarm))

(defun send-new-spots (favorites)
  "Send all of the new spots that match the user's criteria."
  (bt:with-lock-held (sota:*spot-lock*)
    (mapcar (lambda (n)
	      (unless (null n)
		;(print (sota:spot-hash-key (gethash n sota:*spots*)))
		(send-pushover-message (sota:spot-hash-key (gethash n sota:*spots*)))))
	    (get-my-spots *how-far-back* favorites))))

(defun get-association-list ()
  "Return a list of all current associations (as specified by the SOTA
mapping page). For use when you want all results instead of just a
favorites list."
  (mapcar (lambda (n) (first n)) (sota:get-associations)))

(defun send-new-spots-thread ()
  "This is the thread that runs forever sending spots (marking each
one as sent once it's passed to pushover)."
  (setf sota:*spots* (make-hash-table :test #'equal))
  (loop
     (send-new-spots *favorites*)
     (sleep 60)))

(defun start-alerts ()
  "Start the spotter thread."
  (sota:start-spotter)
  (setf *alert-thread* (bt:make-thread
			(lambda () (send-new-spots-thread))
			:name "sota-alert")))
