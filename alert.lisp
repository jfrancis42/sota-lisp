(ql:quickload :sota)
(ql:quickload :creds)
(ql:quickload :pushover)

; These are parameters you might want to fiddle with. The first is
; probably fine as-is (how far back in time to go for the first
; pass). The second parameter (*pref-assocs*), you'll probably want to
; tweak to your taste. With my crappy antenna, I limit results to
; these regions because that's all I can hear, anyway. YMMV. You can
; get a list of all the regions by loading the sota package and
; running (sota:get-association-list)."
(defparameter *verbose* nil)
(defparameter *how-far-back* 3600)
(defparameter *end-alert-thread* nil)
(defparameter *pref-assocs* (list "VE7" "W0C" "W5N" "W6" "W7I" "W7M" "W7N" "W7O" "W7U" "W7Y" "W7A" "KLA" "KLF" "KLS"))
(defparameter *pref-modes* (list "ssb" "cw"))
(defparameter *pref-bands* (list 80 60 40 30 20 17))
(defparameter *pref-min-dist* 0)
(defparameter *pref-max-dist* 1000)
(defparameter *home-lat* 47.837578)
(defparameter *home-lon* -122.1393)

; Load up the necessary credentials to send pushover messages. The two
; we care about are 'potoken' and 'pouser'.
(creds:load-creds)

; This holds the reference to the alert thread that runs forever.
(defparameter *alert-thread* nil)

(defun print-spot (spot)
  "Print a spot object."
  (sota:pp spot))

(defun get-my-spots (max-age regions bands modes &optional (process t))
  "Get a list of the hash keys corresponding to the spots I care
about (given the supplied list of associations). Note that a lock must
be held on sota:*spots* whenever this hash is accessed (the lock is
named sota:*spot-lock*). In the case of this code, that lock is held
by the function that calls this function (named send-new-spots). Note
that if you use this get-my-spots function in your own code, you'll
need to wrap it in a (bt:with-lock-held (sota:*spot-lock*) ), else you
risk inconsistent results. If you specify an optional third argument
as nil, this function will not set the processed flag in the spot
objects (useful for taking a peek at the data) and will also display
spots that have already been sent. Example:

(get-my-spots *how-far-back* *pref-assocs* *pref-bands* *pref-modes* nil)
(get-my-spots *how-far-back* (sota:get-association-list) *pref-bands* *pref-modes* nil)
"
  (remove nil
	  (mapcar
	   (lambda (n)
	     (when (and
		    (member
		     (sota:area (gethash n sota:*spots*))
		     regions :test 'equal)
		    (member
		     (sota:mode (gethash n sota:*spots*))
		     modes :test 'equal)
		    (member
		     (sota:band (gethash n sota:*spots*))
		     bands)
		    (<=
		     (sota:age (gethash n sota:*spots*))
		     max-age)
		    (if process
			(not (sota:processed (gethash n sota:*spots*)))
			t))
	       (when process
		 (setf (sota:processed (gethash n sota:*spots*)) t))
	       n))	    
	   (let ((keys nil))
	     (maphash
	      (lambda (key value)
		(declare (ignore value))
		(setf keys (cons key keys)))
	      sota:*spots*)
	     keys))))

(defun send-pushover-message (message sound)
  "Send the actual message to the user."
  (pushover:send-pushover
   (creds:get-cred "potoken")
   (creds:get-cred "pouser")
   message
   :sound sound))

(defun send-new-spots (assocs bands modes)
  "Send all of the new spots that match the user's criteria."
  (bt:with-lock-held (sota:*spot-lock*)
    (mapcar (lambda (n)
	      (unless (null n)
		(when *verbose*
		  (print (sota:spot-hash-key (gethash n sota:*spots*))))
		(if (equal (sota:mode (gethash n sota:*spots*)) "ssb")
		    (send-pushover-message
		     (sota:spot-hash-key
		      (gethash n sota:*spots*)) :cosmic)
		    (send-pushover-message
		     (sota:spot-hash-key
		      (gethash n sota:*spots*)) :spacealarm))))
	    (get-my-spots *how-far-back* assocs bands modes))))

(defun send-new-spots-thread ()
  "This is the thread that runs forever sending spots (marking each
one as sent once it's passed to pushover)."
  (setf sota:*spots* (make-hash-table :test #'equal))
  (loop
     (when *end-alert-thread* (return t))
     (send-new-spots *pref-assocs* *pref-bands* *pref-modes*)
     (sleep 60)))

(defun stop-alerts ()
  "Stop the spotter thread."
  (print "Stopping alert thread...")
  (setf *end-alert-thread* t)
  (bt:join-thread *alert-thread*))

(defun start-alerts ()
  "Start the spotter thread."
  (sota:start-spotter :rss)
  (setf *alert-thread* (bt:make-thread
			(lambda () (send-new-spots-thread))
			:name "sota-alert")))

(defun start-bin ()
  "Start the spotter thread (used only when creating a binary)."
  (setf *verbose* t)
  (sota:start-spotter :rss)
  (send-new-spots-thread))

(defun make-bin (name)
  "Write an executable compiled version of the code to disk with the
specified name."
  (sb-ext:save-lisp-and-die name
			    :toplevel #'start-bin
			    :executable t
			    :purify t
			    :compression 9))
