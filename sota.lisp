;;;; sota.lisp

(in-package #:sota)

(defparameter *age-out* 86400)
(defparameter *spots* (make-hash-table :test #'equal))
(defparameter *spot-thread* nil)
(defparameter *spot-lock* (bt:make-lock))

(defun get-url (url)
  "Fetch the data at the specified URL and parse it."
  (html-parse:parse-html (drakma:http-request url :method :get)))

(defun get-spots ()
  "Get the spots page from the SOTA web page and parse it."
  (rest (rest (rest (fifth (second (fourth (second (get-url "http://www.sotawatch.org/spots.php")))))))))

(defun get-peak (summit-url)
  "Get the info on the specified SOTA peak and parse the result."
  (get-url summit-url))

(defun make-date (thing)
  "Convert the date format to something local-time can parse."
  (let ((stuff (split-sequence:split-sequence #\Space thing)))
    (list (first stuff)
	  (concatenate 'string
		       (fourth stuff)
		       "-"
		       (cond
			 ((equal (third stuff) "Jan") "01")
			 ((equal (third stuff) "Feb") "02")
			 ((equal (third stuff) "Mar") "03")
			 ((equal (third stuff) "Apr") "04")
			 ((equal (third stuff) "May") "05")
			 ((equal (third stuff) "Jun") "06")
			 ((equal (third stuff) "Jul") "07")
			 ((equal (third stuff) "Aug") "08")
			 ((equal (third stuff) "Sep") "09")
			 ((equal (third stuff) "Oct") "10")
			 ((equal (third stuff) "Nov") "11")
			 ((equal (third stuff) "Dec") "12"))
		       "-"
		       (second stuff)))))

(defun get-association-list ()
  "Return a list of all current associations (as specified by the SOTA
mapping page). Note that this function is extremely brittle, and will
need to be fixed if/when SOTA changes their web page."
  (mapcar (lambda (n) (first n)) 
	  (mapcar
	   (lambda (n)
	     (mapcar
	      (lambda (n)
		(string-trim
		 '(#\Space #\Tab #\Newline #\Linefeed) n))
	      (split-sequence:split-sequence #\- (second n))))
	   (rest
	    (rest
	     (seventh
	      (second
	       (second
		(second
		 (second
		  (second
		   (second
		    (sixth
		     (second
		      (second
		       (third
			(fifth
			 (third
			  (second
			   (get-url "https://sotamaps.org/"))))))))))))))))))))

(defun correct-date-for-day (day)
  "The SOTA spots page returns a day/time in GMT for each spot. This
function figures out which date a specified day/combination refers to,
and returns that (ie, it figures out the day name for each of
yesterday, today, and tomorrow, and matches that up with the
appropriate date so that the time can be parsed and converted
properly. This wouldn't be a problem if the SOTA page returned an
actual date."
  (let* ((right-now (local-time:now))
	 (yesterday-thing (local-time:format-timestring nil (local-time:universal-to-timestamp (- (local-time:timestamp-to-universal right-now) 86400)) :timezone local-time:+utc-zone+ :format local-time:+rfc-1123-format+))
	 (today-thing (local-time:format-timestring nil right-now :timezone local-time:+utc-zone+ :format local-time:+rfc-1123-format+))
	 (tomorrow-thing (local-time:format-timestring nil (local-time:universal-to-timestamp (+ (local-time:timestamp-to-universal right-now) 86400)) :timezone local-time:+utc-zone+ :format local-time:+rfc-1123-format+)))
    (cond
      ((equal day (first (split-sequence:split-sequence #\, yesterday-thing)))
       (second (make-date yesterday-thing)))
      ((equal day (first (split-sequence:split-sequence #\, today-thing)))
       (second (make-date today-thing)))
      ((equal day (first (split-sequence:split-sequence #\, tomorrow-thing)))
       (second (make-date tomorrow-thing)))
      (t nil))))

; A SOTA spot object.
(defclass sota-spot ()
  ((timestamp :accessor timestamp
	      :initarg :timestamp
	      :initform nil)
   (comment :accessor comment
	    :initarg :comment
	    :initform nil)
   (callsign :accessor callsign
	     :initarg :callsign
	     :initform nil)
   (summit :accessor summit
	   :initarg :summit
	   :initform nil)
   (summit-url :accessor summit-url
	       :initarg :summit-url
	       :initform nil)
   (area :accessor area
	 :initarg :area
	 :initform nil)
   (freq :accessor freq
	 :initarg :freq
	 :initform nil)
   (mode :accessor mode
	 :initarg :mode
	 :initform nil)
   (processed :accessor processed
	      :initarg :processed
	      :initform nil)))

(defun make-sota-spot (thing)
  "Create a SOTA spot object from parsed HTML. Note that this function
is very brittle, and will break horribly if/when the HTML in the SOTA
spot page changes."
  (when (listp thing)
    (when (equal :table (first (first thing)))
      (let* ((timestamp-thing (split-sequence:split-sequence #\Space (second (second (second (second thing))))))
	     (summit-thing (split-sequence:split-sequence #\/ (second (second (fourth (third (second thing)))))))
	     (callsign-thing (split-sequence:split-sequence #\/ (second (second (third (second thing))))))
	     (freq-mode (split-sequence:split-sequence #\Space (second (second (fourth (second thing)))))))
	(make-instance 'sota-spot
		       :timestamp (local-time:parse-timestring (concatenate 'string (correct-date-for-day
										     (first timestamp-thing))
									    "T"
									    (second timestamp-thing)
									    ":00Z"))
		       :comment (second (second (third (third thing))))
		       :callsign (string-upcase (if (equal 3 (length callsign-thing))
						    (second callsign-thing)
						    (first callsign-thing)))
		       :summit (second summit-thing)
		       :summit-url (fifth (first (fourth (third (second thing)))))
		       :area (first summit-thing)
		       :freq (with-input-from-string (in (first freq-mode)) (read-number:read-float in))
		       :mode (string-downcase (second freq-mode)))))))

(defmethod age ((s sota-spot))
  "Calculate the age of a SOTA spot in seconds."
  (- (local-time:timestamp-to-universal (local-time:now)) (local-time:timestamp-to-universal (timestamp s))))

(defmethod band ((s sota-spot))
  "Calculate the band (160m-2m), based on the frequency (for US
bands). Returns the band as an integer. nil if it's lower than 160m,
higher than 2m, or falls outside of a valid band."
  (let ((f (freq s)))
    (cond
      ((and (>= f 1.8)
	    (<= f 2.0))
       160)
      ((and (>= f 3.5)
	    (<= f 4.0))
       80)
      ((and (>= f 5.3)
	    (<= f 5.5))
       60)
      ((and (>= f 7.0)
	    (<= f 7.3))
       40)
      ((and (>= f 10.1)
	    (<= f 10.15))
       30)
      ((and (>= f 14.0)
	    (<= f 14.35))
       20)
      ((and (>= f 18.068)
	    (<= f 18.168))
       17)
      ((and (>= f 21.0)
	    (<= f 21.45))
       15)
      ((and (>= f 24.89)
	    (<= f 24.99))
       12)
      ((and (>= f 28.0)
	    (<= f 29.7))
       10)
      ((and (>= f 50.0)
	    (<= f 54.0))
       6)
      ((and (>= f 144.0)
	    (<= f 148.0))
       2)
      (t nil))))

(defmethod pp ((s sota-spot))
  "Pretty print a SOTA spot object."
  (format t "~A ~A ~A ~A ~A ~A ~A ~A~%"
	  (local-time:format-timestring nil (timestamp s) :format local-time:+rfc-1123-format+)
	  (callsign s)
	  (area s)
	  (summit s)
	  (band s)
	  (freq s)
	  (mode s)
	  (age s)))

(defmethod spot-hash-key ((s sota-spot))
  "Create a hash key to refer to this SOTA spot object."
  (format nil "~A ~A ~A ~A ~A ~A ~A"
	  (local-time:format-timestring nil (timestamp s) :format local-time:+rfc-1123-format+)
	  (callsign s)
	  (area s)
	  (summit s)
	  (band s)
	  (freq s)
	  (mode s)))

(defun get-all-spots ()
  "Return a list of all available SOTA spot objects."
  (remove nil (mapcar (lambda (n) (make-sota-spot n)) (get-spots))))

(defun show-spots (care max-age)
  "Return a printed list of SOTA spots <= max-age (in seconds) where
  the region matches one of a list of supplied regions ("W7W", etc)."
  (mapcar
   (lambda (n) (let ((m (make-sota-spot n)))
		 (when (not (null m))
		   (when (and
			  (member (area m) care :test 'equal)
			  (<= (age m) max-age))
		     (pp m)))))
   (get-spots))
  t)

(defun show-all-spots (max-age)
  "Return a printed list of all available SOTA spots with age <=
max-age (in seconds)."
  (mapcar
   (lambda (n) (let ((m (make-sota-spot n)))
		 (when (and
			(not (null m))
			(<= (age m) max-age))
		   (pp m))))
   (get-spots))
  t)

; A SOTA peak object.
(defclass sota-peak ()
  ((peak-designator :accessor peak-designator
		    :initarg :peak-designator
		    :initform nil)
   (peak-name :accessor peak-name
	      :initarg :peak-name
	      :initform nil)
   (association :accessor association
		:initarg :association
		:initarg nil)
   (region :accessor region
	   :initarg :region
	   :initform nil)
   (latitude :accessor latitude
	     :initarg :latitude
	     :initform nil)
   (longitude :accessor longitude
	      :initarg :longitude
	      :initform nil)))

(defun make-sota-peak (thing)
  "Create a SOTA peak object out of parsed HTML. As with the above
parsed HTML functions, this will break if/when the web page changes."
  (make-instance 'sota-peak
		 :peak-designator (first (split-sequence:split-sequence #\, (second (second (second (second (second (third (third (second thing))))))))))
		 :peak-name (second (second (second (second (second (third (third (second thing))))))))
		 :association (string-trim '(#\Space #\Tab #\Newline #\Linefeed) (third (third (second (third (third (second thing)))))))
		 :region (string-trim '(#\Space #\Tab #\Newline #\Linefeed) (fifth (third (second (third (third (second thing)))))))
		 :latitude (with-input-from-string (in (nth 8 (third (second (third (third (second thing))))))) (read in))
		 :longitude (with-input-from-string (in (nth 10 (third (second (third (third (second thing))))))) (read in))))

(defmethod pp ((p sota-peak))
  "Pretty print a SOTA peak object."
  (format t "~A ~A ~A ~A ~A~%"
	  (peak-designator p)
	  (association p)
	  (region p)
	  (latitude p)
	  (longitude p)))

(defun update-spots (&optional (verbose nil))
  "This function uses the sota package to build/update a hash of
spots. If a given spot already exists, the data is discarded. If it's
new, it's added to the hash with the processed field set to
nil."
  (bt:with-lock-held (*spot-lock*)
    (mapcar
     (lambda (n)
       (unless (null n)
	 (if (not (gethash (spot-hash-key n) *spots*))
	     (progn
	       (when verbose (format t "New: ~A~%" (spot-hash-key n)))
	       (setf (gethash (spot-hash-key n) *spots*) n))
	     (when verbose (format t "Duplicate: ~A~%" (spot-hash-key n))))))
     (get-all-spots))
    t))

(defun grim-reaper (&optional (max-age *age-out*))
       (bt:with-lock-held (*spot-lock*)
	 (mapcar
	  (lambda (n) (remhash n *spots*))
	  (let ((keys nil))
	    (maphash
	     (lambda (key value)
	       (when (> (age value) max-age)
		 (setf keys (cons key keys))))
	     *spots*)
	    keys))))

(defun spot-fetcher-thread ()
  "This is the thread that runs forever, fetching data and maintaining
the spot hash."
  (bt:with-lock-held (*spot-lock*)
    (setf *spots* (make-hash-table :test #'equal)))
  (loop
     (update-spots)
     (grim-reaper *age-out*)
     (sleep 60)))

(defun start-spotter ()
  "Start the spotter thread."
  (setf *spot-thread* (bt:make-thread
		       (lambda () (spot-fetcher-thread))
		       :name "sota-spots")))
