;;;; sota.lisp

(in-package #:sota)

(defparameter *age-out* 86400)
(defparameter *spots* (make-hash-table :test #'equal))
(defparameter *spot-thread* nil)
(defparameter *spot-lock* (bt:make-lock))
(defparameter *sota-rss* t) ; t to use RSS, nil to scrape the sotawatch HTML
(defparameter *association-cache* nil)

(defun get-raw-url (url)
  "Fetch the data at the specified URL."
  (handler-case (drakma:http-request url :method :get)
    (usocket:ns-host-not-found-error () "")
    (usocket:ns-try-again-condition () "")))

(defun get-parsed-url (url)
  "Fetch the data at the specified URL and parse it."
  (html-parse:parse-html (get-raw-url url)))

(defun join (stuff separator)
  "Join a list of strings with a separator (like ruby string.join())."
  (with-output-to-string (out)
    (loop (princ (pop stuff) out)
       (unless stuff (return))
       (princ separator out))))

(defun get-spots-from-scrape ()
  "Get the spots page from the SOTA web page, parse the HTML, then
extract the bits we want to use."
  (rest (rest (rest (fifth (second (fourth (second (get-parsed-url "http://www.sotawatch.org/spots.php")))))))))

(defun get-spots-from-rss ()
  "Get the spots page from the SOTA RSS page, parse the result, then
return list of parsed RSS entry objects."
  (rss:items (rss:parse-rss-stream (get-raw-url "http://old.sota.org.uk/RssFeed"))))

(defun get-peak-from-scrape (summit-url)
  "Get the info on the specified SOTA peak and parse the result."
  (get-parsed-url summit-url))

(defun parse-scraped-date (thing)
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

(defun parse-rss-date (thing)
  "Convert the date format to something local-time can parse."
  (concatenate 'string
	       (fourth thing) "-"
	       (cond
		 ((equal (third thing) "Jan") "01")
		 ((equal (third thing) "Feb") "02")
		 ((equal (third thing) "Mar") "03")
		 ((equal (third thing) "Apr") "04")
		 ((equal (third thing) "May") "05")
		 ((equal (third thing) "Jun") "06")
		 ((equal (third thing) "Jul") "07")
		 ((equal (third thing) "Aug") "08")
		 ((equal (third thing) "Sep") "09")
		 ((equal (third thing) "Oct") "10")
		 ((equal (third thing) "Nov") "11")
		 ((equal (third thing) "Dec") "12"))
	       "-" (second thing) "T"
	       (fifth thing) "Z"))
	       
(defun get-associations ()
  "Return a list of all current associations (as specified by the SOTA
mapping page) with their associated descriptions, with the association
name as the first item in the list. Some lists have two entries, some
have three. Note that this function is extremely brittle, and will
need to be fixed if/when SOTA changes their web page."
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
		   (get-parsed-url "https://sotamaps.org/")))))))))))))))))))

(defun get-association-alist ()
  "Return all associations as an alist."
  (mapcar (lambda (n) (cons (first n) (list (rest n)))) *association-cache*))

(defun show-association (a)
  "Show the assocation, given the abbreviation."
  (join (second (assoc a (get-association-alist) :test 'equal)) " - "))

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
	      (split-sequence:split-sequence #\- (first n))))
	   *association-cache*)))

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
       (second (parse-scraped-date yesterday-thing)))
      ((equal day (first (split-sequence:split-sequence #\, today-thing)))
       (second (parse-scraped-date today-thing)))
      ((equal day (first (split-sequence:split-sequence #\, tomorrow-thing)))
       (second (parse-scraped-date tomorrow-thing)))
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

(defun make-sota-spot-from-scrape (thing)
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

(defun make-sota-spot-from-rss-item (item)
  "Create a SOTA spot object from a parsed RSS item."
  (let* ((title-thing (split-sequence:split-sequence #\Space (rss:title item)))
	 (summit-thing (split-sequence:split-sequence #\/ (third title-thing)))
	 (description (rss:description item))
	 (timestamp-thing (split-sequence:split-sequence #\Space (rss:pub-date item))))
	(make-instance 'sota-spot
		       :comment description
		       :callsign (first (split-sequence:split-sequence #\/ (first title-thing)))
		       :area (first summit-thing)
		       :summit (second summit-thing)
		       :summit-url (concatenate 'string "http://www.sota.org.uk/Summit/" (first summit-thing) "/" (second summit-thing))
		       :freq (with-input-from-string (in (fifth title-thing)) (read-number:read-float in))
		       :mode (if (equal (length title-thing) 6)
				 (sixth title-thing)
				 "")
		       :timestamp (local-time:parse-timestring (parse-rss-date timestamp-thing)))))

(defmethod age ((s sota-spot))
  "Calculate the age of a SOTA spot in seconds."
  (- (local-time:timestamp-to-universal (local-time:now)) (local-time:timestamp-to-universal (timestamp s))))

(defmethod band ((s sota-spot))
  "Calculate the band (160m-2m), based on the frequency (for US
bands). Returns the band as an integer. nil if it's lower than 160m,
higher than 2m, or falls outside of a valid band. It's a little vague
on 60M (to save code), but works fine in practice."
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

(defun get-all-spots-via-scraping ()
  "Return a list of all available SOTA spot objects by scraping the
sotawatch HTML."
  (remove nil (mapcar (lambda (n) (make-sota-spot-from-scrape n)) (get-spots-from-scrape))))

(defun get-all-spots-via-rss ()
  "Return a list of all available SOTA spot objects from the sotawatch
RSS feed.."
  (remove nil (mapcar (lambda (n) (make-sota-spot-from-rss-item n)) (get-spots-from-rss))))

; A SOTA peak object.
(defclass sota-peak (af:2d-point)
  ((designator :accessor designator
	       :initarg :designator
	       :initform nil)
   (name :accessor name
	 :initarg :name
	 :initform nil)
   (association :accessor association
		:initarg :association
		:initarg nil)
   (region :accessor region
	   :initarg :region
	   :initform nil)))

(defun make-sota-peak (thing)
  "Create a SOTA peak object out of parsed HTML. As with the above
parsed HTML functions, this will break if/when the web page changes."
  (make-instance 'sota-peak
		 :designator (first (split-sequence:split-sequence #\, (second (second (second (second (second (third (third (second thing))))))))))
		 :name (second (second (second (second (second (third (third (second thing))))))))
		 :association (string-trim '(#\Space #\Tab #\Newline #\Linefeed) (third (third (second (third (third (second thing)))))))
		 :region (string-trim '(#\Space #\Tab #\Newline #\Linefeed) (fifth (third (second (third (third (second thing)))))))
		 :lat (with-input-from-string (in (nth 8 (third (second (third (third (second thing))))))) (read in))
		 :lon (with-input-from-string (in (nth 10 (third (second (third (third (second thing))))))) (read in))))

(defmethod peak-serialize ((p sota-peak))
  "Serialize a SOTA peak. This is for locally caching peak data
instead of fetching it from the SOTA site repeatedly."
  (append
   (list
    '(type sota-peak)
    (list 'lat (point-lat p))
    (list 'lon (point-lon p))
    (list 'designator (designator p))
    (list 'name (name p))
    (list 'association (name p))
    (list 'region (region p))
    )
   (af:point-metadata-serialize p)))

(defmethod peak-deserialize-method ((p sota-peak) peak-data)
  "Create a SOTA peak object from the data dumped by 'peak-serialize'.
If the optional point-type value is supplied, the created object will
be of that type. This is for locally caching peak data instead of
fetching it from the SOTA site repeatedly."
  (point-metadata-deserialize-method p peak-data)
  (mapcar #'(lambda (n)
	      (cond
	       ((equal (first n) 'lat)
		(setf (point-lat p) (second n)))
	       ((equal (first n) 'lon)
		(setf (point-lon p) (second n)))
	       ((equal (first n) 'designator)
		(setf (designator p) (second n)))
	       ((equal (first n) 'name)
		(setf (name p) (second n)))
	       ((equal (first n) 'association)
		(setf (association p) (second n)))
	       ((equal (first n) 'region)
		(setf (region p) (second n)))
	       ))
	  peak-data))

(defmethod pp ((p sota-peak))
  "Pretty print a SOTA peak object."
  (format t "~A ~A ~A ~A ~A~%"
	  (designator p)
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
     (if *sota-rss*
	 (get-all-spots-via-rss)
	 (get-all-spots-via-scraping)))
    t))

(defun grim-reaper (&optional (max-age *age-out*))
  "Removes entries older than max-age seconds in the hash table."
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
  (setf *association-cache* (get-associations))
  (setf *spot-thread* (bt:make-thread
		       (lambda () (spot-fetcher-thread))
		       :name "sota-spots")))
