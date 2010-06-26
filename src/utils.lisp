;;; Copyright (c) 2010 Jorge Gajon

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:

;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package #:tomate)

;;; See: http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                ,@(when doc (list doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACROS TO DEFINE PAGES

;;; These macros to define url handlers were initially taken, and then
;;; modified from: http://www.adampetersen.se/articles/lispweb.htm
;;; Thank you Adam Petersen.
(defmacro define-url-fn (name &body body)
  `(progn
     (defun ,name ()
       (let ((the-user (get-user-obj (session-value 'username))))
         ;; It could happen that the user was not found but the session
         ;; has the 'authenticated value set if the user was deleted from
         ;; the database.
         (unless (and the-user (string= (session-value 'authenticated) "yes"))
           (setf (session-value 'authenticated) nil)
           (redirect "/"))
         ;;
         ;; Yay, we have a valid user, let's set the time zone.
         (let ((time-zone (setf (user-time-zone the-user)
                                (session-value 'timezone))))
           (declare (ignorable time-zone))
           (no-cache) ; Prevent caching on most browsers.
           ,@body)))
     (push (create-prefix-dispatcher ,(format nil "/~(~a~)/" name)
                                     ',name)
           *dispatch-table*)))

;;; TODO: there should be a way to combine these `define-*-fn` macros
;;;       into a single one.
(defmacro define-open-url-fn (name &body body)
  "This macro is like `define-url-fn` macro but does not try to see
if there's a valid session. The variables `the-user` and `time-zone` are
still being inserted into the lexical context but their values are nil.
BE CAREFUL."
  `(progn
     (defun ,name ()
       (let (the-user time-zone)
         (declare (ignorable the-user time-zone))
         ,@body))
     (push (create-prefix-dispatcher ,(format nil "/~(~a~)/" name) ',name)
           *dispatch-table*)))

(defmacro define-index-fn (&body body)
  (let ((index-fn (gensym)))
    `(progn
       (defun ,index-fn ()
         (let (the-user time-zone)
           (declare (ignorable the-user time-zone))
           ,@body))
       (setf hunchentoot::*default-handler* #',index-fn))))


;;; CAUTION, We expect to capture the free variable `the-user`.
(defmacro standard-page ((&key (title "")
                               (show-banner t)
                               css-files js-files
                               (active-tab :listing))
                         &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html
       (:head
         (:meta :charset "utf-8")
         (:title (esc ,title))
         ;;
         ;; CSS files
         ;;
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/html5reset-1.4.1.css")
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/styles.css")
         ,@(mapcar (lambda (file)
                     `(:link :type "text/css" :rel "stylesheet"
                             :media "screen, projection"
                             :href ,(format nil"/static/css/~a" file)))
                   css-files)
         ;;
         ;; JavaScript files
         ;;
         (:script :type "text/javascript" :src "/static/js/jquery-1.4.2.min.js")
         ,@(mapcar (lambda (file)
                     `(:script :type "text/javascript"
                               :src ,(format nil "/static/js/~a" file)))
                   js-files)
         "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
       (:body
         (:div :id "body-container"
           ,(when show-banner
              `(:header :role "banner" :class "banner"
                 (:nav
                   (:ul (:li ,@(when (eql active-tab :listing) `(:class "current"))
                             (:a :href "/listing/" "Listings"))
                        (:li ,@(when (eql active-tab :reports) `(:class "current"))
                             (:a :href "/reports/" "Reports"))
                        (:li ,@(when (eql active-tab :account) `(:class "current"))
                             (:a :href "/account/" "Account"))
                        (:li :class "notab"
                             (:span "Welcome "
                                    (esc (or (trim-or-nil (user-full-name the-user))
                                             (user-username the-user)))))
                        (:li :class "notab" (:a :href "/logout/" "Logout"))))))
           (:div :id "content"
                 ,@body)
           (:footer
             (:p "Powered by Common Lisp")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUEUE AND SHOW ERROR/SUCCESS MESSAGES TO THE USER

(defun push-error-msg (msg)
  (push msg (session-value 'error-msgs))
  nil)

(defun push-success-msg (msg)
  (push msg (session-value 'success-msgs)))

(defun %show-messages (msgs css-id)
  (with-html-output (*standard-output*)
    (:div :id (escape-string css-id)
          (:ul (mapcar
                 (lambda (msg) (htm (:li (esc msg))))
                 msgs)))))

(defun show-error-messages ()
  (when (session-value 'error-msgs)
    (%show-messages (reverse (session-value 'error-msgs)) "errors")
    (setf (session-value 'error-msgs) nil)))

(defun show-success-messages ()
  (when (session-value 'success-msgs)
    (%show-messages (reverse (session-value 'success-msgs)) "success")
    (setf (session-value 'success-msgs) nil)))

(defun show-all-messages ()
  (show-error-messages)
  (show-success-messages))

(defmacro! require-fields (&rest args)
  `(let ((,g!success t))
     (flet ((,g!failed (msg)
               (setf ,g!success nil)
               (push-error-msg msg)))
       ,@(mapcar
           (lambda (arg)
             (if (consp arg)
               `(or ,(car arg) (,g!failed ,(cadr arg)))
               `(or ,arg (,g!failed
                           ,(format nil "The ~(~a~) is required."
                                    (#~s/-/ / (symbol-name arg)))))))
           args)
       ,g!success)))

;(macroexpand-1 '(require-fields username password))
;(macroexpand-1 '(require-fields username password-confirmation))
;(macroexpand-1 '(require-fields (foo "The field foo is lucky.") bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY MACROS

(defmacro table-columns (&rest args)
  `(with-html-output (*standard-output*)
     (:thead
       (:tr
         ,@(loop for a in args collect
                 (if (consp a)
                   `(:th ,@(cdr a) (esc ,(car a)))
                   `(:th (esc ,a))))))))

(defun text-input (label name &key default-value (size 20) disabled
                         labelclass inputclass (type "text"))
  (let* ((name (escape-string name))
         (label (escape-string label))
         (id (format nil "id_~a" name))
         (default-value (or (escape-string (post-parameter name))
                            (escape-string default-value))))
    (with-html-output (*standard-output*)
      (when label
        (htm (:label :for id
                     :class labelclass
                     (str label))))
      (:input :type type
              :id id
              :name name
              :value default-value
              :size size
              :class inputclass
              :disabled disabled))))

(defun password-input (label name &key default-value (size 20) disabled
                             labelclass inputclass)
  (text-input label name :default-value default-value :size size
              :disabled disabled :labelclass labelclass
              :inputclass inputclass :type "password"))

(defun hidden-input (name &key default-value)
  (let* ((name (escape-string name))
         (id (format nil "id_~a" name))
         (default-value (or (escape-string (post-parameter name))
                            (escape-string default-value))))
    (with-html-output (*standard-output*)
      (:input :type "hidden"
              :id id
              :name name
              :value default-value))))

(defun submit-button (label &key (name "submit") disabled)
  (let* ((name (escape-string name))
         (label (escape-string label))
         (id (format nil "id_~a" name)))
    (with-html-output (*standard-output*)
      (:button :type "submit"
              :id id
              :value label
              :name name
              :disabled disabled
              (esc label)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATE CLASS.
;;; This is used to encapsulate an universal-time with a time-zone,
;;; also defining some utility functions to convert the date to
;;; different representations.

(defclass date ()
  ((universal-time :initarg :universal-time :reader date-universal-time)
   (time-zone      :initarg :time-zone      :reader date-time-zone)))

(defmethod print-object ((date date) stream)
  (print-unreadable-object (date stream :identity t :type t)
    (format stream "~a (TZ:~a)" (format-date date) (date-time-zone date))))

(defgeneric make-date (time &optional time-zone)
  (:documentation
"Creates a DATE object. The time parameter must be either an universal-time
or another DATE object. If the time-zone is not supplied a default value of
6 is used. If time is a DATE object and no time-zone is supplied, the
default value is taken from the DATE object itself."))

(defmethod make-date ((universal-time number) &optional (time-zone 6))
  (make-instance 'date
                 :universal-time universal-time
                 :time-zone time-zone))

(defmethod make-date ((date date) &optional time-zone)
  (make-instance 'date
                 :universal-time (date-universal-time date)
                 :time-zone (or time-zone (date-time-zone date))))

(defgeneric date- (date seconds)
  (:documentation
"Creates a new DATE object with its date set to that of the supplied date
in the first parameter, minus the number of seconds supplied in the second
parameter."))

(defmethod date- ((date date) (seconds number))
  (make-instance 'date
                 :universal-time (- (date-universal-time date) seconds)
                 :time-zone (date-time-zone date)))

(defgeneric date+ (date seconds)
  (:documentation
"Creates a new DATE object with its date set to that of the supplied date
in the first parameter, plus the number of seconds supplied in the second
parameter."))

(defmethod date+ ((date date) (seconds number))
  (make-instance 'date
                 :universal-time (+ (date-universal-time date) seconds)
                 :time-zone (date-time-zone date)))


(define-constant +day-names+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(define-constant +month-names+
  #("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December"))

(defun format-date (date &key (longform nil))
    "Returns a string with the date formatted as \"YYYY-MM-DD\" or as
\"DayOfWeek Month DD, YYYY\" (eg. Tuesday May 4, 2010) if key :longform is t."
  (multiple-value-bind (s m h date month year day)
      (decode-universal-time (date-universal-time date) (date-time-zone date))
    (declare (ignore s m h))
    (if longform
      ;; Thursday May 6, 2010
      (format nil "~a ~a ~d, ~d"
              (svref +day-names+ day)
              (svref +month-names+ (1- month))
              date year)
      ;; 2010-05-06
      (format nil "~d-~2,'0d-~2,'0d" year month date))))

(defun parse-date (date time-zone)
  "Parse a date from a string like \"2010-06-25\" and returns a new DATE
object representing that date and the supplied time-zone."
  (when (and (stringp date) (= (length date) 10))
    (let ((year (parse-int-force-pos-or-zero (subseq date 0 4)))
          (month (parse-int-force-pos-or-zero (subseq date 5 7)))
          (day (parse-int-force-pos-or-zero (subseq date 8 10))))
      (when (and (plusp year) (plusp month) (plusp day))
        (make-date
          (encode-universal-time 0 0 12 day month year time-zone)
          time-zone)))))

(defun format-iso8601-date (date)
  "Returns a string with the date formatted to a single restricted format
from the set of formats defined by ISO 8601. Specifically, the format
YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30-05:00 for EST time zone)
See: http://www.w3.org/TR/NOTE-datetime
and  http://en.wikipedia.org/wiki/ISO_8601"
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (date-universal-time date) (date-time-zone date))
    (declare (ignore day daylight-p))
    (format nil
            "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~a~2,'0d:00"
            year month date hour minute second
            (if (plusp zone) "-" "+") zone)))

(defun parse-iso8601-date (date)
  "Parse a date from a string in the following ISO 8601 format
YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30-05:00) into a DATE object."
  (flet ((extract-parts (date)
           (multiple-value-bind (lowbound upperbound vector1 vector2)
               ;; 2010-06-10T19:20:30-06:00
               ;; TODO: Is it a good idea to use a regexp? Why not simply
               ;;       use subseqs of the string?
               (#~m/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})([-|+])(\d{2}):\d{2}$/
                date)
             (declare (ignorable lowbound upperbound))
             (when (and vector1 vector2)
               ;; vector1 contains the starting positions for each matched
               ;; group while vector2 contains the corresponding ending
               ;; positions.
               ;;
               ;; We extract the pairs of positions with the `loop` form
               ;; and pass them to the lambda to extract the strings and
               ;; convert them to fixnums, except for the -/+ sign of the
               ;; zone.
               (mapcar
                 #'(lambda (pos)
                     (let ((str (subseq date (car pos) (cdr pos))))
                       (cond ((string= str "-") +1) ; CL time zones have
                             ((string= str "+") -1) ; their signs inverted.
                             (t (parse-int-force-pos-or-zero str)))))
                 (loop for start across vector1 and end across vector2
                       collect (cons start end)))))))

    ;; Now use the extract-parts return value to reconstruct an
    ;; universal-time.
    (destructuring-bind (year month day hour minute second zone-+ zone)
        (extract-parts date)
      (make-date
        (encode-universal-time
          second minute hour day month year (* zone zone-+))
        (* zone zone-+)))))

;(format-iso8601-date
;  (parse-iso8601-date "2010-06-10T19:20:30-06:00"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc utilities

(defvar %secs-in-one-day (* 60 60 24))

(defun trim-or-nil (string)
  "Returns a string with all spaces, tabs and newlines removed from both
ends. It will return NIL if the resulting string is of length 0, or if
the input string is NIL as well."
  (when string
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) string)))
      (when (> (length trimmed) 0)
        trimmed))))

(defun parse-int-force-pos-or-zero (string &key (start 0) (end nil) (radix 10))
  "Parses an integer from the given string. The string could be NIL or contain
garbage, in which case the function will simply return the number zero (0).
Otherwise, it will parse the string and return the ABSOLUTE VALUE of the
number. The function accepts the same arguments as `parse-integer`, except for
:junk-allowed which is always T. There is no `pos` return value like in
`parse-integer`."
  (abs (or (parse-integer (or string "0")
                          :start start
                          :end end
                          :radix radix
                          :junk-allowed t)
           0)))

;(mapcar #'parse-int-force-pos-or-zero
;        (list "" nil "haosd" "1" "0" "23" "-12" "-1" "-0"))

;;; From Paul Graham's On Lisp, pg. 47.
(defun filter (fn lst)
  "You give FILTER a function and a list, and get back a list of whatever
non-nil values are returned by the function as it is applied to the
elements of the list."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
