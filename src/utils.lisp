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

(defmacro define-index-fn (&body body)
  (let ((index-fn (gensym)))
    `(progn
       (defun ,index-fn ()
         ,@body)
       (setf hunchentoot::*default-handler* #',index-fn))))

;;; TODO: We are repeating most of the code here and in `standard-page`.
(defmacro login-page (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html
       (:head
         (:meta :charset "utf-8")
         (:title "Login")
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/html5reset-1.4.1.css")
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/styles.css")
         (:script :type "text/javascript" :src "/static/js/jquery-1.4.2.min.js")
         (:script :type "text/javascript" :src "/static/js/login.js")
         "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
       (:body
         (:div :id "body-container"
               (:div :id "content"
                     ,@body))))))

;;; CAUTION, We expect to capture the free variable `the-user`.
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent nil)
     (:html
       (:head
         (:meta :charset "utf-8")
         (:title (esc ,title))
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/html5reset-1.4.1.css")
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/styles.css")
         (:script :type "text/javascript" :src "/static/js/jquery-1.4.2.min.js")
         (:script :type "text/javascript" :src "/static/js/code.js")
         (:script :type "text/javascript" :src "/static/js/jquery.tablesorter.min.js")
         (:link :type "text/css"
                :rel "stylesheet"
                :media "screen, projection"
                :href "/static/css/tablesorter/blue/style.css")
         "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
       (:body
         (:div :id "body-container"
           (:header :role "banner" :class "banner"
             (:nav
               (:ul (:li :id "current" (:a :href "/listing/" "Listings"))
                    (:li (:a :href "/reports/" "Reports"))
                    (:li (:a :href "/account/" "Account"))
                    (:li :class "notab"
                         (:span "Welcome "
                                (esc (user-full-name the-user))))
                    (:li :class "notab" (:a :href "/logout/" "Logout")))))
           (:div :id "content"
                 ,@body)
           (:footer
             (:p "Powered by Common Lisp")))))))


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
         (default-value (or (post-parameter name)
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
         (default-value (or (post-parameter name)
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
;;; DATE FORMATTING.

(define-constant +day-names+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(define-constant +month-names+
  #("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December"))

(defun format-date (date time-zone &key (longform nil))
    "Returns a string with the date formatted as \"YYYY-MM-DD\" or as
\"DayOfWeek Month DD, YYYY\" (eg. Tuesday May 4, 2010) if key :longform is t."
  (multiple-value-bind (s m h date month year day)
      (decode-universal-time date time-zone)
    (declare (ignore s m h))
    (if longform
      ;; Thursday May 6, 2010
      (format nil "~a ~a ~d, ~d"
              (svref +day-names+ day)
              (svref +month-names+ (1- month))
              date year)
      ;; 2010-05-06
      (format nil "~d-~2,'0d-~2,'0d" year month date))))

;;; REGARDING THE STORED REPRESENTATION OF DATES
;;; SEE: http://www.w3.org/TR/NOTE-datetime
;;; AND: http://www.blog.activa.be/default,date,2010-03-12.aspx
;;; AND: http://en.wikipedia.org/wiki/ISO_8601

(defun format-iso8601-date (date time-zone)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time date time-zone)
    (declare (ignore day daylight-p))
    ;; YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30-05:00)
    ;; for US Eastern Standard Time
    (format nil
            "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~a~2,'0d:00"
            year month date hour minute second
            (if (plusp zone) "-" "+") zone)))

(defun parse-iso8601-date (date)
  (flet ((extract-parts (date)
           (multiple-value-bind (lowbound upperbound vector1 vector2)
               ;; 2010-06-10T19:20:30-06:00
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
      (values
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
