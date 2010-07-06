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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA RETRIEVAL

(defun get-user-obj (username)
  (handler-case (build-user-from-alist (clouchdb:get-document username))
    (error () nil)))

(defun validate-credentials (username password)
  (handler-case
    (let ((alist (clouchdb:get-document username))
          (digest (hunchentoot::md5-hex password)))
      (string= digest (cdr (assoc :|password| alist))))
    (error () nil)))

(defun get-all-tasks (user)
  (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
    (mapcar #'build-task-from-alist
            (clouchdb:query-document
              `(:|rows| :|value|) ;`(:|rows| :|id| ,#'clouchdb:get-document)
              (clouchdb:invoke-view "tasks" "all-tasks" :key user)))))

(defun get-tasks-by-date (today user)
  (multiple-value-bind (s m h date month year)
      (decode-universal-time (date-universal-time today)
                             (date-time-zone today))
    (declare (ignore s m h))
    (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
      (mapcar #'build-task-from-alist
              (clouchdb:query-document
                `(:|rows| :|id| ,#'clouchdb:get-document)
                (clouchdb:invoke-view "tasks" "tasks-by-date"
                                      :key (list user year month date)))))))

(defun get-task (id user)
  (handler-case
    (let ((user (if (eq (type-of user) 'user) (user-username user) user))
          (alist (clouchdb:get-document id)))
      ;; Ensure this user owns the task.
      (when (string= user (cdr (assoc :|user| alist)))
        (build-task-from-alist alist)))
    (error () nil)))

(defun get-task-json (id user)
  (handler-case
    (let ((user (if (eq (type-of user) 'user) (user-username user) user))
          (alist (clouchdb:get-document id)))
      ;; Ensure this user owns the task.
      (when (string= user (cdr (assoc :|user| alist)))
        (clouchdb:document-to-json alist)))
    (error () nil)))

;;; TODO: What a name!
(defun get-real-pomodoros-and-tasks-count-by-days (user &key (days 30))
  (let* ((user (if (eq (type-of user) 'user) (user-username user) user))
         (data (clouchdb:query-document
                 `(:|rows|)
                 (clouchdb:invoke-view "reports" "real-pomodoros"
                                       :group t
                                       :descending t
                                       :limit days
                                       :end-key (list user)
                                       :start-key (list user (make-hash-table)))))
         ;; We'll collect the values here.
         x-dates
         y-pomodoros
         y-tasks)
    (mapcar (lambda (data)
              ;; The data comes in the following format
              ;; ((:|key| "gajon" 2010 6 11)
              ;;  (:|value| (:|totalPomodoros| . 18) (:|numTasks| . 2)))
              (let ((date (apply #'format nil "~a/~a/~a" (cddar data)))
                    (alist (cdadr data)))
                (push date x-dates)
                (push (cdr (assoc :|totalPomodoros| alist)) y-pomodoros)
                (push (cdr (assoc :|numTasks| alist)) y-tasks)))
            (car data))
    (values x-dates y-pomodoros y-tasks)))

(defun get-last-date-with-records (user time-zone)
  "Returns a new DATE object representing the most recent date that contains
records from the user, or NIL if there are no records by this user."
  (let* ((user (if (eq (type-of user) 'user) (user-username user) user))
         (data
           ;; By querying the tasks-by-date view, in descending order, we
           ;; can get the last day that contains records, they keys in this
           ;; view are of the form [user, year, month, day]
           (clouchdb:query-document
             '(:|rows| :|key|)
             (clouchdb:invoke-view "tasks" "tasks-by-date"
                                   :descending t
                                   :limit 1
                                   :end-key (list user)
                                   :start-key (list user (make-hash-table))))))
    ;; data should come as (("user" year month day))
    (when data
      (handler-case
        (destructuring-bind (year month day) (cdar data)
          (parse-date (format nil "~d-~2,'0d-~2,'0d" year month day)
                      time-zone))
        (error () nil)))))

(defun get-all-tags (user)
  (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
    (mapcar #'cadr
            (clouchdb:query-document
              `(:|rows| :|key|)
              (clouchdb:invoke-view "tags" "all-tags" :group t
                                    :start-key (list user)
                                    ;; ?startkey=["user"]&endkey=["user",{}]
                                    :end-key (list user (make-hash-table)))))))

(defun get-all-users ()
  (mapcar (lambda (alist) (build-user-from-alist alist))
          (clouchdb:query-document
            `(:|rows| :|value|)
            (clouchdb:invoke-view "users" "all-users"))))


(defmacro %couchdb-field (field-name obj)
  "We want to use lowercase field names on the database(CouchDB) which means
that we need symbols like :|foo|, but it is annoying to have to type them.
Also, the data returned by clouchdb is a lists of alists. This macro helps
a little bit:
(%couchdb-field 'foo alist) is expanded into `(cdr (assoc :|foo| alist))"
  ;; the `remove-quote` function is needed because when field-name
  ;; is a quoted symbol we would get a symbol like :|'foo|
  (flet ((remove-quote (symbol)
                       (if (and (consp symbol) (eq (car symbol) 'QUOTE))
                         (cadr symbol)
                         symbol)))
    `(cdr (assoc
            ,(intern
               (format nil "~(~a~)" (mkstr (remove-quote field-name)))
               "KEYWORD")
            ,obj))))


(defun build-task-from-alist (alist)
  (make-instance 'task
                 :id (%couchdb-field '_id alist)
                 :rev (%couchdb-field '_rev alist)
                 :name (%couchdb-field 'name alist)
                 :tags (%couchdb-field 'tags alist)
                 :location (%couchdb-field 'location alist)
                 :estimations (%couchdb-field 'estimations alist)
                 :real (%couchdb-field 'real alist)
                 :date (%couchdb-field 'date alist)))

(defun build-user-from-alist (alist)
  (make-instance 'user
                 :rev (%couchdb-field '_rev alist)
                 :full-name (%couchdb-field 'full-name alist)
                 :username (%couchdb-field '_id alist)
                 :password-digest (%couchdb-field 'password alist)
                 :email (%couchdb-field 'email alist)
                 :current-location (%couchdb-field 'current-location alist)
                 :time-zone (%couchdb-field 'time-zone alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA STORAGE

(defun add-task (task user)
  (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
    (clouchdb:create-document
      `((:|type| . "task")
        (:|name| . ,(task-name task))
        (:|date| . ,(task-date task))
        (:|tags| . ,(task-tags task))
        (:|location| . ,(task-location task))
        (:|estimations| . ,(task-estimations task))
        (:|real| . ,(task-real task))
        (:|user| . ,user)))
    ;; TODO: Should verify that (:|ok| . T)??
    ;; TODO: fetch the task from the database or simply
    ;; set the _rev field?
    task))

(defun update-task (task user)
  ;; TODO: We are repeating code here.
  (let ((user (if (eq (type-of user) 'user) (user-username user) user)))
    (clouchdb:put-document
      `((:|_id| . ,(task-id task))
        (:|_rev| . ,(task-rev task))
        (:|type| . "task")
        (:|name| . ,(task-name task))
        (:|date| . ,(task-date task))
        (:|tags| . ,(task-tags task))
        (:|location| . ,(task-location task))
        (:|estimations| . ,(task-estimations task))
        (:|real| . ,(task-real task))
        (:|user| . ,user)))
    ;; TODO: fetch the task from the database or simply
    ;; set the _rev field?
    task))

(defun delete-task (task)
  (clouchdb:delete-document (task-id task) :if-missing :ignore))

(defun add-user (the-user)
  (handler-case
    (progn
      (clouchdb:create-document
        `((:|type| . "user")
          (:|password| . ,(user-password-digest the-user))
          (:|full-name| . ,(user-full-name the-user))
          (:|email| . ,(user-email the-user))
          (:|current-location| . ,(user-current-location the-user))
          (:|time-zone| . ,(user-time-zone the-user)))
        :id (user-username the-user))
      ;; TODO: fetch the user from the database or simply
      ;; set the _rev field?
      the-user)
    (error () nil)))

(defun update-user (the-user)
  ;; For User documents, the username is the _id of the document.
  (clouchdb:put-document
    `((:|_id| . ,(user-username the-user))
      (:|_rev| . ,(user-rev the-user))
      (:|type| . "user")
      (:|password| . ,(user-password-digest the-user))
      (:|full-name| . ,(user-full-name the-user))
      (:|email| . ,(user-email the-user))
      (:|current-location| . ,(user-current-location the-user))
      (:|time-zone| . ,(user-time-zone the-user))))
  ;; TODO: fetch the user from the database or simply
  ;; set the _rev field?
  the-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES TO SETUP THE COUCHDB DATABASE, MAINLY THE
;;; DESIGN VIEWS.

;(defun %%create-design-documents ()
;  (clouchdb:create-ps-view "users"
;    (clouchdb:ps-view ("all-users")
;                      (defun map (doc)
;                        (with-slots (type _id) doc
;                          (if (= type "user")
;                            (emit _id doc)))))
;
;    ;;; TODO: Is this view really necessary??
;    (clouchdb:ps-view ("user")
;                      (defun map (doc)
;                        (with-slots (type _id password) doc
;                          (if (= type "user")
;                            (emit (array _id password) nil))))))
;
;  (clouchdb:create-ps-view "tags"
;    (clouchdb:ps-view ("all-tags")
;                      (defun map (doc)
;                        (with-slots (type user tags) doc
;                          (if (= type "task")
;                            ((parenscript:@ doc tags for-each)
;                              (lambda (tag)
;                                (emit (array user tag) 1))))))
;                      (defun reduce (keys values)
;                        (return (sum values)))))
;
;  (clouchdb:create-ps-view "tasks"
;    (clouchdb:ps-view ("all-tasks")
;                      (defun map (doc)
;                        (with-slots (type user) doc
;                          (if (= type "task")
;                            (emit user doc)))))
;    ;; I didn't want to fight with parenscript to generate the regex, 
;    ;; therefore I'm supplying the whole string here, and thanks to
;    ;; Hoyte's #> reader macro this is very easy.
;    #>END
;    "tasks-by-date": {
;      "map": "function(doc) {
;                if (doc.type == 'task') {
;                  var parts = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})/.exec(doc.date);
;                  if (parts != null) {
;                    emit([doc.user,                // The user
;                          parseInt(parts[1], 10),  // Year
;                          parseInt(parts[2], 10),  // Month
;                          parseInt(parts[3], 10)], // Day
;                         null);
;                  }
;                }
;              }"
;    }
;    END)
;  
;  (clouchdb:create-ps-view "reports"
;    #>END
;    "real-pomodoros": {
;      "map": "function(doc) {
;                if (doc.type == 'task') {
;                  var parts = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})/.exec(doc.date);
;                  if (parts != null) {
;                    emit([doc.user,                // The user
;                          parseInt(parts[1], 10),  // Year
;                          parseInt(parts[2], 10),  // Month
;                          parseInt(parts[3], 10)], // Day
;                         doc.real); // And the # of pomodoros.
;                  }
;                }
;              }",
;      "reduce": "function (keys, values, rereduce) {
;                   var result = {totalPomodoros:0, numTasks:0};
;                   for(var i=0; i<values.length; i++) {
;                     if (rereduce) {
;                       result.totalPomodoros += values[i].totalPomodoros;
;                       result.numTasks += values[i].numTasks;
;                     } else {
;                       result.totalPomodoros += values[i];
;                       result.numTasks++;
;                     }
;                   }
;                   return result;
;                 }"
;    }
;    END))
;
;(defun %%delete-design-documents ()
;  (clouchdb:delete-view "tasks")
;  (clouchdb:delete-view "users")
;  (clouchdb:delete-view "tags")
;  (clouchdb:delete-view "reports"))

#|
"validate_doc_update":
  "function (newDoc, oldDoc, userCtx) {
    function require(field, message) {
      message = message || "Document must have a " + field;
      if (!newDoc[field]) throw({forbidden : message});
    };
    
    if (newDoc.type == "task") {
      require("name");
      require("date");
      require("tags");
      require("user");
      require("estimations");
      require("real");
    }
  }"
|#

#|
"validate_doc_update":
  "function (newDoc, oldDoc, userCtx) {
     function require(field, message) {
       message = message || "Document must have a " + field;
       if (!newDoc[field]) throw({forbidden : message});
     };

   if (newDoc.type == "user") {
     require("full-name");
     require("username");
     require("current-location");
   }
  }"
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OTHER STUFF USED DURING DEVELOPMENT.


;(setf clouchdb::*debug-requests* nil)

;(defun %%create-test-documents ()
;  (let* ((user1 `((:|type| . "user")
;                  (:|full-name| . "Jorge Gajon")
;                  (:|password| . "5A3218FE6063AD3B9B4A7E49980BAE12") ; gajon
;                  (:|current-location| . "Mexico City")
;                  (:|email| . "jorge.gajon@gmail.com")
;                  (:|time-zone| . 6)))
;         (user2 `((:|type| . "user")
;                  (:|full-name| . "Gorda")
;                  (:|password| . "311FFB7B8D804A5DA6B8A6EFD50F4AE5") ; gorda
;                  (:|current-location| . "DogoLandia")
;                  (:|email| . "la_gorda@example.com")
;                  (:|time-zone| . 6)))
;         (data1 `((:|type| . "task")
;                  (:|name| . "Practical Common Lisp (Seibel)")
;                  (:|date| . ,(format-iso8601-date (make-date (get-universal-time) 6)))
;                  (:|tags| . ("lisp" "books" "learning"))
;                  (:|location| . "Mexico City")
;                  (:|estimations| . "3+2")
;                  (:|real| . 4)
;                  (:|user| . "gajon")))
;         (data2 `((:|type| . "task")
;                  (:|name| . "Project Tomate")
;                  (:|date| . ,(format-iso8601-date (make-date (get-universal-time) 6)))
;                  (:|tags| . ("lisp" "projects" "tomate"))
;                  (:|location| . "Mexico City")
;                  (:|estimations| . "5")
;                  (:|real| . 4)
;                  (:|user| . "gajon")))
;         (data3 `((:|type| . "task")
;                  (:|name| . "Comp.Lang.Lisp")
;                  (:|date| . ,(format-iso8601-date
;                                (make-date (- (get-universal-time) %secs-in-one-day)
;                                6)))
;                  (:|tags| . ("lisp" "online"))
;                  (:|location| . "Mexico City")
;                  (:|estimations| . "1")
;                  (:|real| . 1)
;                  (:|user| . "gajon")))
;         (data4 `((:|type| . "task")
;                  (:|name| . "Sleep")
;                  (:|date| . ,(format-iso8601-date (make-date (get-universal-time) 6)))
;                  (:|tags| . ("sleep"))
;                  (:|location| . "DogoLandia")
;                  (:|estimations| . "1")
;                  (:|real| . 1)
;                  (:|user| . "gorda")))
;         (data5 `((:|type| . "task")
;                  (:|name| . "Eat")
;                  (:|date| . ,(format-iso8601-date (make-date (get-universal-time) 6)))
;                  (:|tags| . ("eat"))
;                  (:|location| . "DogoLandia")
;                  (:|estimations| . "1")
;                  (:|real| . 1)
;                  (:|user| . "gorda")))
;         (data6 `((:|type| . "task")
;                  (:|name| . "Shit")
;                  (:|date| . ,(format-iso8601-date (make-date (get-universal-time) 6)))
;                  (:|tags| . ("shit"))
;                  (:|location| . "DogoLandia")
;                  (:|estimations| . "1")
;                  (:|real| . 1)
;                  (:|user| . "gorda"))))
;    (clouchdb:create-document user1 :id "gajon")
;    (clouchdb:create-document user2 :id "gorda")
;    (clouchdb:create-document data1)
;    (clouchdb:create-document data2)
;    (clouchdb:create-document data3)
;    (clouchdb:create-document data4)
;    (clouchdb:create-document data5)
;    (clouchdb:create-document data6)))
;
;(defun %%delete-users () 
;  (let* ((view-results (clouchdb:invoke-view "users" "all-users"))
;         (users (clouchdb:query-document '(:|rows| :|value|) view-results)))
;    (clouchdb:bulk-document-update
;      (mapcar #'clouchdb:as-deleted-document users))))
;
;(defun %%delete-tasks ()
;  (let* ((view-results (clouchdb:invoke-view "tasks" "all-tasks"))
;         (tasks (clouchdb:query-document '(:|rows| :|value|) view-results)))
;    (clouchdb:bulk-document-update
;      (mapcar #'clouchdb:as-deleted-document tasks))))
;
;(defun %%delete-test-documents ()
;  (%%delete-users)
;  (%%delete-tasks))
;
