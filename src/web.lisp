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
;;; Start and Stop the Server

;;; These start/stop functions were initially lifted from:
;;; http://github.com/smanek/trivial-lisp-webapp (Shaneal Manek)

(defvar *tomate-acceptor-instance* nil)

(defun start ()
  (unless *tomate-acceptor-instance*
    (setf *tomate-acceptor-instance*
          (make-instance 'hunchentoot:acceptor :port *server-port*))
    (hunchentoot:start *tomate-acceptor-instance*)
    (format t "Webserver started on port ~A.~%" *server-port*)))

(defun stop ()
  (when *tomate-acceptor-instance*
    (format t "Shutting down")
    (hunchentoot:stop *tomate-acceptor-instance*)
    (setf *tomate-acceptor-instance* nil)
    #+ImDebugging (portable-quit)))

(defun portable-quit ()
  #+allegro(excl:exit)
  #+sbcl(sb-ext:quit)
  #+clisp(ext:quit)
  #-(or allegro sbcl clisp)(error "don't know how to quit."))

;;; Originally the server instance was held in a closure (below), but since
;;; I'm continually reloading the source while developing, I turned it into
;;; a special variable above.
;(let ((server nil))
;  (defun start ()
;    (setf server (make-instance 'hunchentoot:acceptor :port *server-port*))
;    (hunchentoot:start server)
;    (format t "Webserver started on port ~A.~%" *server-port*))
;
;  (defun stop ()
;    (format t "Shutting down")
;    (hunchentoot:stop server)
;    (portable-quit)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INDEX PAGE

(define-index-fn
  ;; Validate and set session (if there's a form post).
  (when (eql :post (request-method*))
    (let ((form-username (trim-or-nil (post-parameter "username")))
          (form-password (trim-or-nil (post-parameter "password")))
          (form-timezone (parse-int-force-pos-or-zero
                           (trim-or-nil (post-parameter "timezone")))))
      (if (validate-credentials form-username form-password)
        (setf (session-value 'authenticated) "yes"
              (session-value 'username) form-username
              (session-value 'timezone) form-timezone)
        (setf (session-value 'authenticated) nil))))
  (when (string= (session-value 'authenticated) "yes")
    (redirect "/listing/"))
  ;; Render login form
  (login-page
    (:section :id "login"
      (:form :method "post" :action "."
             (hidden-input "timezone")
             (:div (text-input "Username:" "username"))
             (:div (password-input "Password:" "password"))
             (:div (submit-button "Login"))))))

(define-url-fn logout
  (setf (session-value 'authenticated) nil
        (session-value 'username) "")
  (redirect "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LISTING

(defun get-day-from-parameter-or-now (param)
  (let ((parsed (parse-int-force-pos-or-zero param)))
    (if (zerop parsed)
      (get-universal-time)
      parsed)))

(define-url-fn listing
  (let* ((today (get-day-from-parameter-or-now (parameter "d")))
         (yesterday (- today %secs-in-one-day))
         (tomorrow (+ today %secs-in-one-day))
         (tasks (get-tasks-by-date today time-zone the-user)))
    (standard-page (:title "Listing of tasks recorded")
      ;;
      ;; DAY NAVIGATION: displays the date, location and links to
      ;; go to the previous/next day.
      ;;
      (:section :id "day-navigation"
        (:div :id "previousday"
              (:p (:a :href (conc (format nil "?d=~d" yesterday))
                      "<< Previous day")))
        (:div :id "location"
              (:h1 (esc (user-current-location the-user))
                   ", " (esc (format-date today time-zone :longform t)))
              (:p "Record sheet"
                  (:span "[" (:a :href "#" "select date") "]")))
        (:div :id "nextday"
              (:p (:a :href (conc (format nil "?d=~d" tomorrow))
                      "Next day >>"))))
      ;;
      ;; THE MAIN LISTING
      ;;
      (:section :class "listing"
        (:table :id "task-listing"
                :class "tablesorter"
                :cellspacing 1 :cellpadding 0
          (table-columns ("Task" :style "width:100%;")
                         "Estimations" "Real" "Difference")
          (:tfoot (render-tasks-totals tasks))
          (:tbody
            (if tasks
              (loop for task in tasks do (render-task-as-row task))
              (htm (:tr (:td :colspan 4 "No tasks recorded.")))))))
      ;;
      ;; ADD NEW TASK
      ;;
      (render-add-new-task today))))

(defun render-task-as-row (task)
  (let* ((estimations (task-estimations task))
         (estimated (task-total-estimated task))
         (real (task-real task))
         (difference (- real estimated))
         (edit-url (format nil "/edit-task/?id=~a" (task-id task))))
    (with-html-output (*standard-output*)
      (:tr
        (:td
          (:div :class "editable-task"
            (:div :class "task-name" (esc (task-name task))
              (:span :class "edit hidden"
                     (:a :href edit-url "[edit]")))
            (:div :class "task-tags"
              (:small "(" (esc (fmt-task-tags task)) ")"))))
        (:td :class "text-center"
             (esc estimations))
        (:td :class "text-center" (str real))
        (:td :class "text-center" (str difference))))))

(defun render-tasks-totals (tasks)
  (let* ((estimated (reduce #'+ tasks :key #'task-total-estimated))
         (real (reduce #'+ tasks :key #'task-real))
         (difference (- real estimated)))
    (with-html-output (*standard-output*)
      (:tr (:th "Totals:")
           (:th (str estimated))
           (:th (str real))
           (:th (str difference))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADD TASK

(define-url-fn add-new-task
  ;; Validate, create new object and redirect.
  (when (process-add-new-task the-user time-zone)
    (redirect (format nil "/listing/?d=~d" (post-parameter "d"))))
  ;; Display form.
  ;; TODO: What if there's no "d" post parameter?
  (standard-page (:title "Add new record")
    (render-add-new-task (post-parameter "d"))
    ;;;
    ;;; DEBUG
    ;;;
    (:section
      (:header (:h1 "tags"))
      (:p (loop for tag in (get-all-tags the-user)
                do (htm (esc tag) ", ")))
      (:header (:h1 "data"))
      (loop for p in (post-parameters*)
            do (htm (:p (esc (car p))
                        ": "
                        (esc (cdr p))))))))

(defun render-add-new-task (today)
  (with-html-output (*standard-output*)
    (:section :class "add-task"
      (:header (:h1 "Add new record"))
      (:form :method "post" :action "/add-new-task/"
        (hidden-input "d" :default-value (format nil "~d" today))
        (:div
          (text-input nil "task" :default-value "Task description")
          (text-input nil "estimations" :default-value "Estimations")
          (text-input nil "real" :default-value "Real"))
        (:div (text-input nil "tags" :default-value "List of tags")
              (submit-button "Add record"))))))

(defun process-add-new-task (the-user time-zone)
  (let* ((task (trim-or-nil (post-parameter "task")))
         (tags (loop for tag in (split-sequence #\, (post-parameter "tags"))
                     if (trim-or-nil tag) collect it))
         (date (parse-int-force-pos-or-zero (post-parameter "d")))
         (estimations (extract-estimations (post-parameter "estimations")))
         (real (trim-or-nil
                 ;; Anything that's not a digit is removed
                 (#~s/[^\d+]// (post-parameter "real")))))
    (when (and task tags (> date 0) estimations real)
      (add-task
        (make-instance 'task
                       :name task
                       :tags tags
                       ;; We parsed a list of estimations above
                       ;; for example, a user entry like this: "1, 2, 4"
                       ;; is parsed into a list of numbers: '(1 2 4)
                       ;; And with the following format directive we
                       ;; join the numbers with a plus sign: "1+2+4"
                       :estimations (format nil "~{~d~^+~}" estimations)
                       :real (parse-int-force-pos-or-zero real)
                       :date (format-iso8601-date date time-zone))
        the-user))))

(defun extract-estimations (estimations)
  (mapcar #'parse-int-force-pos-or-zero
          (split-sequence #\+
                          ;; Anything that's not a digit is replaced by +
                          (#~s/[^\d+]/+/ estimations)
                          :remove-empty-subseqs t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EDIT A TASK

(define-url-fn fetch-task-json
  (let ((task (get-task-json (parameter "id"))))
    (with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
      (setf (content-type*) "application/json")
      (if task
        (htm (str task))
        (htm #"{"status":"notfound"}"#)))))

;;; TODO: WE ARE REPEATING A LOT OF CODE HERE!
(defun render-edit-task ()
  (with-html-output (*standard-output*)
    (:section :class "add-task"
      (:header (:h1 "Edit record"))
      (:form :method "post" :action "/edit-task/"
        (hidden-input "id")
        ;; TODO: Get default values from the object if there's no post data.
        (:div
          (text-input nil "task")
          (text-input nil "estimations")
          (text-input nil "real"))
        (:div (text-input nil "tags")
              (submit-button "Save"))))))

(defun process-edit-task (task the-user)
  (let* ((name (trim-or-nil (post-parameter "task")))
         (tags (loop for tag in (split-sequence #\, (post-parameter "tags"))
                     if (trim-or-nil tag) collect it))
         (estimations (extract-estimations (post-parameter "estimations")))
         (real (trim-or-nil
                 ;; Anything that's not a digit is removed
                 (#~s/[^\d+]// (post-parameter "real")))))
    (when (and task tags estimations real)
      (setf (task-name task) name
            (task-tags task) tags
            (task-estimations task) (format nil "~{~d~^+~}" estimations)
            (task-real task) (parse-int-force-pos-or-zero real))
      (update-task task the-user))))

(define-url-fn edit-task
  (let ((task (get-task (parameter "id"))))
    (when (and task (process-edit-task task the-user))
      (redirect (format nil "/listing/?d=~d"
                        (parse-iso8601-date (task-date task)))))
    (standard-page (:title "Add new record")
      (render-edit-task))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING

;(define-url-fn testing
;  (let ((a "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·º» ☐ ☑ ☒ ☓"))
;    (standard-page (:title "2test")
;      (:p (str a))
;      (esc a)
;      (loop for i from 8500 to 10000
;            do (htm (:span :title (str i) (str (code-char i)) (str " ")))))))

