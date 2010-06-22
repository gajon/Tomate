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
    (let ((username (trim-or-nil (post-parameter "username")))
          (password (trim-or-nil (post-parameter "password")))
          (timezone (parse-int-force-pos-or-zero
                      (trim-or-nil (post-parameter "timezone")))))
      (if (and (require-fields username password)
               (or (validate-credentials username password)
                   (push-error-msg "The username or password is not valid.")))
        (setf (session-value 'authenticated) "yes"
              (session-value 'username) username
              (session-value 'timezone) timezone)
        (setf (session-value 'authenticated) nil))))
  (when (string= (session-value 'authenticated) "yes")
    (redirect "/listing/"))
  ;;
  ;; Render login form
  ;;
  (standard-page (:title "Login"
                  :show-banner nil
                  :js-files ("login.js"))
    (:section :id "login"
      (show-all-messages)
      (:form :method "post" :action "."
             (hidden-input "timezone")
             (:div (text-input "Username:" "username"))
             (:div (password-input "Password:" "password"))
             (:div (submit-button "Login"))
             (:div (:a :href "/register/"
                       :class "new-account" "New account?"))))))

(define-url-fn logout
  (setf (session-value 'authenticated) nil
        (session-value 'username) "")
  (redirect "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE NEW ACCOUNT

(define-open-url-fn register
  (when (eql :post (request-method*))
    (let ((new-user (process-register-new-account)))
      (when new-user
        (setf (session-value 'authenticated) "yes"
              (session-value 'username) (user-username new-user)
              (session-value 'timezone) (user-time-zone new-user))
        (redirect "/listing/"))))
  (standard-page (:title "Create a new account"
                  :show-banner nil)
    (:section :id "register"
      (:h1 "Create a new account:")
      (show-all-messages)
      (:form :method "post" :action "."
        (hidden-input "timezone")
        (:div (text-input "Username:" "username"))
        (:div (:p "Your username can be anything, your email, numbers, whatever,
                  but beginning and ending whitespace will be removed."))
        (:div (text-input "Full name:" "full-name"))
        (:div (:p "This is optional, you can enter whatever you want.
                  It will appear on the heading of your list of tasks."))
        (:div (text-input "Location:" "current-location"))
        (:div (:p "Optional. The Pomodoro Technique encourages you to also
                  record the place you were in when you did your work."))
        (:div (password-input "Password:" "password"))
        (:div (password-input "Re-type:" "password2"))
        (:div (submit-button "Create")
              (:a :href "/" "go back"))))))

(defun process-register-new-account ()
  (let ((username (trim-or-nil (post-parameter "username")))
        (full-name (trim-or-nil (post-parameter "full-name")))
        (location (trim-or-nil (post-parameter "current-location")))
        (password (trim-or-nil (post-parameter "password")))
        (password-confirmation (trim-or-nil (post-parameter "password2")))
        (timezone (parse-int-force-pos-or-zero
                    (trim-or-nil (post-parameter "timezone")))))
    (when (and (require-fields username password password-confirmation)
               (or (string= password password-confirmation)
                   (push-error-msg "The password didn't match, try again.")))
      (or
        ;; If the username already exists, `add-user` will return nil.
        (add-user
          (make-instance 'user
                         :username username
                         :full-name (or full-name "")
                         :password-digest (hunchentoot::md5-hex password)
                         :email ""
                         :current-location (or location "")
                         :time-zone timezone))
        (push-error-msg
          "The username already exists, please select another.")))))


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
         (tasks (get-tasks-by-date today time-zone the-user))
         ;; If we have tasks, the location is the one any of these tasks
         ;; tells us; otherwise we pull it from the user information.
         (location (if tasks
                     (task-location (car tasks))
                     (user-current-location the-user))))
    (standard-page (:title "Listing of recorded tasks"
                    :active-tab :listing
                    :css-files ("tablesorter/blue/style.css")
                    :js-files ("code.js" "jquery.tablesorter.min.js"))
      ;;
      ;; DAY NAVIGATION: displays the date, location and links to
      ;; go to the previous/next day.
      ;;
      (:section :id "day-navigation"
        (:div :id "previousday"
              (:p (:a :href (conc (format nil "?d=~d" yesterday))
                      "<< Previous day")))
        (:div :id "location"
              (:h1 (esc location)
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
        (show-all-messages)
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
      (render-add-new-task today location))))

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
    (redirect (format nil "/listing/?d=~d" (url-encode (post-parameter "d")))))
  ;; Display form.
  (standard-page (:title "Add new record")
    (render-add-new-task (post-parameter "d") (post-parameter "location"))))

(defun render-add-new-task (today location)
  (with-html-output (*standard-output*)
    (:section :class "add-task"
      (show-all-messages)
      (:header (:h1 "Add new record"))
      (:form :method "post" :action "/add-new-task/"
        (hidden-input "d" :default-value (format nil "~d" today))
        (hidden-input "location" :default-value location)
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
         (location (trim-or-nil (post-parameter "location")))
         (estimations (extract-estimations (post-parameter "estimations")))
         (real (trim-or-nil
                 ;; Anything that's not a digit is removed
                 (#~s/[^\d+]// (post-parameter "real")))))
    ;; TODO: What if there's no "d" post parameter?
    (when (require-fields (task "The task name is required.")
                          (tags "A list comma separated tags is required.")
                          (estimations "An estimation is required.
                                       You can enter estimations separated by
                                       the plus sign, e.g.: 3+3+2.")
                          (real "Enter the real (final) number of pomodoros."))
      (add-task
        (make-instance 'task
                       :name task
                       :tags tags
                       :location (or location
                                     (user-current-location the-user))
                       ;; We parsed a list of estimations above
                       ;; for example, a user entry like this: "1, 2, 4"
                       ;; is parsed into a list of numbers: '(1 2 4)
                       ;; And with the following format directive we
                       ;; join the numbers with a plus sign: "1+2+4"
                       :estimations (format nil "~{~d~^+~}" estimations)
                       :real (parse-int-force-pos-or-zero real)
                       :date (format-iso8601-date date time-zone))
        the-user)
      (push-success-msg "The task has been recorded."))))

(defun extract-estimations (estimations)
  (mapcar #'parse-int-force-pos-or-zero
          (split-sequence #\+
                          ;; Anything that's not a digit is replaced by +
                          (#~s/[^\d+]/+/ estimations)
                          :remove-empty-subseqs t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EDIT A TASK

(define-url-fn fetch-task-json
  (let ((task (get-task-json (parameter "id") the-user)))
    (with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
      (setf (content-type*) "application/json")
      (if task
        (htm (str task))
        (htm #"{"status":"notfound"}"#)))))

(define-url-fn edit-task
  (let ((task (get-task (parameter "id") the-user)))
    (when (and task (process-edit-task task the-user))
      (redirect (format nil "/listing/?d=~d"
                        (parse-iso8601-date (task-date task)))))
    (standard-page (:title "Add new record")
      (:section :class "add-task"
        (show-all-messages)
        (:header (:h1 "Edit record"))
        (:form :method "post" :action "/edit-task/"
          (hidden-input "id")
          ;; TODO: Get default values from the object if there's no post data.
          (:div
            (text-input nil "task")
            (text-input nil "estimations")
            (text-input nil "real"))
          (:div (text-input nil "tags")
                (submit-button "Save")))))))

;;; TODO: WE ARE REPEATING CODE HERE!
(defun process-edit-task (task the-user)
  (let* ((name (trim-or-nil (post-parameter "task")))
         (tags (loop for tag in (split-sequence #\, (post-parameter "tags"))
                     if (trim-or-nil tag) collect it))
         (estimations (extract-estimations (post-parameter "estimations")))
         (real (trim-or-nil
                 ;; Anything that's not a digit is removed
                 (#~s/[^\d+]// (post-parameter "real")))))
    (when (require-fields (task "The task name is required.")
                          (tags "A list comma separated tags is required.")
                          (estimations "An estimation is required.
                                       You can enter estimations separated by
                                       the plus sign, e.g.: 3+3+2.")
                          (real "Enter the real (final) number of pomodoros."))
      (setf (task-name task) name
            (task-tags task) tags
            (task-estimations task) (format nil "~{~d~^+~}" estimations)
            (task-real task) (parse-int-force-pos-or-zero real))
      (update-task task the-user)
      (push-success-msg "The task has been updated."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DELETE A TASK

(define-url-fn delete-this-task
  ;; The current user can only retrieve his own tasks,
  ;; it should not be possible for a user to delete the task
  ;; of another user.
  (let ((task (get-task (post-parameter "id") the-user)))
    (if task
      (progn
        (delete-task task)
        (push-success-msg "The task has been deleted.")
        (redirect (format nil "/listing/?d=~d"
                          (parse-iso8601-date (task-date task)))))
      (redirect "/listing/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCOUNT SETTINGS

(defun process-account-settings (the-user)
  ;; It could be that the user wants to change basic info
  ;; or change her password.
  (if (post-parameter "full-name")
    (let ((full-name (trim-or-nil (post-parameter "full-name")))
          (email (trim-or-nil (post-parameter "email")))
          (location (trim-or-nil (post-parameter "current-location"))))
      (setf (user-full-name the-user) (or full-name "")
            (user-email the-user) (or email "")
            (user-current-location the-user) (or location ""))
      (update-user the-user)
      (push-success-msg "The changes were saved."))
    ;;
    ;; Change the password
    ;;
    (let ((current (trim-or-nil (post-parameter "password")))
          (new-password (trim-or-nil (post-parameter "new-password")))
          (new-password2 (trim-or-nil (post-parameter "new-password2"))))
      (when (and (require-fields (current "The current password is required.")
                                 new-password
                                 (new-password2
                                   "The password confirmation is required."))
                 (or (and (string= (hunchentoot::md5-hex current)
                                   (user-password-digest the-user))
                          (string= new-password new-password2))
                     (push-error-msg "The password didn't match, try again.")))
        (setf (user-password-digest the-user)
              (hunchentoot::md5-hex new-password))
        (update-user the-user)
        (push-success-msg "The password has been changed.")))))

(define-url-fn account
  (when (and (eql :post (request-method*))
             (process-account-settings the-user))
    (redirect (format nil "/account/")))
  (standard-page (:title "Account settings"
                  :active-tab :account)
    (:section :id "account-settings"
      (show-all-messages)
      (:h1 "Account settings:")
      (:div
        (:form :method "post" :action "."
          (:div (text-input "Username:" "username"
                            :default-value (user-username the-user)
                            :disabled t))
          (:div (text-input "Full-name:" "full-name"
                            :default-value (user-full-name the-user))
                "Optional")
          (:div (text-input "E-Mail:" "email"
                            :default-value (user-email the-user)) "Optional")
          (:div (text-input "Location:" "current-location"
                            :default-value (user-current-location the-user))
                "Optional")
          (:div (submit-button "Save changes")))))
    (:section :id "account-change-password"
      (:h1 "Change password:")
      (:div
        (:form :method "post" :action "."
          (:div (password-input "Current:" "password"))
          (:div (password-input "New password:" "new-password"))
          (:div (password-input "Confirm:" "new-password2"))
          (:div (submit-button "Change")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING

;(define-url-fn testing
;  (let ((a "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·º» ☐ ☑ ☒ ☓"))
;    (standard-page (:title "2test")
;      (:p (str a))
;      (esc a)
;      (loop for i from 8500 to 10000
;            do (htm (:span :title (str i) (str (code-char i)) (str " ")))))))

