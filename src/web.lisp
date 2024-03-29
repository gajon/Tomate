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
          (timezone (parse-int-or-force-value (post-parameter "timezone") 6)))
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
  ;; Render login page
  ;;
  (standard-page (:title "Pomolog.com, Record your achievements while using the Pomodoro Technique."
                  :show-banner nil)
    (:div :id "login"
      (:section :id "login-text1"
        ;; Login Form
        (:div :id "login-form"
          (show-all-messages)
          (:form :method "post" :action "."
                 (hidden-input "timezone")
                 (:div (text-input "Username:" "username"))
                 (:div (password-input "Password:" "password"))
                 (:div (submit-button "Login"))
                 (:div (:p (:a :href "/register/"
                               :class "new-account" "Create new account")
                           (:br)
                           "It's 100% free and only takes 30 seconds. "
                           "No email required"))))
        ;; End login form
        #>END_OF_HTML
        <h1>Are you using the Pomodoro Technique?</h1>

        <p>Keep using your notebook or any piece of paper to plan your taks
        and estimate your pomodoros.</p>

        <p>You've gotta admit nothing's better than using a physical &amp;
        tangible tool when you just want to focus on your work and get
        things done.</p>

        <div style="clear:both;"></div>
        END_OF_HTML)

      (:section :id "login-text2"
        #>END_OF_HTML
        <h2>Record your achievements with this tool.</h2>

        <p>At the end of the day, week or month (whenever you want) log in
        and record the total number of pomodoros you achieved. It's that
        simple!</p>
        END_OF_HTML)

      (:section :id "login-text3"
        #>END_OF_HTML
        <h2>Why?</h2>

        <p>So you can easily see your improvement in gettings things done;
        be aware of where your time goes; if you are spending
        disproportionaly more time on things that are not that important; or
        if you are getting better at estimating effort over time.</p>
        END_OF_HTML)

      (:section :id "login-text4"
        #>END_OF_HTML
        <h1>Pomo.. WHAT??</h1>

        <blockquote cite="http://en.wikipedia.org/wiki/Pomodoro_Technique">
          <p>“The Pomodoro Technique is a time management method
          developed by Francesco Cirillo in the late 1980s. The technique
          uses a timer to break down periods of work into 25-minute
          intervals called 'pomodoros' (from the Italian word for 'tomato')
          separated by breaks. Closely related to concepts such as
          timeboxing  and iterative and incremental development used in
          software design, the method has been adopted in pair programming
          contexts.</p>

          <p>The method is based on the idea that frequent breaks can
          improve mental agility and seeks to provide an effective response
          to time as an anxiety-provoking state referred to as temporal
          'becoming' ...”</p>
        </blockquote>

        <p style="text-align:right;">
        <cite><a href="http://en.wikipedia.org/wiki/Pomodoro_Technique">
        http://en.wikipedia.org/wiki/Pomodoro_Technique</a></cite></p>
        END_OF_HTML)

      (:section :id "login-text5"
        #>END_OF_HTML
        <p>You can learn more about the technique, read the book from the
        original author and download other resources from its web page:</p>
        <p><a href="http://www.pomodorotechnique.com/">
        http://www.pomodorotechnique.com/</a></p>
        END_OF_HTML))

    (:script
      #>SCRIPT
      var usernameInput = document.getElementById("id_username");
      usernameInput.focus();
      usernameInput.select();

      var timezoneInput = document.getElementById("id_timezone");
      timezoneInput.value = (new Date()).getTimezoneOffset() / 60;
      SCRIPT)

    (:script :type "text/javascript"
    #>GOOGLEANALYTICS
    var _gaq = _gaq || [];
    _gaq.push(['_setAccount', 'UA-17396999-1']);
    _gaq.push(['_trackPageview']);
    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();
    GOOGLEANALYTICS)))

      

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
        ;; Add a couple of record examples so that the new user
        ;; has a better idea of how to start recording his own tasks.
        (add-task
          (make-instance 'task
                         :name "I'm an example task record, please delete me!"
                         :tags '("example" "work" "office")
                         :location (user-current-location new-user)
                         :estimations "4" :real 4
                         :date (format-iso8601-date
                                 (make-date (get-universal-time)
                                            (user-time-zone new-user))))
          new-user)
        (add-task
          (make-instance 'task
                         :name "This is another example, first estimation was 3 then re-estimated 2 more pomodoros. Please delete me!"
                         :tags '("example" "work" "estimations")
                         :location (user-current-location new-user)
                         :estimations "3+2" :real 4
                         :date (format-iso8601-date
                                 (make-date (get-universal-time)
                                            (user-time-zone new-user))))
          new-user)
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
        (:div (text-input "Location:" "current-location"))
        (:div (:p "Optional. The Pomodoro Technique encourages you to also
                  record the place you were in when you did your work."))
        (:div (password-input "Password:" "password"))
        (:div (password-input "Re-type:" "password2"))
        (:div (submit-button "Create")
              (:a :href "/" "go back"))))
    (:section :id "register-terms"
      (:div (:p (:strong "I promise you this:")))
      (:div (:ul
              (:li "I'll never use, sell, spy on, trade, or disseminate your
                   information. I truly respect your privacy.")
              (:li "You'll always be able to download your records to your
                   computer; your information is yours, and I'll never lock
                   you here.")))
      (:div (:p (:strong "And you promise me this:")))
      (:div (:ul
              (:li "Don't send unsolicited information to others (e.g. Spam).")
              (:li "Don't attempt to violate the security of this application
                   or do anything with the intention of accessing the
                   information of other users.")
              (:li "Be nice to others :)"))))

    (:script :type "text/javascript"
    #>GOOGLEANALYTICS
    var _gaq = _gaq || [];
    _gaq.push(['_setAccount', 'UA-17396999-1']);
    _gaq.push(['_trackPageview']);
    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();
    GOOGLEANALYTICS)))


(defun process-register-new-account ()
  (let ((username (trim-or-nil (post-parameter "username")))
        (location (trim-or-nil (post-parameter "current-location")))
        (password (trim-or-nil (post-parameter "password")))
        (password-confirmation (trim-or-nil (post-parameter "password2")))
        (timezone (parse-int-or-force-value (post-parameter "timezone") 6)))
    (when (and (require-fields username password password-confirmation)
               (or (string= password password-confirmation)
                   (push-error-msg "The password didn't match, try again.")))
      (or
        ;; If the username already exists, `add-user` will return nil.
        (add-user
          (make-instance 'user
                         :username username
                         :full-name ""
                         :password-digest (hunchentoot::md5-hex password)
                         :email ""
                         :current-location (or location "")
                         :time-zone timezone))
        (push-error-msg
          "The username already exists, please select another.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LISTING

(define-url-fn listing
  (let* ((today (or (parse-date (parameter "d") time-zone)
                    (get-last-date-with-records the-user time-zone)
                    (make-date (get-universal-time) time-zone)))
         (yesterday (make-date (date- today %secs-in-one-day)))
         (tomorrow  (make-date (date+ today %secs-in-one-day)))
         (tasks (get-tasks-by-date today the-user))
         ;; If we have tasks, the location is the one any of these tasks
         ;; tells us; otherwise we pull it from the user information.
         (location (if tasks
                     (task-location (car tasks))
                     (user-current-location the-user))))
    (standard-page (:title "Listing of recorded tasks"
                    :active-tab :listing
                    :css-files ("tablesorter/blue/style.css"
                                "jquery-ui-custom-theme/jquery-ui-1.8.2.custom.css")
                    :js-files ("jquery-1.4.2.min.js"
                               "jquery.tablesorter.min.js"
                               "jquery-ui-1.8.2.custom.min.js"
                               "code.js"))
      ;;
      ;; DAY NAVIGATION: displays the date, location and links to
      ;; go to the previous/next day.
      ;;
      (:section :id "day-navigation"
        (:div :id "previousday"
              (:p (:a :href (conc (format nil "?d=~a" (format-date yesterday)))
                      "&lt;&lt; Previous day")))
        (:div :id "location"
              (:h1 (esc location) ", " (esc (format-date today :longform t)))
              (:p "Record sheet"
                  (text-input nil "datepicker"
                              :default-value (format-date today))
                  (:button :id "id_changelocation_button"
                           :class "ui-datepicker-trigger"; reuse datepicker css
                           "[location]")))
        (:div :id "nextday"
              (:p (:a :href (conc (format nil "?d=~a" (format-date tomorrow)))
                      "Next day &gt;&gt;"))))
      ;;
      ;; This div is used to change the location, it is displayed
      ;; when the button defined above is clicked on.
      ;;
      (:div :id "id_changelocation"
            :style "display:none;"
            (:form :method "post" :action "/change-location-day/"
              (:p
                (hidden-input "d" :default-value (format-date today))
                (text-input nil "location" :default-value location)
                (submit-button "Change"))
              (:p (:strong "Note:")
                  " This will change the location of<br>this day only.")))
      ;;
      ;; THE MAIN LISTING
      ;;
      (:section :class "listing"
        (show-all-messages)
        (:table :id "task-listing"
                :class "tablesorter"
                :cellspacing 1 :cellpadding 0
          (table-columns ("Task" :style "width:100%;")
                         "Estimated" "Real" "Difference")
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
  (let ((today (or (parse-date (post-parameter "d") time-zone)
                   (make-date (get-universal-time) time-zone))))
    ;; Validate, create new object and redirect.
    (when (process-add-new-task the-user today)
      (redirect (format nil "/listing/?d=~a" (format-date today))))
    ;; Display form.
    (standard-page (:title "Add new record"
                    :active-tab :listing
                    :js-files ("jquery-1.4.2.min.js"))
      (render-add-new-task today (post-parameter "location")))))

(defun render-add-new-task (today location)
  (with-html-output (*standard-output*)
    (:section :class "add-task"
      (show-all-messages)
      (:header (:h1 "Add new record"))
      (:form :method "post" :action "/add-new-task/"
        (hidden-input "d" :default-value (format-date today))
        (hidden-input "location" :default-value location)
        (:div
          (text-input nil "task" :default-value "Task description")
          (text-input nil "estimations" :default-value "Estimated")
          (text-input nil "real" :default-value "Real"))
        (:div (text-input nil "tags" :default-value "List of tags")
              (submit-button "Add record"))
        (:div :class "help-toggle"
          (:p (:a :id "help-toggle" :href "#" "Show help.")))
        (:div :class "help hidden"
          (:p "The task description is free form, you can enter whatever you
              want there.")
          (:p "The estimations are a series of numbers separated by the plus
              (+) sign; for example, \"3+2\" means that your first
              estimation was 3 pomodoros, but after that you re-estimated
              2 more pomodoros to finish the task.")
          (:p "The next field is for the actual number of pomodoros that you
              spent on this task, which could be less than your estimations.")
          (:p "Finally you can enter a comma separated list of tags related
              to this task. This will help you see in the reports how you
              spend your time among different types of activities. For example
              I use the list of tags \"projects, pomolog, programming\" for 
              tasks involving the development of this web application, and
              \"books, entertainment\" for leisure reading while \"books,
              learning\" for technical books."))))
    (:script "$('#help-toggle').click(function(){$('.help').toggle();});")))

(defun process-add-new-task (the-user today)
  (let* ((task (trim-or-nil (post-parameter "task")))
         (tags (loop for tag in (split-sequence #\, (post-parameter "tags"))
                     if (trim-or-nil tag) collect it))
         (location (trim-or-nil (post-parameter "location")))
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
                       :date (format-iso8601-date today))
        the-user)
      (push-success-msg "The task has been recorded."))))


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
      (redirect (format nil "/listing/?d=~a"
                        (format-date (parse-iso8601-date (task-date task))))))
    (standard-page (:title "Add new record"
                    :active-tab :listing)
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
        (redirect (format nil "/listing/?d=~a"
                          (format-date (parse-iso8601-date (task-date task))))))
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
                "Optional"
                (:em "If supplied it will be shown in the community
                     forums, otherwise only your username will be shown."))
          (:div (text-input "E-Mail:" "email"
                            :default-value (user-email the-user))
                "Optional")
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
          (:div (submit-button "Change")))))
    (:section :id "account-download-data"
      (:h1 "Download your records:")
      (:div
        (:form :method "post" :action "/download-data/"
          (:div
            (:label :for "id_download_fmt" "File format:")
            (:select :id "id_download_fmt"
                     :name "download_fmt"
                     (:option :value "csv" "CSV (comma separated values)")))
          (:div (submit-button "Download"))
          (:div :class "help"
            (:p "For the moment there's only CSV format. To open the
                downloaded file you can use a program like MS Excel,
                OpenOffice.org or similar.")
            (:p "Sometimes when you directly open the file with MS Excel, it
                won't recognize the character set and display junk. To fix
                that open a blank spreadsheet and then look for an option to
                insert external data from a file. Select the downloaded file
                and when prompted indicate that the file uses UTF-8 for
                encoding, and that the fields are separated by commas and
                surrounded by double quotes (&quot;).")))))))

(define-url-fn download-data
  (unless (eql :post (request-method*))
    (redirect "/account/"))
  ;; TODO: We only have CSV for now.
  (let ((file (export-data the-user)))
    (setf (hunchentoot:header-out :content-disposition)
          (format nil "attachment; filename=\"~a\"" (file-namestring file)))
    (hunchentoot:handle-static-file file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHANGE THE LOCATION OF A SINGLE DAY.
;;; AND SET THE LOCATION AS THE NEW DEFAULT LOCATION.

(define-url-fn change-location-day
  (let ((today (parse-date (post-parameter "d") time-zone))
        (location (trim-or-nil (post-parameter "location"))))
    (if (and today location)
      ;; We update the location of the tasks recoded on this day
      ;; and also update the default location of the user.
      (progn
        (mapcar (lambda (task)
                  (setf (task-location task) location)
                  (update-task task the-user))
                (get-tasks-by-date today the-user))
        (setf (user-current-location the-user) location)
        (update-user the-user)
        (push-success-msg "Location has been changed.")
        (redirect (format nil "/listing/?d=~a" (format-date today))))
      ;;
      ;; Post parameters not found or valid, bail out!
      ;;
      (redirect "/logout/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPORTS

(defmacro! embed-chart (source &key (width 756) (height 400))
  (let ((source (url-encode (mkstr "/ofc-test-json/?data=" source))))
    `(with-html-output (*standard-output*)
       (:script :type "text/javascript"
         ,(format nil "swfobject.embedSWF('/static/open-flash-chart.swf',
                                          '~a','~d','~d','9.0.0',
                                          'expressInstall.swf',
                                          {'data-file':'~a'});"
                  g!div-id width height source))
       (:div :id ,(format nil "~a" g!div-id)))))

;(embed-chart estimated)


(define-url-fn reports
  (push-info-msg "This section is under development, keep looking for updates.")
  (push-info-msg "And remember to visit the \"Community\" page to suggest reports you'd like to see.")
  (standard-page (:title "Reports about your data"
                  :active-tab :reports
                  :css-files ("jquery-ui-custom-theme/jquery-ui-1.8.2.custom.css")
                  :js-files ("jquery-1.4.2.min.js"
                             "jquery-ui-1.8.2.custom.min.js"
                             "swfobject.js"))
    (:section :id "reports"
      (show-all-messages)
      (:div :class "chart"
        (:h1 "Some stats about your work")
        (display-stats the-user))
      (:div :class "chart"
        (:h1 "Real number of pomodoros over the last 30 days")
        (embed-chart estimated-vs-real))
      (:div :class "chart"
        (:h1 "Tags and number of tasks for each")
        (embed-chart tags :height 500)))))

(defun display-stats (the-user)
  (multiple-value-bind (dates pomodoros tasks)
      ;; TODO: Instead of :days 9999 make the function accept :days nil
      (get-real-pomodoros-and-tasks-count-by-days the-user :days 9999)
    (let* ((total-pomodoros (reduce #'+ pomodoros))
           (total-tasks (reduce #'+ tasks))
           (total-days (length dates))
           (average-pomodoros (if (> total-days 0)
                                (/ total-pomodoros total-days)
                                0)))
      (with-html-output (*standard-output*)
        (:table :id "stats-table" :cellspacing 1 :cellpadding 0
          (:tbody
            (:tr
              (:td "Total pomodoros:")
              (:td :class "text-center" (str total-pomodoros)))
            (:tr
              (:td "Total tasks:")
              (:td :class "text-center" (str total-tasks)))
            (:tr
              (:td "Days with records:")
              (:td :class "text-center" (str total-days)))
            (:tr
              (:td "Average pomodoros per day:")
              (:td :class "text-center"
                   (fmt "~,2f" average-pomodoros)))))))))

(define-url-fn ofc-test-json
  (with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (setf (content-type*) "application/json")
    (cond
      ;;
      ;; Real number of pomodoros by date (last 30 days)
      ;;
      ((string-equal (parameter "data") "ESTIMATED-VS-REAL")
       (multiple-value-bind (x-dates y-pomodoros)
           (get-real-pomodoros-and-tasks-count-by-days the-user :days 30)
         (let* ((x-axis (make-instance
                          'chart-x-axis
                          :offset-p t
                          :steps 1
                          :labels (make-instance 'chart-x-axis-labels
                                                 :labels x-dates
                                                 :rotate 270)))
                (y-axis (make-instance 'chart-y-axis
                                       :max-range 20
                                       :steps 2))
                (x-legend (make-instance 'chart-x-legend
                                         :text "Last 30 days with records"))
                (y-legend (make-instance 'chart-y-legend
                                         :text "Number of pomodoros"))
                (bar (make-instance 'chart-bar
                                    :colour "#0000ff"
                                    :values y-pomodoros
                                    :text "Real pomodoros"
                                    :on-show (make-instance
                                               'chart-bar-show
                                               :type "grow-up"
                                               :cascade 0.5)
                                    :font-size 10))
                (chart (make-instance 'chart
                                      :bg-colour "#ffffff"
                                      :x-axis x-axis
                                      :y-axis y-axis
                                      :x-legend x-legend
                                      :y-legend y-legend
                                      :elements (list bar))))
           (let ((json:*lisp-identifier-name-to-json* 'ofc-lisp-to-json-name))
             (htm (str (json:encode-json-to-string chart)))))))
      ;;
      ;; TAGS and the number of tasks recorded for each tag.
      ;;
      ((string-equal (parameter "data") "TAGS")
       (multiple-value-bind (x-tags y-tasks max-value) 
           (get-all-tags-with-number-of-tasks the-user)
         (let* ((x-axis (make-instance
                          'chart-x-axis
                          :offset-p t
                          :steps 1
                          :labels (make-instance 'chart-x-axis-labels
                                                 :labels x-tags
                                                 :size 12
                                                 :rotate 270)))
                (y-axis (make-instance 'chart-y-axis
                                       :max-range (1+ max-value)
                                       :steps 1))
                (x-legend (make-instance 'chart-x-legend
                                         :text "Tags"))
                (y-legend (make-instance 'chart-y-legend
                                         :text "Number of tasks"))
                (bar (make-instance 'chart-bar
                                    :colour "#0000ff"
                                    :values y-tasks
                                    :text "Number of tasks"
                                    :on-show (make-instance
                                               'chart-bar-show
                                               :type "grow-up"
                                               :cascade 0.5)
                                    :font-size 10))
                (chart (make-instance 'chart
                                      :bg-colour "#ffffff"
                                      :x-axis x-axis
                                      :y-axis y-axis
                                      :x-legend x-legend
                                      :y-legend y-legend
                                      :elements (list bar))))
           (let ((json:*lisp-identifier-name-to-json* 'ofc-lisp-to-json-name))
             (htm (str (json:encode-json-to-string chart))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMUNITY

(define-url-fn community
  (let* ((board (max (parse-int-force-pos-or-zero (parameter "board")) 1))
         (topics (get-all-topics board)))
    (standard-page (:title "Community discussion boards"
                    :active-tab :community)
      (display-board-selection)
      ;;
      ;; List of topics
      ;;
      (:section :id "community-topics"
        (show-all-messages)
        (:div :class "title"
          (case board
            (1 (htm (:h1 "General discussions:")))
            (2 (htm (:h1 "Ideas or suggestions:")))
            (3 (htm (:h1 "Problems and bug reports:"))))
          (:a :href (format nil "/community-new-topic/?board=~a" board)
              "Create new topic"))
        (loop for topic in topics
              for topic-id = (topic-id topic)
              for topic-url = (format nil "/community-topic/?topic=~a" topic-id)
              for topic-msg-count = (get-topic-messages-count topic-id)
              do
              (htm
                (:div :class "topic"
                  (:a :href topic-url (esc (topic-title topic)))
                  (:em (esc (format nil "~a message~p"
                                    topic-msg-count
                                    topic-msg-count))))))))))

(define-url-fn community-topic
  (let* ((topic-id (parameter "topic"))
         (topic (get-topic topic-id))
         (messages (get-topic-messages topic-id)))
    (unless topic (redirect "/community/"))
    ;;
    ;; Sent a new reply, save it.
    ;;
    (when (eql :post (request-method*))
      (let ((reply (trim-or-nil (post-parameter "reply"))))
        (when (require-fields (reply "You cannot send an empty reply."))
          (add-topic-msg
            (make-instance 'topic-msg
                           :topic topic-id
                           :date (format-iso8601-date
                                   (make-date (get-universal-time) time-zone))
                           :user (user-username the-user)
                           :message reply))
          (push-success-msg "Your reply has been sent.")
          (redirect (format nil "/community-topic/?topic=~a" topic-id)))))
    (standard-page (:title "Community discussion boards"
                    :active-tab :community)
      (display-board-selection)
      ;;
      ;; The topic and its messages
      ;;
      (:section :id "community-topic"
        (show-all-messages)
        (:h1 (esc (topic-title topic)))
        (loop for msg in messages
              for msg-user = (get-user-obj (topic-msg-user msg)) do
              (htm
                (:div :class "message"
                  (:p (esc (topic-msg-message msg)))
                  (:div :class "signature"
                    (esc (or (trim-or-nil (user-full-name msg-user))
                             (user-username msg-user)))
                    (:br)
                    (esc (format-date (parse-iso8601-date (topic-msg-date msg))
                                      :longform t))))))
        ;;
        ;; Form to add a new reply
        ;;
        (:section :id "community-topic-reply"
          (:h1 "Add a reply:")
          (:form :method "post" :action "."
                 (hidden-input "topic" :default-value topic-id)
                 (:div (text-area nil "reply"))
                 (:div :class "submit" (submit-button "Send reply"))))))))

(define-url-fn community-new-topic
  (let ((board (parse-int-force-pos-or-zero (parameter "board"))))
    (unless (> board 0) (redirect "/community/"))
    ;;
    ;; Process the new topic.
    ;;
    (when (eql :post (request-method*))
      (let ((title (trim-or-nil (post-parameter "title")))
            (message (trim-or-nil (post-parameter "message"))))
        (when (require-fields title message)
          ;; We have to create two documents, one is the topic itself
          ;; and the other is the first message under this topic.
          (let ((new-topic
                  (add-topic
                    (make-instance 'topic
                                   :board board
                                   :title title
                                   :date (format-iso8601-date
                                           (make-date (get-universal-time)
                                                      time-zone))
                                   :user (user-username the-user)))))
            (add-topic-msg (make-instance 'topic-msg
                                          :topic (topic-id new-topic)
                                          :date (topic-date new-topic)
                                          :user (topic-user new-topic)
                                          :message message))
            (push-success-msg "Your topic has been created.")
            (redirect (format nil "/community-topic/?topic=~a"
                              (topic-id new-topic)))))))
    ;;
    ;; Display the form and error messages.
    ;;
    (standard-page (:title "Community discussion boards"
                    :active-tab :community)
      (display-board-selection)
      (:section :id "community-new-topic"
        (show-all-messages)
        (:h1 "Create a new topic")
        (:form :method "post" :action "."
               (hidden-input "board" :default-value (mkstr board))
               (:div (text-input "Title:" "title"))
               (:div (text-area "Message:" "message"))
               (:div (submit-button "Save")))))))

(defun display-board-selection ()
  (with-html-output (*standard-output*)
    (:section :id "community-board-selection"
      (:h1 "Community discussion boards (select one to view its topics).")
      (:div :class "boards"
        (:a :href "/community/?board=1" "General discussions") " | "
        (:a :href "/community/?board=2" "Ideas or suggestions") " | "
        (:a :href "/community/?board=3" "Problems and bug reports")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FOOTER PAGES

(define-open-url-fn updates
  (standard-page (:title "Updates"
                  :active-tab :community)
    (:section :id "updates"
      (:h1 "Updates")
      (:p "You can see the full set of changesets and the source code of
          this application at its Mercurial repository: "
          (:a :href "http://bitbucket.org/gajon/tomate"
              :target "_blank" "http://bitbucket.org/gajon/tomate"))
      (:div :class "updates"
        (:h2 "2010-08-08")
        (:ul
          (:li "Finally added the &quot;Data export&quot; feature under the "
               (:a :href "/account/" "account") " tab. I'm sorry for the
               big delay; I've been a little bit busy with other projects.")))
      (:div :class "updates"
        (:h2 "2010-07-10")
        (:ul
          (:li "Added the discussion forums under the "
               (:a :href "/community/" "community") " tab.")
          (:li "It's possible to use your full name, if you wish to do so, for
          the discussion forums.")))
      (:div :class "updates"
        (:h2 "2010-07-07")
        (:ul
          (:li "Added the first two charts under the "
               (:a :href "/reports/" "reports") " tab. There's still a lot to
               do; the intention is to be able to select the charts that you
               would like to see and customize them.")))
      (:div :class "updates"
        (:h2 "2010-07-06")
        (:ul
          (:li "We are live at pomolog.com!"))))))

(define-open-url-fn about
  (standard-page (:title "About this application"
                  :active-tab :community)
    (:section :id "about"
      (:h1 "About this application")
      (:p "I had been using the Pomodoro Technique for eight months when I
          decided to create this little tool. I've always used a notebook, a
          pen and a countdown timer on my cellphone.")

      (:p "I never wanted to use a software tool to plan my tasks and
          estimate pomodoros; I love working with paper and I will continue
          to do so. But there is a problem, it's difficult to get valuable
          information from your records when they are only in paper.")

      (:p "Hence this tool was born, and the reason of its extremely narrow
          focus of only recording spent pomodoros.")

      (:p "If your are as crazy as me and enjoy working with pen and paper,
          I hope that this tool is useful to you. I'll make three promises
          to you:")

      (:ul
        (:li "I'll keep this tool forever free.")
        (:li "I'll never use, sell, spy on, trade, or disseminate your
             information. I truly respect your privacy.")
        (:li "You'll always be able to download your records to your
             computer; your information is yours, and I'll never lock you
             here."))

      (:p "I only request the following from you:")
      (:ul
        (:li "Don't send unsolicited information to others (e.g. Spam).")
        (:li "Don't attempt to violate the security of this application or do
             anything with the intention of accessing other's information.")
        (:li "Be nice to others :)"))

      (:h1 "Who is this crazy geek?")

      (:p "I'm a software developer living in Mexico City. Last November I
          quitted my last " (:em "formal") " job and have been freelancing
          since then. With the drastic change in working style I needed to
          be more aware of how I used my time; that's why I started
          using the Pomodoro Technique, with great results.")

      (:p "I have a programming related blog at "
          (:a :href "http://gajon.org" :target "_blank" "http://gajon.org")
          ", and you can reach me via email at gajon@gajon.org; just be
          careful with the subject line, i.e. don't use a subject like
          \"Business proposition\" or your email will go straight to the
          trash can.")

      (:p "You can also find me in twitter at: "
          (:a :href "http://twitter.com/gajon" :target "_blank"
              "http://twitter.com/gajon")))))

(define-open-url-fn credits
  (standard-page (:title "Credits"
                  :active-tab :community)
    (:section :id "credits"
      (:h1 "Credits")
      (:p #"This application was developed with <a target="_blank"
          href="http://en.wikipedia.org/wiki/Common_Lisp">ANSI Common Lisp</a>,
          using <a href="http://www.sbcl.org/" target="_blank">Steel Bank
          Common Lisp</a> (SBCL) for compiling it."#)
      (:p #"This runs atop <a href="http://weitz.de/hunchentoot/"
          target="_blank">Hunchentoot</a>, a web server also written in Common
          Lisp by Edi Weitz. Another piece of <em>Ediware</em> being used is
          <a href="http://weitz.de/cl-who/" target="_blank">CL-WHO</a> which
          lets you convert Lisp S-Expressions into (X)HTML code."#)
      (:p #"<a target="_blank" href="http://couchdb.apache.org/">CouchDB</a>
          is a document-oriented database used by this application for data
          storage and retrieval. To talk to CouchDB from Common Lisp, I used a
          library by Peter Eddy called <a target="_blank" 
          href="http://common-lisp.net/project/clouchdb/">clouchdb</a>."#)
      (:p #"There's some JavaScript thrown in there, with the help of
          <a target="_blank" href="http://jquery.com/">jQuery</a>. And to
          generate the charts I used the nice <a target="_blank"
          href="http://teethgrinder.co.uk/open-flash-chart-2/">Open Flash
          Chart</a> open source tool."#)
      (:p #"My favorite editor is <a target="_blank" href="http://www.vim.org/">
          Vim</a>, which I've been using for years."#)
      (:p #"My operating system of choice is <a href="http://slackware.com/"
          target="_blank">Slackware GNU/Linux</a>, which I use in my workstation
          as well as in the server this application runs on."#)
      (:h1 "This application's source code is available")
      (:p "You can get the complete source code of this application from
          it's Mercurial repository, located at:")
      (:p (:a :href "http://bitbucket.org/gajon/tomate/" :target "_blank"
              "http://bitbucket.org/gajon/tomate/"))
      (:p "It is released under an MIT license.")
      (:p "Keep in mind that this is my very first application in Common Lisp,
          and I still need to gain more experience with it. It is a completely
          different beast when compared with most popular languages like Java,
          C# and even Python. What I'm trying to say is that if you are trying
          to learn something from it be very critical of what you see. You
          should always do that anyways."))
      (:section :id "credits-logos"
        (:img :src "/static/logos.jpg" :alt "logos"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTING

;(define-url-fn testing
;  (let ((a "¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·º» ☐ ☑ ☒ ☓"))
;    (standard-page (:title "2test")
;      (:p (str a))
;      (esc a)
;      (loop for i from 8500 to 10000
;            do (htm (:span :title (str i) (str (code-char i)) (str " ")))))))

