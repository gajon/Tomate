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
;;; Classes

;;; TODO: Is it a bad idea to carry the _id and _rev data all along?
;;; For the user, the username is the id of the document.
(defclass user ()
  ((_rev :initarg :rev :reader user-rev)
   (full-name :initarg :full-name :accessor user-full-name)
   (username :initarg :username :accessor user-username)
   (password-digest :initarg :password-digest :accessor user-password-digest)
   (email :initarg :email :accessor user-email)
   (current-location :initarg :current-location :accessor user-current-location)
   (time-zone :initarg :time-zone :accessor user-time-zone))
  (:documentation ""))

(defclass task ()
  ((_id :initarg :id :reader task-id)
   (_rev :initarg :rev :reader task-rev)
   (name :initarg :name :accessor task-name)
   (tags :initarg :tags :initform nil :accessor task-tags)
   (date :initarg :date
         :initform (format-iso8601-date (make-date (get-universal-time) 6))
         :reader task-date)
   (location :initarg :location :initform "" :accessor task-location)
   (estimations :initarg :estimations :initform 0 :accessor task-estimations)
   (real :initarg :real :initform 0 :accessor task-real)
   (total-estimated :initform 0 :accessor task-total-estimated))
  (:documentation ""))

(defmethod initialize-instance :after ((task task) &key)
  (setf (task-total-estimated task)
        (reduce #'+ (extract-estimations (task-estimations task)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods to get a nice REPL display.

(defmethod print-object ((u user) stream)
  (print-unreadable-object (u stream :identity t :type t)
    (format stream "~a (~a)" (user-username u) (user-full-name u))))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :identity t :type t)
    (let ((date (parse-iso8601-date (task-date task))))
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (date-universal-time date)
                                 (date-time-zone date))
        (declare (ignore s m h))
        (format stream "~a [~a] (date: ~4,'0d-~2,'0d-~2,'0d)"
                (task-name task)
                (task-tags task)
                year month day)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formatted Data

(defgeneric fmt-task-tags (task)
  (:documentation
    "Returns a string of the tags that this task belongs to, each
separated by a comma. Example: \"Tag1, Tag2, Tag3\""))

(defmethod fmt-task-tags ((task task))
  (format nil "~{~a~^, ~}" (task-tags task)))

