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
;;; Server Port and Directories

(defvar *server-port* 8080)
(defvar *webapp-home* *default-pathname-defaults*)
(defvar *static-web-files* (merge-pathnames "static/" *webapp-home*))
(defvar *data-dumps-home* (merge-pathnames "data-dumps/" *webapp-home*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other Hunchentoot Settings

(push 
  (create-folder-dispatcher-and-handler "/static/" *static-web-files*)
  *dispatch-table*)

(setf *access-log-pathname*
      (merge-pathnames "access.txt" *webapp-home*))

(setf *message-log-pathname*
      (merge-pathnames "messages.txt" *webapp-home*))

(setf *default-content-type* "text/html; charset=UTF-8")

(setf *hunchentoot-default-external-format* hunchentoot::+utf-8+)
;(flexi-streams:make-external-format :utf-8)

;; Sessions time out after 12 hours of no use.
(setf *session-max-time* (* 12 60 60))

(setf *show-lisp-errors-p* t)
(setf *catch-errors-p* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CouchDB Settings

(clouchdb:set-connection :name "tomate")
;;  set-connection &key host db-name protocol port
;;                      document-update-fn document-fetch-fn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings for CL-WHO

(setf (cl-who:html-mode) :SGML)

;;; We want HTML5 Doctype
(setf cl-who:*prologue* "<!DOCTYPE html>")
