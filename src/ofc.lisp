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

(defclass chart ()
  ((title        :initarg :title        :accessor chart-title)
   (x_axis       :initarg :x-axis       :accessor chart-x-axis)
   (y_axis       :initarg :y-axis       :accessor chart-y-axis)
   (y_axis_right :initarg :y-axis-right :accessor chart-y-axis-right)
   (x_legend     :initarg :x-legend     :accessor chart-x-legend)
   (y_legend     :initarg :y-legend     :accessor chart-y-legend)
   (bg_colour    :initarg :bg-colour    :accessor chart-bg-colour)
   (elements     :initarg :elements     :accessor chart-elements :initform nil)
   (radar_axis   :initarg :radar-axis   :accessor chart-radar-axis)
   (tooltip      :initarg :tooltip      :accessor chart-tooltip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chart-title ()
  ((text  :initarg :text  :accessor chart-text)
   (style :initarg :style :accessor chart-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chart-x-axis ()
  ((stroke      :initarg :stroke      :accessor chart-stroke)
   (tick-height :initarg :tick-height :accessor chart-tick-height)
   (colour      :initarg :colour      :accessor chart-colour)
   (grid-colour :initarg :grid-colour :accessor chart-grid-colour)
   (min         :initarg :min-range   :accessor chart-min-range)
   (max         :initarg :max-range   :accessor chart-max-range)
   (steps       :initarg :steps       :accessor chart-steps)
   (offset      :initarg :offset-p    :accessor chart-offset-p)
   (labels      :initarg :labels      :accessor chart-labels)
   (3d          :initarg :3d          :accessor chart-3d)))

(defclass chart-y-axis ()
  ((stroke      :initarg :stroke      :accessor chart-stroke)
   (tick-length :initarg :tick-length :accessor chart-tick-length)
   (colour      :initarg :colour      :accessor chart-colour)
   (grid-colour :initarg :grid-colour :accessor chart-grid-colour)
   (min         :initarg :min-range   :accessor chart-min-range)
   (max         :initarg :max-range   :accessor chart-max-range)
   (steps       :initarg :steps       :accessor chart-steps)
   (offset      :initarg :offset-p    :accessor chart-offset-p)
   (labels      :initarg :labels      :accessor chart-labels)
   (rotate      :initarg :rotate      :accessor chart-rotate)))

(defclass chart-x-axis-labels ()
  ((steps         :initarg :steps         :accessor chart-steps)
   (visible-steps :initarg :visible-steps :accessor chart-visible-steps)
   (labels        :initarg :labels        :accessor chart-labels)
   (colour        :initarg :colour        :accessor chart-colour)
   (size          :initarg :size          :accessor chart-size)
   (rotate        :initarg :rotate        :accessor chart-rotate)
   (text          :initarg :text          :accessor chart-text)))

(defclass chart-x-axis-label ()
  ((text    :initarg :text    :accessor chart-text)
   (colour  :initarg :colour  :accessor chart-colour)
   (size    :initarg :size    :accessor chart-size)
   (rotate  :initarg :rotate  :accessor chart-rotate)
   (visible :initarg :visible :accessor chart-visible)))

(defclass chart-y-axis-labels ()
  ((steps  :initarg :steps  :accessor chart-steps)
   (labels :initarg :labels :accessor chart-labels)
   (colour :initarg :colour :accessor chart-colour)
   (size   :initarg :size   :accessor chart-size)
   (rotate :initarg :rotate :accessor chart-rotate)
   (text   :initarg :text   :accessor chart-text)))

(defclass chart-y-axis-label ()
  ((y      :initarg :y      :accessor chart-y)
   (text   :initarg :text   :accessor chart-text)
   (colour :initarg :colour :accessor chart-colour)
   (size   :initarg :size   :accessor chart-size)
   (rotate :initarg :rotate :accessor chart-rotate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chart-x-legend ()
  ((text  :initarg :text  :accessor chart-text)
   (style :initarg :style :accessor chart-style
          :initform "{font-size: 20px; color: #778877}")))

(defclass chart-y-legend ()
  ((text  :initarg :text  :accessor chart-text)
   (style :initarg :style :accessor chart-style
          :initform "{font-size: 20px; color: #778877}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chart-line ()
  ((type       :initform "line"     :reader chart-type)
   (text       :initarg :text       :accessor chart-text)
   (font-size  :initarg :font-size  :accessor chart-font-size)
   (values     :initarg :values     :accessor chart-values)
   (width      :initarg :width      :accessor chart-width)
   (colour     :initarg :colour     :accessor chart-colour)
   (dot-size   :initarg :dot-size   :accessor chart-dot-size)
   (halo-size  :initarg :halo-size  :accessor chart-halo-size)
   (tip        :initarg :tooltip    :accessor chart-tooltip)
   (on-click   :initarg :on-click   :accessor chart-on-click)
   (loop       :initarg :loop-p     :accessor chart-loop-p)
   (line-style :initarg :line-style :accessor chart-line-style)
   (dot-style  :initarg :dot-style  :accessor chart-dot-style)
   (axis       :initarg :axis       :accessor chart-axis)
   (on-show    :initarg :on-show    :accessor chart-on-show)))

(defclass chart-line-style ()
  ((style :initform "dash" :reader chart-style)
   (on    :initarg :on     :accessor chart-style-on)
   (off   :initarg :off    :accessor chart-style-off)))

(defclass chart-line-show ()
  ((type    :initarg :type    :accessor chart-show-type)
   (cascade :initarg :cascade :accessor chart-show-cascade)
   (delay   :initarg :delay   :accessor chart-show-delay)))

(defclass chart-dot ()
  ((type      :initform "dot"     :reader chart-type)
   (value     :initarg :value     :accessor chart-value)
   (x         :initarg :x         :accessor chart-x)
   (y         :initarg :y         :accessor chart-y)
   (colour    :initarg :colour    :accessor chart-colour)
   (tip       :initarg :tooltip   :accessor chart-tooltip)
   (dot-size  :initarg :dot-size  :accessor chart-dot-size)
   (halo-size :initarg :halo-size :accessor chart-halo-size)
   (on-click  :initarg :on-click  :accessor chart-on-click)))

(defclass chart-solid-dot (chart-dot)
  ((type :initform "solid-dot" :reader chart-type)))

(defclass chart-hollow-dot (chart-dot)
  ((type :initform "hollow-dot" :reader chart-type)))

(defclass chart-star-dot (chart-dot)
  ((type     :initform "star"   :reader chart-type)
   (rotation :initarg :rotation :accessor chart-rotation)
   (hollow   :initarg :hollow-p :accessor chart-hollow-p)))

(defclass chart-bow-dot (chart-dot)
  ((type     :initform "bow"    :reader chart-type)
   (rotation :initarg :rotation :accessor chart-rotation)))

(defclass chart-anchor-dot (chart-dot)
  ((type     :initform "anchor" :reader chart-type)
   (rotation :initarg :rotation :accessor chart-rotation)
   (sides    :initarg :sides    :accessor chart-sides)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chart-bar ()
  ((type      :initform "bar"     :reader chart-type)
   (text      :initarg :text      :accessor chart-text)
   (font-size :initarg :font-size :accessor chart-font-size)
   (values    :initarg :values    :accessor chart-values)
   (colour    :initarg :colour    :accessor chart-colour)
   (alpha     :initarg :alpha     :accessor chart-alpha)
   (tip       :initarg :tooltip   :accessor chart-tooltip)
   (axis      :initarg :axis      :accessor chart-axis)
   (on-show   :initarg :on-show   :accessor chart-on-show)
   (on-click  :initarg :on-click  :accessor chart-on-click)))

(defclass chart-bar-show ()
  ((type    :initarg :type    :accessor chart-show-type)
   (cascade :initarg :cascade :accessor chart-show-cascade)
   (delay   :initarg :delay   :accessor chart-show-delay)))

(defclass chart-bar-value ()
  ((top    :initarg :top     :accessor chart-value-top)
   (bottom :initarg :bottom  :accessor chart-value-bottom)
   (colour :initarg :colour  :accessor chart-value-colour)
   (tip    :initarg :tooltip :accessor chart-value-tooltip)))

(defclass chart-bar-filled-value (chart-bar-value)
  ((outline-colour :initarg :outline-colour :accessor chart-value-outline)))

(defclass chart-bar-stack-value ()
  ((val    :initarg :val     :accessor chart-value-val)
   (colour :initarg :colour  :accessor chart-value-colour)
   (tip    :initarg :tooltip :accessor chart-value-tooltip)))

(defclass chart-bar-stack-key ()
  ((colour    :initarg :colour    :accessor chart-colour)
   (text      :initarg :text      :accessor chart-text)
   (font-size :initarg :font-size :accessor chart-font-size)))


(defclass chart-bar-glass (chart-bar)
  ((type :initform "bar_glass" :reader chart-type)))

(defclass chart-bar-cylinder (chart-bar)
  ((type :initform "bar_cylinder" :reader chart-type)))

(defclass chart-bar-cylinder-outline (chart-bar)
  ((type :initform "bar_cylinder_outline" :reader chart-type)))

(defclass chart-bar-round-glass (chart-bar)
  ((type :initform "bar_round_glass" :reader chart-type)))

(defclass chart-bar-round (chart-bar)
  ((type :initform "bar_round" :reader chart-type)))

(defclass chart-bar-dome (chart-bar)
  ((type :initform "bar_dome" :reader chart-type)))

(defclass chart-bar-round-3d (chart-bar)
  ((type :initform "bar_round3d" :reader chart-type)))

(defclass chart-bar-3d (chart-bar)
  ((type :initform "bar_3d" :reader chart-type)))

(defclass chart-bar-filled (chart-bar)
  ((type           :initform "bar_filled"   :reader chart-type)
   (outline-colour :initarg :outline-colour :accessor chart-outline)))

(defclass chart-bar-stack (chart-bar)
  ((type    :initform "bar_stack" :reader chart-type)
   (colours :initarg :colours     :accessor chart-colours)
   (keys    :initarg :keys        :accessor chart-keys)))

(defclass chart-bar-sketch (chart-bar)
  ((type           :initform "bar_sketch"   :reader chart-type)
   (outline-colour :initarg :outline-colour :accessor chart-outline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chart-hbar ()
  ((type      :initform "hbar"    :reader chart-type)
   (values    :initarg :values    :accessor chart-values)
   (colour    :initarg :colour    :accessor chart-colour)
   (text      :initarg :text      :accessor chart-text)
   (font-size :initarg :font-size :accessor chart-font-size)
   (tip       :initarg :tooltip   :accessor chart-tooltip)))

(defclass chart-hbar-value ()
  ((left   :initarg :left    :accessor chart-value-left)
   (right  :initarg :right   :accessor chart-value-right)
   (colour :initarg :colour  :accessor chart-value-colour)
   (tip    :initarg :tooltip :accessor chart-value-tooltip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ofc-lisp-to-json-name (string)
  (loop with i = 0 and l = (length string)
        with cc-string = (make-string l) and cc-i = 0
        with init = t and cap = nil and all-caps = nil
        while (< i l)
        do (let ((c (aref string i)))
             (unless (case c
                       (#\* (if init (setq cap t)))
                       (#\+ (cond
                              (all-caps (setq all-caps nil init t))
                              (init (setq all-caps t)))))
               (setf (aref cc-string cc-i)
                     (if (and (or cap all-caps) (alpha-char-p c))
                       (char-upcase c)
                       (char-downcase c)))
               (incf cc-i)
               (setq cap nil init nil))
             (incf i))
        finally (return (subseq cc-string 0 cc-i))))
