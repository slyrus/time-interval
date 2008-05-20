;;; file: time-interval.lisp
;;;
;;; Copyright (c) 2008 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :time-interval)

(defclass explicit-time ()
  (;; universal time slots
   (year :accessor time-year :initarg :year :initform 1900)
   (month :accessor time-month :initarg :month :initform 1)
   (date :accessor time-date :initarg :date :initform 1)
   (hours :accessor time-hours :initarg :hours :initform 0)
   (minutes :accessor time-minutes :initarg :minutes :initform 0)
   (seconds :accessor time-seconds :initarg :seconds :initform 0)
   (daylight-p :accessor time-daylight-p :initarg :daylight-p :initform nil)
   (tz :accessor time-tz :initarg :tz :initform nil)))

(defmethod print-object ((object explicit-time) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~4D.~2,'0D.~2,'0D ~2,'0D:~2,'0D:~2,'0D" 
            (time-year object)
            (time-month object)
            (time-date object)
            (time-hours object)
            (time-minutes object)
            (time-seconds object))))

(defun copy-explicit-time (orig)
  (make-instance 'explicit-time
                 :year (time-year orig)
                 :month (time-month orig)
                 :date (time-date orig)
                 :hours (time-hours orig)
                 :minutes (time-minutes orig)
                 :seconds (time-seconds orig)
                 :daylight-p (time-daylight-p orig)
                 :tz (time-tz orig)))

(defclass time-interval ()
  ((years :accessor interval-years :initarg :years :initform 0)
   (months :accessor interval-months :initarg :months :initform 0)
   (weeks :accessor interval-weeks :initarg :weeks :initform 0)
   (days :accessor interval-days :initarg :days :initform 0)
   (hours :accessor interval-hours :initarg :hours :initform 0)
   (minutes :accessor interval-minutes :initarg :minutes :initform 0)
   (seconds :accessor interval-seconds :initarg :seconds :initform 0)))

(defmethod print-object ((object time-interval) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~DY ~DM ~DW ~DD ~DH ~DM ~DS" 
            (interval-years object)
            (interval-months object)
            (interval-weeks object)
            (interval-days object)
            (interval-hours object)
            (interval-minutes object)
            (interval-seconds object))))

(defun copy-time-interval (orig)
  (make-instance 'time-interval
                 :years (interval-years orig)
                 :months (interval-months orig)
                 :weeks (interval-weeks orig)
                 :days (interval-days orig)
                 :hours (interval-hours orig)
                 :minutes (interval-minutes orig)
                 :seconds (interval-seconds orig)))

(defmacro or-zero (value)
  (let ((result (gensym)))
    `(let ((,result ,value))
       (if ,result ,result 0))))

(defun decoded-time->explicit-time (second minute hour date month
                                    year day daylight-p &optional time-zone)
  (declare (ignore day))
  (apply #'make-instance
         'explicit-time
         :seconds second
         :minutes minute
         :hours hour
         :date date
         :month month
         :year year
         :daylight-p daylight-p
         (when time-zone
           `(:tz ,time-zone))))

(defun universal-time->explicit-time (utime)
  (multiple-value-call #'decoded-time->explicit-time (decode-universal-time utime)))

(defun explicit-time->universal-time (etime)
  (apply #'encode-universal-time
         (or-zero (time-seconds etime))
         (or-zero (time-minutes etime))
         (or-zero (time-hours etime))
         (or-zero (time-date etime))
         (or-zero (time-month etime))
         (or-zero (time-year etime))
         (when (time-tz etime)
           (list (time-tz etime)))))

(defun explicit-time->decoded-time (etime)
  (decode-universal-time (explicit-time->universal-time etime)))

(defun explicit-time+time-interval (etime interval)
  (let ((dest (copy-explicit-time etime))
        (interval (copy-time-interval interval)))

    ;; we have a date that may be > the number of days in the
    ;; month (or the year!), so we have to adjust the year and month
    ;; accordingly...
    
    ;; adjust months and years
    (incf (time-month dest) (interval-months interval))
    (incf (time-year dest) (interval-years interval))

    ;;; canonicalize
    (multiple-value-bind (years months)
        (floor (time-month dest) 12)
      (setf (time-month dest) months)
      (incf (time-year dest) years))

    ;; adjust years
    
    ;; now convert the (partial) destination time into a universal
    ;; time and add the number of seconds corresponding to the weeks,
    ;; days, hours, minutes and seconds of the interval.
    (let ((utime (explicit-time->universal-time dest)))
      (incf utime (+ (* 60
                        (+ (* 60
                              (+ (* 24
                                    (+ (* 7 (interval-weeks interval))
                                       (interval-days interval)))
                                 (interval-hours interval)))
                           (interval-minutes interval)))
                     (interval-seconds interval)))
      (universal-time->explicit-time utime))))

(defun universal-time+time-interval (utime interval)
  (explicit-time+time-interval
   (universal-time->explicit-time utime)
   interval))

(defun interval (&rest args)
  (apply #'make-instance 'time-interval args))

(defparameter *unit-hash* (make-hash-table :test 'equal))
(mapcar (lambda (unit)
          (destructuring-bind (abbrev keyword)
              unit
            (setf (gethash abbrev *unit-hash*)
                  keyword)))
        '(("Y" :years)
          ("MO" :months)
          ("W" :weeks)
          ("D" :days)
          ("H" :hours)
          ("M" :minutes)
          ("MI" :minutes)
          ("S" :seconds)))

(defun parse-time-interval-string (string)
  (let ((regs (cl-ppcre:split "([ymwdhsoiYMWDHSOI\s]+)" string :with-registers-p t)))
    (let ((keys (loop for (num unit) on regs by #'cddr
                   nconc
                     (let ((key (gethash (string-upcase unit) *unit-hash*))
                           (num (parse-integer num)))
                       (unless (and key num)
                         (error "bad time-interval-string!"))
                       (list key num)))))
      (when keys
        (apply #'make-instance 'time-interval keys)))))

