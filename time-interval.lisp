;;; file: time-interval.lisp
;;;
;;; Copyright (c) 2008-2012 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(defclass time-interval ()
  ((years :accessor interval-years :initarg :years :initform 0)
   (months :accessor interval-months :initarg :months :initform 0)
   (weeks :accessor interval-weeks :initarg :weeks :initform 0)
   (days :accessor interval-days :initarg :days :initform 0)
   (hours :accessor interval-hours :initarg :hours :initform 0)
   (minutes :accessor interval-minutes :initarg :minutes :initform 0)
   (seconds :accessor interval-seconds :initarg :seconds :initform 0)
   (nanoseconds :accessor interval-nanoseconds :initarg :nanoseconds :initform 0)))

(defmethod print-object ((object time-interval) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~DY ~DMO ~DW ~DD ~DH ~DMI ~DS ~DNS" 
            (interval-years object)
            (interval-months object)
            (interval-weeks object)
            (interval-days object)
            (interval-hours object)
            (interval-minutes object)
            (interval-seconds object)
            (interval-nanoseconds object))))

(defun copy-time-interval (orig)
  (make-instance 'time-interval
                 :years (interval-years orig)
                 :months (interval-months orig)
                 :weeks (interval-weeks orig)
                 :days (interval-days orig)
                 :hours (interval-hours orig)
                 :minutes (interval-minutes orig)
                 :seconds (interval-seconds orig)
                 :nanoseconds (interval-nanoseconds orig)))

(defmacro or-zero (value)
  (let ((result (gensym)))
    `(let ((,result ,value))
       (if ,result ,result 0))))

(defun time-interval (&rest args)
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
          ("MI" :minutes)
          ("S" :seconds)
          ("NS" :nanoseconds)))

(defun parse-time-interval-string (string)
  (let ((regs (cl-ppcre:split "([ymwdhsnoiYMWDHSNOI\s]+)" string :with-registers-p t)))
    (let ((keys (loop for (num unit) on regs by #'cddr
                   nconc
                     (let ((key (gethash (string-upcase unit) *unit-hash*))
                           (num (parse-integer num)))
                       (unless (and key num)
                         (error "bad time-interval-string!"))
                       (list key num)))))
      (when keys
        (apply #'make-instance 'time-interval keys)))))

(defun timestamp-add-interval (time interval)
  (with-accessors ((years interval-years)
                   (months interval-months)
                   (weeks interval-weeks)
                   (days interval-days)
                   (hours interval-hours)
                   (minutes interval-minutes)
                   (seconds interval-seconds)
                   (nanoseconds interval-nanoseconds))
      interval
    (reduce (lambda (time increment)
              (destructuring-bind (amount unit)
                  increment
                (if amount
                    (local-time:timestamp+ time amount unit)
                    time)))
            `((,years :year)
              (,months :month)
              (,(* 7 (or-zero weeks)) :day)
              (,days :day)
              (,hours :hour)
              (,minutes :minute)
              (,seconds :sec)
              (,nanoseconds :nsec))
            :initial-value time)))

(defun negate-time-interval (orig)
  (make-instance 'time-interval
                 :years (- (interval-years orig))
                 :months (- (interval-months orig))
                 :weeks (- (interval-weeks orig))
                 :days (- (interval-days orig))
                 :hours (- (interval-hours orig))
                 :minutes (- (interval-minutes orig))
                 :seconds (- (interval-seconds orig))
                 :nanoseconds (- (interval-nanoseconds orig))))

(defun make-timestamp* (&key year month day hour minute second nanosecond)
  (local-time:encode-timestamp
   (or-zero nanosecond)
   (or-zero second)
   (or-zero minute)
   (or-zero hour)
   (or-zero day)
   (or-zero month)
   (or-zero year)))

(defgeneric t+ (time-or-interval-1 time-or-interval-2))

(defmethod t+ ((t1 local-time:timestamp) (t2 time-interval))
  (timestamp-add-interval t1 t2))

(defmethod t+ ((t1 time-interval) (t2 local-time:timestamp))
  (timestamp-add-interval t2 t1))

(defmethod t+ ((t1 time-interval) (t2 time-interval))
  (make-instance 'time-interval
                 :years (+ (interval-years t1) (interval-years t2))
                 :months (+ (interval-months t1) (interval-months t2))
                 :weeks (+ (interval-weeks t1) (interval-weeks t2))
                 :days (+ (interval-days t1) (interval-days t2))
                 :hours (+ (interval-hours t1) (interval-hours t2))
                 :minutes (+ (interval-minutes t1) (interval-minutes t2))
                 :seconds (+ (interval-seconds t1) (interval-seconds t2))
                 :nanoseconds (+ (interval-nanoseconds t1) (interval-nanoseconds t2))))

(defgeneric t- (time-or-interval-1 time-or-interval-2))

(defmethod t- ((t1 local-time:timestamp) (t2 time-interval))
  (timestamp-add-interval t1 (negate-time-interval t2)))

(defmethod t- ((t1 time-interval) (t2 time-interval))
  (make-instance 'time-interval
                 :years (- (interval-years t1) (interval-years t2))
                 :months (- (interval-months t1) (interval-months t2))
                 :weeks (- (interval-weeks t1) (interval-weeks t2))
                 :days (- (interval-days t1) (interval-days t2))
                 :hours (- (interval-hours t1) (interval-hours t2))
                 :minutes (- (interval-minutes t1) (interval-minutes t2))
                 :seconds (- (interval-seconds t1) (interval-seconds t2))
                 :nanoseconds (- (interval-nanoseconds t1) (interval-nanoseconds t2))))

(defun timestamp-decoded-difference (tend tstart)
  "Computes the difference between the individual components of
local-time:timestamp t1 and t2. Returns 8 values: dyear dmonth dday
dhh dmm dss dns dday-of-week where dyear is the difference in years,
etc..."
  (destructuring-bind (dns dss dmm dhh dday dmonth dyear dday-of-week)
      (mapcar #'-
              (subseq (multiple-value-list
                       (local-time:decode-timestamp tend))
                      0 8)
              (subseq (multiple-value-list
                       (local-time:decode-timestamp tstart))
                      0 8))
    (values dyear dmonth dday dhh dmm dss dns dday-of-week)))

(defmethod t- ((t1 local-time:timestamp) (t2 local-time:timestamp))
  (multiple-value-bind (y mo d h m s ns)
      (timestamp-decoded-difference t1 t2)
    (time-interval :years y :months mo :days d :hours h :minutes m :seconds s :nanoseconds ns)))
