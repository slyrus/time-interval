
(in-package :time-interval)

(defparameter *t1*
  (universal-time->explicit-time (get-universal-time)))

(defparameter *t2*
  (make-instance 'explicit-time
                 :year 2008
                 :month 5
                 :date 18))

(defparameter *i1*
  (make-instance 'time-interval
                 :weeks 4
                 :days -365))

(defparameter *i2*
  (make-instance 'time-interval :hours -1 :days 2))

(explicit-time->universal-time *t1*)

(explicit-time+time-interval *t1* *i1*)

(explicit-time+time-interval *t1*
                             (parse-time-interval-string "2Y 3M"))

