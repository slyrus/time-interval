
(in-package :time-interval)

(defparameter *t1* (local-time:now))

(defparameter *t2* (make-timestamp* :year 2012 :month 4 :day 1))

(defparameter *i1* (time-interval :weeks 4 :days -365))

(defparameter *i2*
  (make-instance 'time-interval :hours -1 :days 2))

(t+ *t1* *i1*)
(t+ *t1* *i2*)
(t+ *t2* *i1*)
(t+ *t2* *i2*)

(t- (time-interval :weeks 4 :days -365)
    (time-interval :years 2 :days 43))
