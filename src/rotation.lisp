(defpackage :co.prismarine.splatdotink.rotation
  (:nicknames :splatdotink.rotation :sdi.rotation)
  (:use :cl)
  (:export :rule :rotation :salmon-rotation))
(in-package :co.prismarine.splatdotink.rotation)

(deftype rule () '(member :turf-war :splat-zones :rainmaker :tower-control :clam-blitz))

(defparameter +rulemap+ '((:turf-war . "turf_war")
                         (:splat-zones . "splat_zones")
                         (:rainmaker . "rainmaker")
                         (:tower-control . "tower_control")
                         (:clam-blitz . "clam_blitz")))

(declaim (inline rule-from-key))
(defun rule-from-key (key)
  (declare (type string key))
  (car (rassoc key +rulemap+ :test #'string=)))


(defclass rotation ()
  ((start-time
    :type integer
    :initarg :start-time
    :initform (error "Must specify :start-time. STOP.")
    :documentation "A unix timestamp specifying the start time of this rotation.")
   (end-time
    :type integer
    :initarg :end-time
    :initform (error "Must specify :end-time. STOP.")
    :documentation "A unix timestamp specifying the end time of this rotation.")
   (gamerule
    :type rule
    :initarg :rule
    :initform :turf-war
    :documentation "The game mode of the rotation. Can be one of :TURF-WAR, :SPLAT-ZONES, :RAINMAKER, :TOWER-CONTROL, or :CLAM-BLITZ. Defaults to :TURF-WAR.")
   (stage-a
    :type string
    :initarg :stage-a
    :initform (error "Must specify :stage-a. STOP.")
    :documentation "One of the stages available in this rotation.")
   (stage-b
    :type string
    :initarg :stage-b
    :initform (error "Must specify :stage-b. STOP.")
    :documentation "One of the stages available in this rotation.")))

(defun make-rotation-from-hash (table)
  (make-instance 'rotation
                 :start-time (gethash "start_time" table)
                 :end-time (gethash "end_time" table)
                 :stage-a (gethash "name" (gethash "stage_a" table))
                 :stage-b (gethash "name" (gethash "stage_b" table))
                 :rule (rule-from-key (gethash "key" (gethash "rule" table)))))

(defmethod print-object ((object rotation) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (start-time end-time gamerule stage-a stage-b) object
      (format stream ":start-time ~a :end-time ~a :gamerule ~a :stage-a ~a :stage-b ~a"
              start-time
              end-time
              gamerule
              stage-a
              stage-b))))

(defclass salmon-rotation ()
  ((start-time
    :type integer
    :initarg :start-time
    :initform (error "Must specify :start-time. STOP.")
    :documentation "A unix timestamp specifying the start time of this rotation.")
   (end-time
    :type integer
    :initarg :end-time
    :initform (error "Must specify :end-time. STOP.")
    :documentation "A unix timestamp specifying the end time of this rotation.")
   (stage
    :type string
    :initarg :stage
    :initform nil
    :documentation "The scheduled stage for this rotation.")
   (weapons
    :type list
    :initarg :weapons
    :initform nil
    :documentation "The weapons to be supplied for this rotation.")))

(defun make-salmon-rotation-from-hash (table)
  (make-instance 'salmon-rotation
                 :start-time (gethash "start_time" table)
                 :end-time (gethash "end_time" table)
                 :stage (multiple-value-bind (stage exists-p)
                            (gethash "stage" table)
                        (when exists-p
                          (gethash "name" stage)))
                 :weapons (multiple-value-bind (weapons exists-p)
                              (gethash "weapons" table)
                            (when exists-p
                              (loop for weapon across weapons
                                    collect (gethash "name" (gethash "weapon" weapon)))))))

(defmethod print-object ((object salmon-rotation) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (start-time end-time stage weapons) object
      (format stream ":start-time ~a :end-time ~a~@[ :stage ~a~]~@[ :weapons ~{~A~^, ~}~]"
              start-time
              end-time
              stage
              weapons))))
