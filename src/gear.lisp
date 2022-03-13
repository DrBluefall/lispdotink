(defpackage :co.prismarine.splatdotink.gear
  (:nicknames :splatdotink.gear :sdi.gear)
  (:use :cl)
  (:export :gear-item :gear-kind))
(in-package :co.prismarine.splatdotink.gear)

(deftype gear-kind () '(member :clothes :shoes :head))

(defclass gear-item ()
  ((name
    :type string
    :initarg :name
    :initform (error "Must specify :name. STOP."))
   (end-time
    :type integer
    :initarg :end-time
    :initform (error "Must specify :end-time. STOP.")
    :documentation "A unix timestamp for when this gear will no longer be available.")
   (kind
    :type gear-kind
    :initarg :kind
    :initform (error "Must specify :kind. STOP.")
    :documentation "The type of this gear. Can be one of :CLOTHES, :SHOES, or :HEAD.")
   (price
    :type (cons integer integer)
    :initarg :price
    :initform (error "Must specify :price. STOP.")
    :documentation "The prices of this gear in its original form and on the SplatNet 2 Shop respectively.")
   (ability
    :type (cons (cons integer string) (cons integer string))
    :initarg :ability
    :initform (error "Must specify :ability. STOP.")
    :documentation "The abilities this gear comes with. The CAR is the original ability, and the CDR is the ability currently available on the SplatNet 2 Shop.")
   (rarity
    :type (integer 0 2)
    :initarg :rarity
    :initform (error "Must specify :rarity. STOP.")
    :documentation "The rarity of this gear. This coresponds to the number of slots this gear item comes with, minus one.")
   (id
    :type integer
    :initarg :id
    :initform (error "Must specify :id. STOP.")
    :documentation "The SplatNet 2 ID for this gear item.")
   (brand
    :type (cons integer string)
    :initarg :brand
    :initform (error "Must specify :brand. STOP.")
    :documentation "The brand for this gear item.")))

(defun make-gear-item-from-hash (table)
  (let ((original-gear (gethash "original_gear" table))
        (item (gethash "gear" table)))
    (make-instance 'gear-item
                   :end-time (gethash "end_time" table)
                   :kind (read-from-string (concatenate 'string ":" (gethash "kind" table)))
                   :price (cons (gethash "price" table) (gethash "price" original-gear))
                   :ability (let ((ability (gethash "skill" table))
                                  (original (gethash "skill" original-gear)))
                              (cons
                               (cons (parse-integer (gethash "id" original)) (gethash "name" original))
                               (cons (parse-integer (gethash "id" ability)) (gethash "name" ability))))
                   :name (gethash "name" item)
                   :rarity (gethash "rarity" item)
                   :id (parse-integer (gethash "id" item))
                   :brand (let ((brand (gethash "brand" item)))
                            (cons (parse-integer (gethash "id" brand)) (gethash "name" brand))))))
