(defpackage :co.prismarine.splatdotink
  (:nicknames :splatdotink :sdi)
  (:use :cl)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export
   :pvp-schedules
   :salmon-schedule
   :merchandise))
(in-package :co.prismarine.splatdotink)

(defun pvp-schedules ()
  "Return a plist of sdi.rotation:rotation with the league, ranked, and Turf War map and mode rotations."

  (let* ((schedules (jzon:parse
                     (drakma:http-request "http://splatoon2.ink/data/schedules.json")))
         (turf (gethash "regular" schedules))
         (ranked (gethash "gachi" schedules))
         (league (gethash "league" schedules)))
    (loop for t1 across turf
          for t2 across ranked
          for t3 across league
          collect (sdi.rotation::make-rotation-from-hash t1) into turflist
          collect (sdi.rotation::make-rotation-from-hash t2) into rankedlist
          collect (sdi.rotation::make-rotation-from-hash t3) into leaguelist
          finally (return (list :turf turflist :ranked rankedlist :league leaguelist)))))

(defun salmon-schedule ()
  "Return a list of sdi.rotation:salmon-rotation."

  (let ((response (jzon:parse
                   (drakma:http-request "http://splatoon2.ink/data/coop-schedules.json"))))
    (loop for schedule across (gethash "schedules" response)
          for detail-list = (coerce (gethash "details" response) 'list) then (cdr detail-list)
          for detail = (car detail-list)
          collect (sdi.rotation::make-salmon-rotation-from-hash (or detail schedule)))))

(defun merchandise ()
  "Return a list with the current gear available on the SplatNet 2 Shop."

  (let ((merch
          (gethash "merchandises"
                   (jzon:parse
                    (drakma:http-request "http://splatoon2.ink/data/merchandises.json")))))
    (loop for gear across merch
          collect (sdi.gear::make-gear-item-from-hash gear))))
