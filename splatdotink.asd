(asdf:defsystem :splatdotink
  :name "splatdotink"
  :description "A Common Lisp library to simplify fetching data from splatoon2.ink."
  :version "0.0.0"
  :author "Alexander Bisono <sbisonol@gmail.com>"
  :licence "LGPL-3.0-or-later"
  :depends-on (:drakma :com.inuoe.jzon)
  :components ((:module "src"
                :serial t
                :components ((:file "gear")
                             (:file "rotation")
                             (:file "lib")))))
