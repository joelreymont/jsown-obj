(defpackage jsown-obj
  (:use :cl)
  (:export :defobject
           :to-json
           :from-json
           :read-map
           :read-array
           :read-string-array
           :read-obj-array
           )
  (:nicknames :jo))
