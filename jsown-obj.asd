(defsystem "jsown-obj"
  :version "0.1.0"
  :author "Joel Reymont"
  :license "LGPL"
  :depends-on ("jsown")
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "wrappers"
                  :depends-on ("packages"))
                 )))
  :description "Statically-generated wrappers for JSON objects"
  )

