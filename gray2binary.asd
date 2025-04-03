(defsystem :gray2binary
  :name :gray2binary
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Segmentation of grayscale images of two-phase porous media"
  :license "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "segmentation"))
  :depends-on (:imago/pngload
               :imago/jpeg-turbo
               :array-operations
               :serapeum
               :alexandria
               :cl-watershed))

(defsystem :gray2binary/app
  :name :gray2binary/app
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "app/"
  :serial t
  :components ((:file "package")
               (:file "app"))
  :depends-on (:gray2binary
               :imago
               :command-line-parse)
  :build-operation program-op
  :build-pathname "gray2binary"
  :entry-point "gray2binary/app:main")
