(ql:quickload '(:zpng 
                :png-read
                :cl-ppcre
                :transducers))

(asdf:defsystem :bianet-mesh
  :description "Build neural networks using bianet-neuron nodes."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:bianet-neuron
               :cl-ppcre
               :zpng
               :dc-eclectic
               :sb-concurrency 
               :transducers)
  :serial t
  :components ((:file "bianet-mesh-package")
               (:file "bianet-mesh")
               (:file "files")
               (:file "pngs")
               (:file "labels")))
