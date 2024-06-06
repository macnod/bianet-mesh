(in-package :cl-user)

(require :bianet-neuron)
(require :bianet-mesh)
(require :dc-eclectic)
(require :prove)

(defpackage :bianet-mesh-tests
  (:use :cl :prove :dc-eclectic :bianet-neuron :bianet-mesh))

(setf prove:*enable-colors* t)

;; (subtest "Learn mnist 0/1"
;;   (with-simple-network (neurons 
;;                         input-layer
;;                         hidden-layers
;;                         output-layer
;;                         "01"
;;                         '(784 32 2))
;;     (enable neurons)
;;     (let* ((project-path "/home/macnod/data/mnist-0-1")
;;            (train-path (join-paths project-path "train"))
;;            (test-path (join-paths project-path "test"))
;;            (image-width 28)
;;            (image-height 28)
;;            (output-labels (make-instance 'output-labels 
;;                                          :data-set-path train-path))
;;            (transformation (lambda (f)
;;                              (png-to-inputs f image-width image-height)))
;;            (training-set (create-sample-set
;;                           train-path
;;                           (label-outputs output-labels)
;;                           transformation))
;;            (test-set (create-sample-set 
;;                       test-path
;;                       (label-outputs output-labels)
;;                       transformation))
;;            (topology (create-topology '(32) training-set)))
;;       t)))
                         
