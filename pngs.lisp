(in-package :bianet-mesh)

(defun read-png (filename &key (width 28) (height 28))
  (loop with image-data = (png-read:image-data
                           (png-read:read-png-file filename))
        with dimensions = (length (array-dimensions image-data))
        for y from 0 below height 
        appending (loop for x from 0 below width 
                        collecting (if (= dimensions 2)
                                       (aref image-data x y)
                                       (aref image-data x y 0)))
          into intensity-list
        finally (return (invert-intensity intensity-list))))

(defun invert-intensity (list &key (max 255))
  (loop for element in list collect (- max element)))

(defun png-to-inputs (filename width height)
  (normalize-list
   (read-png filename :width width :height height) :min 0 :max 255))
