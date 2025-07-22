(in-package :bianet-mesh)

(defparameter *wait-for-output-timeout* 1.0)
(defparameter *wait-for-backprop-timeout* 1.0)

(defmacro with-neurons ((neurons-var neuron-count name)
                        &body body)
  `(let ((,neurons-var
           (loop for a from 1 to ,neuron-count
                 collect
                 (make-instance 't-neuron 
                                :name (format nil "~a-~9,'0d" ,name a)))))
     (unwind-protect
          (progn ,@body)
       (disable ,neurons-var))))

(defun make-simple-network (name layer-counts)
  "Used by the with-simple-network macro to create a standard, multilayer
neural network, where every neuron in one layer is connected to every
neuron in the downstream layer. This includes connections to biased
neurons in the downstream layer, because even though the biased neuron
ignores the incoming value, the incoming connection serves to alert
the biased neuron that it should activate.

This function adds a biased neuron to each layer that is not an input
or output layer. Therefore, if the sum of LAYER-COUNTS is 10, and the
network contains 2 layers, the total number of neurons that this
function will create is 12."
  (loop with cx-count = (loop with layers = (length layer-counts)
                              for layer-count in layer-counts
                              for next-layer-count in (cdr layer-counts)
                              for index from 0 below layers
                              for cx-count = (* layer-count (1+ next-layer-count))
                                then (+ cx-count (* (1+ layer-count) 
                                                    (1+ next-layer-count)))
                              finally (return (1- cx-count)))
        with weights = (loop with step = (/ pi cx-count)
                             for a from 1 to cx-count
                             for b = 0.0 then (+ b step)
                             collect (sin b) into w
                             finally (return (map 'vector 'identity w)))
        for layer-count in layer-counts
        for layer-index = 0 then (1+ layer-index)
        for is-input-layer = (zerop layer-index)
        for is-output-layer = (= (1+ layer-index) (length layer-counts))
        collect (loop with layer-size = (if (or is-input-layer is-output-layer)
                                            layer-count
                                            (1+ layer-count))
                      for a from 1 to layer-size
                      for transfer-key = (if is-output-layer
                                             :logistic
                                             :relu)
                      for biased = (and (not (or is-input-layer is-output-layer))
                                        (= a (1- layer-size)))
                      for neuron = (make-instance 
                                    't-neuron
                                    :name (format nil "~a-~d-~d" 
                                                  name (1+ layer-index) a)
                                    :transfer-key transfer-key
                                    :biased biased
                                    :layer layer-index)
                      collect neuron)
          into layers
        finally (loop with weight-index = 0
                      for layer in (butlast layers)
                      for next-layer in (cdr layers)
                      do (loop for source in layer
                               do (loop for target in next-layer
                                        for weight = (aref weights weight-index)
                                        do (connect source target
                                                    :weight weight
                                                    :learning-rate weight
                                                    :momentum weight)
                                           (incf weight-index))))
                (return (reduce #'append layers))))

(defmacro with-simple-network ((neurons-var
                                input-layer-var
                                hidden-layers-var
                                output-layer-var
                                name 
                                &rest layer-counts) 
                               &body body)
  "Creates a neural network with neurons arranged into layers, then
disables the neurons (to clean up threads) upon exit.

LAYER-COUNTS is a list of integers representing the number of neurons
needed for each layer, starting with the input layer. The length of
the list determines the number of layers in the neural network. This
macro adds a biased neuron to each layer that is not an input or
output layer. Thus, the total number of neurons that this macro
creates when building a network is equal to the sum of the values
in LAYER-COUNTS plus the number of hidden layers.

NAME is a prefix that is used to label neurons and threads. This
prefix should be a short, simple word, like 'alpha'.

NEURONS-VAR contains the name of the variable that will be made
available inside the body of the macro to hold the list of all the
neurons.

INPUT-LAYER-VAR contains the name of the variable that will be made
available inside the body of the macro to hold the list of all the
input-layer neurons.

HIDDEN-LAYERS-VAR contains the name of the variable that will be made
available inside the body of the macro to hold the list of all the
neurons in hidden layers. If there are multiple hidden layers, this
variable will hold neurons from all of those layers.

OUTPUT-LAYER-VAR contains the name of the variable that will be made
available inside the body of the macro to hold the list of all the
neurons in the output layer.

The following example creates a neural network with 8 neurons and
trains it to learn the XOR-gate table:

  (with-simple-network
      (neurons input-layer hidden-layers output-layer \"alpha\" 2 4 1)
    (let ((iterations 1000)
          (training-set #(((0 0) (0))
                          ((0 1) (1))
                          ((1 0) (1))
                          ((1 1) (0))))
      (enable neurons)
      (loop with l = (length training-set)
            for i from 1 to iterations
            for frame (aref training-set (mod i 4))
            do (train-frame input-layer output-layer frame)))))
"
  `(let* ((counts ',layer-counts)
          (,neurons-var (make-simple-network 
                         ,name counts))
          (layer-count (length counts))
          (,input-layer-var (remove-if-not 
                             (lambda (n) (zerop (layer n))) 
                             ,neurons-var))
          (,hidden-layers-var 
            (remove-if
             (lambda (n) 
               (or (zerop (layer n))
                   (= (layer n) (1- layer-count))))
             ,neurons-var))
          (,output-layer-var 
            (remove-if-not
             (lambda (n) 
               (= (layer n) (1- layer-count)))
             ,neurons-var))
          (sum-of-layer-lengths 
            (+ (length ,input-layer-var)
               (length ,hidden-layers-var)
               (length ,output-layer-var)))
          (biased-neuron-count 
            (max (- (length counts) 2) 0)))
     (assert (= sum-of-layer-lengths 
                (+ (reduce '+ counts) biased-neuron-count)))
     (unwind-protect
          (progn ,@body)
       (disable ,neurons-var))))

(defmethod enable ((neurons list))
  (loop for neuron in neurons counting (enable neuron)))

(defmethod enable ((neurons dlist))
  (loop for neuron-node = (head neurons) then (next neuron-node)
        while neuron-node
        counting (enable (value neuron-node))))

(defmethod disable ((neurons list))
  (loop for neuron in neurons 
        collect (disable neuron) into disabled
        finally (sleep 0.1)
                (return disabled)))

(defmethod isolate ((neurons list))
  (loop for neuron in neurons collect (isolate neuron)))

(defmethod excite ((neurons list) (values list))
  (loop for neuron in neurons
        for value in values
        counting (excite neuron value)))

(defmethod excite ((neurons dlist) (values list))
  (loop for neuron-node = (head neurons) then (next neuron-node)
        for value in values
        while neuron-node
        for neuron = (value neuron-node)
        counting (excite neuron value)))

(defmethod list-incoming ((neurons list))
  (loop for neuron in neurons
        append (list-incoming neuron)))

(defmethod list-incoming ((neurons dlist))
  (loop for neuron-node = (head neurons) then (next neuron-node)
        while neuron-node
        append (list-incoming (value neuron-node))))

(defmethod list-outgoing ((neurons list))
  (loop for neuron in neurons
        appending (list-outgoing neuron)))

(defmethod list-outgoing ((neurons dlist))
  (loop for neuron-node = (head neurons) then (next neuron-node)
        while neuron-node
        appending (list-outgoing (value neuron-node))))

(defmethod list-outputs ((neurons list))
  (loop for neuron in neurons collect (output neuron)))

(defmethod wait-for-output ((neurons list)
                            (target-ff-count integer)
                            (timeout-seconds float))
  (loop with start-time = (mark-time)
        while (some (lambda (neuron)
                      (< (ff-count neuron) target-ff-count))
                    neurons)
        when (> (elapsed-time start-time) timeout-seconds)
          do (error "Timed out after ~f seconds" timeout-seconds)
        finally (return (every (lambda (neuron)
                                 (= (ff-count neuron) target-ff-count))
                               neurons))))

(defmethod wait-for-backprop ((neurons list)
                              (target-bp-count integer)
                              (timeout-seconds float))
  (loop with start-time = (mark-time)
        while (some (lambda (neuron)
                      (< (bp-count neuron) target-bp-count))
                    neurons)
        when (> (elapsed-time start-time) timeout-seconds)
          do (error "Timed out after ~f seconds" timeout-seconds)
        finally (return (every (lambda (neuron)
                                 (= (bp-count neuron) target-bp-count))
                               neurons))))

(defmethod wait-for-output-p ((neurons list)
                              (target-ff-count integer)
                              (timeout-seconds float))
  (loop with start-time = (mark-time)
        while (some (lambda (neuron)
                      (< (ff-count neuron) target-ff-count))
                    neurons)
        when (> (elapsed-time start-time) timeout-seconds)
          do (return nil)
        finally (return t)))

(defmethod wait-for-backprop-p ((neurons list)
                                (target-bp-count integer)
                                (timeout-seconds float))
  (loop with start-time = (mark-time)
        while (some (lambda (neuron)
                      (< (bp-count neuron) target-bp-count))
                    neurons)
        when (> (elapsed-time start-time) timeout-seconds)
          do (return nil)
        finally (return t)))

(defmethod train-frame ((input-layer list) (output-layer list) (frame list))
  (let* ((inputs (first frame))
         (expected-outputs (second frame))
         (ff-count (ff-count (first input-layer)))
         (bp-count (bp-count (first input-layer)))
         (excited (or (loop for neuron in input-layer
                            for input in inputs
                            always (excite neuron input))
                      (error "excite failed")))
         (ff-wait (or (wait-for-output-p output-layer (1+ ff-count) 
                        *wait-for-output-timeout*)
                      (error "wait-for-output-p failed")))
         (outputs (mapcar #'output output-layer))
         (errors (mapcar (lambda (e o) (- e o)) expected-outputs outputs))
         (modulated (or (loop for neuron in output-layer
                              for error in errors
                              always (modulate neuron error))
                        (error "modulate failed")))
         (bp-wait (or (wait-for-backprop-p input-layer (1+ bp-count)
                        *wait-for-backprop-timeout*)
                      (error "wait-for-backprop-p failed"))))
    errors))

(defmethod feed-forward ((input-layer list) (output-layer list) (inputs list))
  (let* ((ff-count (ff-count (first input-layer)))
         (excited (loop for neuron in input-layer
                        for input in inputs
                        always (excite neuron input)))
         (ff-wait (when excited
                    (wait-for-output-p output-layer (1+ ff-count)
                      *wait-for-output-timeout*))))
    (when ff-wait (mapcar #'output output-layer))))

(defmethod apply-inputs ((input-layer list) (inputs list))
  (loop for neuron in input-layer
        for value in inputs
        do (setf (input neuron) value)))

(defmethod apply-error ((output-layer list) (expected-outputs list))
  (loop for neuron in output-layer
        for expected in expected-outputs
        for actual = (output neuron)
        for error = (- expected actual)
        do (setf (err-input neuron) error)
        collect error))

(defmethod output-layer-error ((output-layer list) (expected-output list))
  (sqrt 
   (reduce 
    '+ 
    (mapcar 
     (lambda (n e)
       (expt (- e (output n)) 2))
     output-layer
     expected-output))))
