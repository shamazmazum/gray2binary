(defpackage gray2binary
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:export #:gray->binary
           #:threshold-levels))
