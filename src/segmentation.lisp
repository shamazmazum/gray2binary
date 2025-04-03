(in-package :gray2binary)

(deftype unsigned-octet () '(unsigned-byte 8))

(sera:-> gray->binary
         (imago:grayscale-image unsigned-octet unsigned-octet)
         (values imago:binary-image &optional))
(defun gray->binary (image low-threshold high-threshold)
  "Convert grayscale image to binary image using watershed
algorithm. All pixels below LOW-THRESHOLD are black and all pixels
above HIGH-THRESHOLD are white."
  (declare (optimize (speed 3)))
  (let* ((pixels (imago:image-pixels image))
         (gray (aops:vectorize* 'alex:non-negative-fixnum
                   (pixels)
                 (imago:gray-intensity pixels)))
         (seeds (aops:vectorize* 'alex:non-negative-fixnum
                    (gray)
                  (cond
                    ;; Definitely void
                    ((< gray low-threshold) 1)
                    ;; Definitely solid
                    ((> gray high-threshold) 2)
                    ;; Need to assign a label
                    (t 0))))
         (segments (cl-watershed:watershed gray seeds)))
    (declare (type (simple-array imago:grayscale-pixel (* *)) pixels))
    (make-instance 'imago:binary-image
                   :pixels (aops:vectorize* 'bit
                               (segments)
                             ;; Void becomes 0 and solid 1
                             (if (= segments 2) 1 0)))))
