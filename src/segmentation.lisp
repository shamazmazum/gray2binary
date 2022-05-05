(in-package :gray2binary)

(deftype unsigned-octet () '(unsigned-byte 8))

(sera:-> quick-select
         ((simple-array unsigned-octet (*))
          function
          alex:non-negative-fixnum)
         (values unsigned-octet &optional))
(defun quick-select (array predicate k)
  "Return K-th value of ARRAY sorted by PREDICATE."
  (declare (optimize (speed 3))
           (type (simple-array unsigned-octet (*)) array)
           (type function predicate)
           (type alex:non-negative-fixnum k))
  (labels ((select% (array n)
             (declare (type (simple-array (unsigned-byte 8)) array)
                      (type alex:non-negative-fixnum n))
             (let* ((pivot (aref array (random (length array))))
                    (pn   (count  pivot array))
                    (rest (remove pivot array)))
               (flet ((predicate% (x)
                        (funcall predicate x pivot)))
                 (let* ((set (remove-if-not #'predicate% rest))
                        (m (+ n (length set))))
                   (cond
                     ((<= m k (+ m (1- pn)))
                       pivot)
                     ((< k m)
                      (select% set n))
                     (t
                      (select%
                       (remove-if #'predicate% rest)
                       (+ pn m)))))))))
    (select% array 0)))

(sera:-> threshold-levels
         (imago:grayscale-image
          (real 0 1))
         (values unsigned-octet unsigned-octet &optional))
(defun threshold-levels (image q)
  "Return pixel intensities which correspond to Q-th and (1-Q)-th
quantiles of the image."
  (declare (type imago:grayscale-image image)
           (type (real 0 1) q))
  (let ((intensities (map '(vector unsigned-octet)
                          #'imago:gray-intensity
                          (aops:flatten
                           (imago:image-pixels image)))))
    (values
     (quick-select intensities #'<
                   (floor (* q (length intensities))))
     (quick-select intensities #'>
                   (floor (* q (length intensities)))))))

(sera:-> gray->binary
         (imago:grayscale-image
          unsigned-octet
          unsigned-octet)
         (values imago:binary-image &optional))
(defun gray->binary (image low-threshold high-threshold)
  "Convert grayscale image to binary image using watershed
algorithm. All pixels below LOW-THRESHOLD are black and all pixels
above HIGH-THRESHOLD are white."
  (declare (type imago:grayscale-image image)
           (type unsigned-octet low-threshold high-threshold)
           (optimize (speed 3)))
  (let* ((pixels (imago:image-pixels image))
         (gray (aops:vectorize* 'single-float
                   (pixels)
                 (/ (imago:gray-intensity pixels) 255.0)))
         (seeds (aops:vectorize* 'fixnum
                    (pixels)
                  (let ((intensity (imago:gray-intensity pixels)))
                    (cond
                      ;; Definitely void
                      ((< intensity low-threshold) 1)
                      ;; Definitely solid
                      ((> intensity high-threshold) 2)
                      ;; Need to assign a label
                      (t 0)))))
         (segments (cl-watershed:watershed gray seeds)))
    (declare (type (simple-array imago:grayscale-pixel (* *)) pixels))
    (make-instance 'imago:binary-image
                   :pixels (aops:vectorize* 'bit
                               (segments)
                             ;; Void becomes 0 and solid 1
                             (1- segments)))))
