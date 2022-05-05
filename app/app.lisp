(in-package :gray2binary-app)

(define-condition gray2binary-error (error)
  ())

(define-condition wrong-output-type (gray2binary-error)
  ((wrong-type :initarg :wrong-type
               :reader  wrong-type))
  (:report (lambda (c s)
             (format s "Only Portable Anymap format is supported for output files, got ~a~%"
                     (wrong-type c)))))

(opts:define-opts
  (:name        :low-threshold
   :description "Pixels below this threshold are labeled as void"
   :short       #\l
   :long        "low"
   :arg-parser  #'parse-integer
   :meta-var    "LOW")
  (:name        :high-threshold
   :description "Pixels above this threshold are labeled as solids"
   :short       #\h
   :long        "high"
   :arg-parser  #'parse-integer
   :meta-var    "HIGH")
  (:name        :quantile
   :description "Q-th and (1-Q)-th quantiles for void and solid thresholds"
   :short       #\q
   :long        "quantile"
   :arg-parser  #'parse-number:parse-number
   :meta-var    "Q"))

(defun print-help-and-quit ()
  (opts:describe :usage-of "gray2binary"
                 :args "INPUT-NAME OUTPUT-NAME")
  (uiop:quit 1))

(defun check-output-type (output-name)
  (declare (type (or string pathname) output-name))
  (let ((type (pathname-type (pathname output-name))))
    (unless (or (string= "pnm" type)
                (string= "pbm" type))
      (error 'wrong-output-type :wrong-type type)))
  (values))

(defun main ()
  (handler-case
      (multiple-value-bind (options arguments)
          (opts:get-opts)
        (unless (= (length arguments) 2)
          (print-help-and-quit))
        (destructuring-bind (input-name output-name)
            arguments
          (check-output-type output-name)
          (let ((image (imago:read-image input-name)))
            (multiple-value-bind (low-threshold high-threshold)
                (gray2binary:threshold-levels
                 image
                 (getf options :quantile 1/6))
              (imago:write-image
               (gray2binary:gray->binary
                image
                (getf options :low-threshold  low-threshold)
                (getf options :high-threshold high-threshold))
               output-name)))))
    ((or opts:troublesome-option gray2binary-error imago:imago-error) (c)
     (princ c *error-output*)
     (terpri  *error-output*)
     (uiop:quit 1)))
  (uiop:quit 0))
