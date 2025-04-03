(in-package :gray2binary/app)

(defun parse-threshold (string)
  (let ((x (parse-integer string)))
    (unless (<= 0 x 255)
      (error 'cmd-line-parse-error
             :format-control "Threshold must be in range 0-255"))
    x))

(defun check-output-type (output-name)
  (let ((type (pathname-type (pathname output-name))))
    (unless (or (string= "pnm" type)
                (string= "pbm" type))
      (error 'cmd-line-parse-error
             :format-control
             "Only portable bitmap (.pbm or .pnm) is supported as an output format")))
  output-name)

(defparameter *parser*
  (seq
   (argument :input  "INPUT"
             :description "Input picture")
   (argument :output "OUTPUT"
             :description "Output picture"
             :fn          #'check-output-type)
   (argument :low    "LOW"
             :description "Threshold for the black phase"
             :fn          #'parse-threshold)
   (argument :high   "HIGH"
             :description "Threshold for the white phase"
             :fn          #'parse-threshold)))

(defun print-error (c &optional (stream *error-output*))
  (princ c stream)
  (terpri stream))

(defun main ()
  (handler-case
      (let* ((args (parse-argv *parser*))
             (image (imago:convert-to-grayscale
                     (imago:read-image (%assoc :input args)))))
        (imago:write-image
         (gray2binary:gray->binary
          image
          (%assoc :low  args)
          (%assoc :high args))
         (%assoc :output args)))
    (cmd-line-parse-error (c)
      (print-error c)
      (print-usage *parser* "gray2binary")
      (uiop:quit 1))
    (error (c)
      (print-error c)
      (uiop:quit 1)))
  (uiop:quit 0))
