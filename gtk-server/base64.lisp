(in-package :gtk-server)

(defparameter +base64-string+ (load-time-value
                               (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" 'vector)))

(defun base64-encode (bytes-to-encode)
  "Encode a byte array in the base64 format"

  (let ((result nil)
        (vector-length (length bytes-to-encode)))
    (do ((current-index 0))
        ((>= current-index vector-length))
      (let* ((remaining-bytes (- vector-length current-index))
             (byte1 (aref bytes-to-encode current-index))
             (byte2 (cond ((>= remaining-bytes 2) (aref bytes-to-encode (1+ current-index))) (t 0)))
             (byte3 (cond ((>= remaining-bytes 3) (aref bytes-to-encode (+ 2 current-index))) (t 0)))
             (base1 (ash byte1 -2))
             (base2 (+ (ash (logand byte1 3) 4) (ash (logand byte2 240) -4)))
             (base3 (+ (ash (logand byte2 15) 2) (ash (logand byte3 192) -6)))
             (base4 (logand byte3 63)))
      
        (push (aref +base64-string+ base1) result)
        (push (aref +base64-string+ base2) result)
        (push (cond
                ((>= remaining-bytes 2) (aref +base64-string+ base3))
                (t #\=)) result)
        (push (cond
                ((>= remaining-bytes 3) (aref +base64-string+ base4))
                (t #\=)) result)
        (when (< remaining-bytes 3)
          (return))
        (incf current-index 3)))
    (map 'string (lambda (x) x) (reverse result))))

#|
(defun base64-random-test ()
  "Test the base64 encoding implementation comparing with the one
in the s-base64 package (the s-base64 package is not needed for normal operation."
  (labels ((make-random-array ()
             (coerce (loop repeat (random 1000)
                           collecting (random 255)) 'vector)))
    (do ((iteration-count 0 (1+ iteration-count)))
        ((>= iteration-count 10000))
      (let* ((vector-to-encode (make-random-array))
             (my-encoding (base64-encode vector-to-encode))
             (s-base64-encoding (with-output-to-string (out)
                                 (s-base64:encode-base64-bytes vector-to-encode out nil))))
        (when (= 0 (mod iteration-count 1000))
          (format t "."))
        (when (string/= my-encoding s-base64-encoding)
          (cerror "Test Failed"
                  (format nil "~a s-base64:~a" my-encoding
                          s-base64-encoding)))))))

|#