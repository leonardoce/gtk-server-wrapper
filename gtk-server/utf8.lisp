(in-package :gtk-server)

(define-condition utf8-error (condition)
  ((string :initarg :string :reader utf8-error-string)
   (message :initarg :message :reader utf8-error-message)))

(defun signal-utf8-error (message string)
  "Signal an utf8 error"
  (let ((my-cond (make-condition 'utf8-error :string string :message message)))
    (error my-cond)))

(defun convert-utf8-to-latin1 (string)
  "Convert a utf8 string to a latin1 string. Thanks to Pierre R. Mai from comp.lang.lisp"
  (declare (simple-string string) (optimize (speed 3)))
  (with-output-to-string (stream)
    (let ((length (length string))
          (index 0))
      (declare (fixnum length index))
      (loop
        (unless (< index length) (return nil))
        (let* ((char (char string index))
               (code (char-code char)))
          (cond
            ((< code #x80) ; ASCII
             (write-char char stream)
             (incf index 1))
            ((< code #xC0)
             ;; We are in the middle of a multi-byte sequence!
             ;; This should never happen, so we raise an error.
             (signal-utf8-error "Encountered illegal multi-byte sequence." string))
            ((< code #xC4)
             ;; Two byte sequence in Latin-1 range
             (unless (< (1+ index) length)
               (signal-utf8-error "Encountered incomplete two-byte sequence." string))
             (let* ((char2 (char string (1+ index)))
                    (code2 (char-code char2)))
               (unless (and (logbitp 7 code2) (not (logbitp 6 code2)))
                 (signal-utf8-error "Second byte in sequence is not a continuation." string))
               (let* ((upper-bits (ldb (byte 2 0) code))
                      (lower-bits (ldb (byte 6 0) code2))
                      (new-code (dpb upper-bits (byte 2 6) lower-bits)))
                 (write-char (code-char new-code) stream)))
             (incf index 2))
            ((>= code #xFE)
             ;; Ignore stray byte-order markers
             (incf index 1))
            (t
             (signal-utf8-error "Multi-byte sequence outside Latin-1 range." string))))))))


(defun utf8-encode (string)
  "Encode in utf8 a string"
  (coerce (mapcar #'code-char
                  (loop for char across string
                        appending (translate-char-utf-8 (char-code char)))) 'string))

(defun translate-char-utf-8 (char-code)
  "Converts the character denoted by the character code CHAR-CODE
into a list of up to six octets which correspond to its UTF-8
encoding.
Comes from Flexy Streams by Edi Weitz - http://www.weitz.de/flexi-streams/"
  (let* (result
         (count
          (cond ((< char-code #x80) (push char-code result) nil)
                ((< char-code #x800) 1)
                ((< char-code #x10000) 2)
                ((< char-code #x200000) 3)
                ((< char-code #x4000000) 4)
                (t 5))))
    (when count
      (loop for rest = char-code then (ash rest -6)
            repeat count
            do (push (logior #b10000000
                             (logand #b111111 rest))
                     result)
            finally (push (logior (logand #b11111111
                                          (ash #b1111110 (- 6 count)))
                                  rest)
                          result)))
    result))

