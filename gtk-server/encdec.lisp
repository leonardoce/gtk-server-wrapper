;;; Encoding and decoding information to/from gtk-server

(in-package :gtk-server)

(defvar *utf8-encode-decode* t "Enable/Disable utf8 encoding/decoding")

;;; From Common Lisp Cookbook
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
			   :start old-pos
			   :end (or pos (length string)))
	  when pos do (write-string replacement out)
	  while pos))) 

(defun is-integer (string)
  "Return true if a string represent an integer"
  (let ((len (second (multiple-value-list (parse-integer string
							 :junk-allowed t)))))
    (= (length string) len)))

(defun gtkserver-decode-none (string)
  "Check none type"
  (when (string/= (string-upcase (string-trim " " string)) "OK")
    (error (format nil "(not none type) Gtk-Server Error: ~a" string)))
  nil)

(defun gtkserver-decode-widget (string)
  "Check widget type"
  (unless (is-integer string)
    (error (format nil "(not widget type) Gtk-Server Error: ~a" string)))
  string)

(defun gtkserver-decode-integer (string)
  "Check integer type"
  (unless (is-integer string)
    (error (format nil "(not integer type) Gtk-Server Error: ~a" string)))
  (parse-integer string))

(defun gtkserver-decode-string (string)
  "Check string-c-encoded type"
  (cond
    ;; Empty string
    ((= (length (string-trim " " string)) 0) "")
    ;; Not empty string
    ((< (length string) 2)
     (error (format nil "(not string type) Gtk-Server Error: ~a" string))) 
    ((and (char= (char string 0) #\")
	  (char= (char string (1- (length string))) #\"))
     (cescape-to-lisp string))
    (t (error (format nil "(not string type) Gtk-Server Error: ~a" string)))))

(defun gtkserver-decode-and-check (return-type string)
  "Decode Gtk-Server output and check it's return type"
  (cond
    ((string= return-type 'none) (gtkserver-decode-none string))
    ((string= return-type 'widget) (gtkserver-decode-widget string))
    ((string= return-type 'string) (gtkserver-decode-string string))
    ((string= return-type 'integer) (gtkserver-decode-integer string))
    (t (error (format nil "Unknown return type ~a" return-type)))))

(defun gtkserver-encode (type string)
  "Encode a string"
  (cond 
    ((equal type 'string) (cond
                            (*utf8-encode-decode* (utf8-encode (lisp-to-cescape string)))
                            (t (lisp-to-cescape string))))
    ((equal type 'integer) string)
    ((equal type 'float) string)
    ((equal type 'boolean) string)
    ((equal type 'base64) (base64-encode string))
    ((equal type 'widget) (gtkserver-encode-widget string))
    (t (error (format nil "Unknown type ~A" type)))))

(defun gtkserver-encode-widget (widget)
  "Encode a widget in the gtk-server command"
  (cond
    ((stringp widget) widget)
    (t (widget widget))))

(defun cescape-to-lisp (cescaped)
  "String conversion from the c-escaped format to the lisp format"
  (let* ((lunghezza (length cescaped))
	 (prefisso (char cescaped 0))
	 (postfisso (char cescaped (1- lunghezza)))
	 (risultato cescaped))

    ;; Controllo
    (when (not (equal prefisso #\"))
      (error (concatenate 'string "Missing string prefix: " cescaped)))

    (when (not (equal postfisso #\"))
      (error (concatenate 'string "Missing string postfix: " cescaped)))
    
    ;; Rimozione prefissi e postfissi
    (setf risultato (subseq risultato 1))
    (setf risultato (subseq risultato 0 (1- (length risultato))))

    ;; Sostituzione
    (let* ((character-list (loop with escape-status = nil
                                 for character across risultato
                                 appending (cond
                                             ((not escape-status) (cond
                                                                    ((char= character #\\)
                                                                     (setf escape-status t) nil)
                                                                    (t (list character))))
                                             (escape-status (setf escape-status nil)
                                                            (cond
                                                              ((char= character #\n) (list #\Newline))
                                                              ((char= character #\\) (list #\\))
                                                              ((char= character #\t) (list #\Tab))
                                                              (t (list character)))))))
           (encoded-string (coerce character-list 'string)))      
      (cond
        (*utf8-encode-decode* (convert-utf8-to-latin1 encoded-string))
        (t encoded-string)))))
