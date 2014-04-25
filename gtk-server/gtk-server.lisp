(in-package :gtk-server)

;;;;;;;;;;;;;;;;;;;;;;; Pacchetto per GTK-Server

(defvar *debug-gtk* nil "Enable/Disable Lisp<->Gtk-Server communication debugging")
(defvar *gs-ffi* nil)
(defvar *gs-process* nil)
(defvar *gs-input-socket* nil)
(defvar *gs-output-socket* nil)

(defun gtkserver-args ()
  "The gtk-server arguments"
  '("stdin"))

;; sbcl seems to need this to keep the return type non nil when redefined
;; however it shouldn't hurt other impls.
(declaim (ftype (function (t) t) %gtk))
(defun %gtk (command-string)
  "stub for ffi gtk"
  (error "ffi not loaded"))

(defun init-socket ()
  "(this function is implementation-dependent)"
  #+sbcl
  (progn 
    (setf *gs-process* (sb-ext:run-program "gtk-server" (gtkserver-args)
					   :search t
					   :input :stream :output :stream :wait nil))
    (setf *gs-input-socket* (sb-ext:process-input *gs-process*)) 
    (setf *gs-output-socket* (sb-ext:process-output *gs-process*)) )
  #+openmcl
  (progn 
    (setf *gs-process* (ccl:run-program "gtk-server" (gtkserver-args)
					:input :stream :output :stream :wait nil))
    (setf *gs-input-socket* (ccl:external-process-input-stream *gs-process*)) 
    (setf *gs-output-socket* (ccl:external-process-output-stream *gs-process*)) )

  #+clisp
  (progn
    (setf *gs-input-socket* (ext:run-program "gtk-server" :arguments (gtkserver-args)
					     :input :stream :output :stream :wait t))
    (setf *gs-output-socket* *gs-input-socket*))
  #+cmu
  (progn
    (setf *gs-process* (extensions:run-program "gtk-server" (gtkserver-args)
				   :wait nil :input :stream :output :stream
				   :error nil))
    (when (null *gs-process*)
      (error "Unable to start Gtk-Server"))
    (setf *gs-input-socket* (extensions:process-input *gs-process*))
    (setf *gs-output-socket* (extensions:process-output *gs-process*)))
  #+lispworks
  (progn
    (setf *gs-input-socket* (sys:open-pipe "gtk-server stdin log" :direction :io))
    (setf *gs-output-socket* *gs-input-socket*)))

(defun gtkserver-init ()
  "GtkServer init"
  (unless *gs-ffi*
    (init-socket))
  ;; Inizializza le gtk
  (send-gtk "gtk_init NULL NULL")

  ;; Questo programma lavora solamente in c_escaping
  (gtk_server_enable_c_string_escaping))

;;; Uscita da GtkServer
(defun gtkserver-exit ()
  "Exit from the gtk-server issuing a gtk_exit 0"
  (send-gs-socket "gtk_exit 0"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVENIENCE FUNCTION

(defun send-gs-socket (text)
   "Send some output to the gtk-server socket"
  (cond (*gs-ffi* (%gtk text))
        (t (format *gs-input-socket* "~A~%" text)
           (force-output *gs-input-socket*))))

;; Define communication function
(defun gtk (str)
  "Comunication function. Send the command in 'str' to the gtk-server and returns
the gtk-server output"
  (when *debug-gtk*
    (format t "Send: ~a~%" str))
         
  (let* ((linea (send-gs-socket str))
         (linea (if *gs-ffi* linea 
                    (read-line *gs-output-socket*))))

    (when *debug-gtk*
      (format t "Received: ~a~%" linea))
    linea))

(defun send-gtk (func &rest args)
  "Send a command to the gtk-server stream. It concatenates all the arguments
in args to the command"
  (gtk (format nil "~a ~{~a ~}" func args)))

(defun force-string (what)
  "Force the argument to be a string"
  (format nil "~a" what))

(defun eventp (event widget)
  "Test is an event is from a widget."
  (string= (force-string event) (force-string widget)))

;;; Da Lisp a C-Escape convention 
(defun lisp-to-cescape (cescaped)
  "String conversion from the lisp format to the c-escaped format"
  (let* ((risultato cescaped))

    ;; Sostituzione
    (dolist (elemento '(("\\" "\\\\") ("\""  "\\\"") (#\Newline "\\n") (#\Tab "\\t")))
      (setf risultato (replace-all risultato (string (first elemento)) (string (second elemento)))))

    (concatenate 'string "\"" risultato "\"")))

