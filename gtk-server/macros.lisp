;;; Definizioni di alcune macro e metodi che servono per gtk-server
(in-package :gtk-server)

;;; Mapping Generator
(defmacro defgtk (nome-lisp return-value argomenti)
  (let ((nome-gtk (string-downcase (symbol-name nome-lisp))))
    (flet ((converti-argomenti (arg) `(gtkserver-encode ',(second arg) ,(first arg))))

      `(defun ,nome-lisp  ,(mapcar #'first argomenti)
        "Please see the documention of the relative gtk function in the Gtk Api docs"
	(gtkserver-decode-and-check ',return-value
	 (send-gtk ,(format nil "~A" nome-gtk)
	  ,@(loop for arg in argomenti
	       collect (converti-argomenti arg)
	       collect " ")))))))

;;; Macro per il codice con Gtk-Server
(defmacro with-gtk-server ((&rest options) &body body)
  "Protect gtk-servered code and call initializations and cleanup
utilities"
  `(let ((*gs-ffi* ,(getf options :ffi)))
     (unwind-protect
          (progn
            (gtkserver-init)
(format t "bau")
            ,@body)
       (gtkserver-exit))))
