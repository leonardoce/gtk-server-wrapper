(in-package :gtk-server)


;;; Prende un testo da una textview
(defun gtk_text_view_get_text (textview)
  "Prende tutto il testo contenuto in una textview"
  (let ((start-iter (gtk_frame_new))
	(end-iter (gtk_frame_new))
	(text-buffer (gtk_text_view_get_buffer textview)))
    (gtk_text_buffer_get_start_iter text-buffer start-iter)
    (gtk_text_buffer_get_end_iter text-buffer end-iter)
    (gtk_text_buffer_get_text text-buffer start-iter end-iter 1)))

;;; Imposta il testo in una textview
(defun gtk_text_view_set_text (textview testo)
  "Imposta tutto il testo contenuto in una textview con protezione per stringhe troppo lunghe"
  (let ((buffer-destinazione (gtk_text_view_get_buffer textview))
	(dimensione-split 500)
	(lunghezza-testo (length testo))
	(end-iter (gtk_frame_new)))

    ;; Svuoto il campo di testo
    (gtk_text_buffer_set_text buffer-destinazione "" 0)

    (dotimes (i  (1+ (truncate (/ lunghezza-testo dimensione-split))))
      (let* ((indice-inizio (* i dimensione-split))
	     (indice-fine (min (* (1+ i) dimensione-split) lunghezza-testo))
	     (da-inserire (subseq testo indice-inizio indice-fine))) 
	;; Preleva l'iteratore della fine del testo
	(gtk_text_buffer_get_end_iter buffer-destinazione end-iter)
	
	;; Inserisce il testo
	(gtk_text_buffer_insert buffer-destinazione end-iter da-inserire -1)))))

;;; Finestra con piccolo messaggio
(defun append-in-menu (menu &rest what)
  "Append with gtk_menu_shell_append all things in what to menu"
  (dolist (elem what)
    (gtk_menu_shell_append menu elem)))


;;; Finestre con messaggi

(defun show-dialog (parent type message)
  "Convenient message dialog function"
  (let ((dialog (gtk_message_dialog_new parent
					+GTK_DIALOG_DESTROY_WITH_PARENT+
					type
					+GTK_BUTTONS_CLOSE+
					(replace-all message "%" "%%"))))
    (gtk_dialog_run dialog)
    (gtk_widget_destroy dialog)))

(defun show-info-dialog (parent message)
  "Message information dialog"
  (show-dialog parent +GTK_MESSAGE_INFO+ message))

(defun show-warning-dialog (parent message)
  "Message information dialog"
  (show-dialog parent +GTK_MESSAGE_WARNING+ message))

(defun show-error-dialog (parent message)
  "Message information dialog"
  (show-dialog parent +GTK_MESSAGE_ERROR+ message))

(defun show-yes-no-dialog (parent message)
  "Yes/No dialog. Returns t if yes and nil if no"
  (let* ((dialog (gtk_message_dialog_new parent
					+GTK_DIALOG_DESTROY_WITH_PARENT+
					+GTK_MESSAGE_QUESTION+
					+GTK_BUTTONS_YES_NO+
					message))
	 (dialog-response (gtk_dialog_run dialog)))
    (gtk_widget_destroy dialog)
    (= dialog-response +GTK_RESPONSE_YES+)))

(defun show-choose-between (message choices &key (title "Choose.."))
  "Dialog chooser with a message 'message' and the choises 'choises'"
  (let ((mapping (make-hash-table :test #'equal)))
    (with-components ((win (window 0))
                      (lb (label message))
                      (vbox-tot (vbox 0 2))
                      (hbox-restarts (hbox 0 2)))
      (loop for item in choices
            for bt = (gtk_button_new_with_label (format nil "~a" item))
            do (progn (gtk_box_pack_start hbox-restarts bt 1 1 2)
                      (setf (gethash (format nil "~a" bt) mapping) item)))
      (gtk_window_set_title win title)
      (with-pack-in vbox-tot
        ((:padding 10 :fill 0 :expand 0) lb)
        ((:padding 2 :fill 0 :expand 0 :direction :end) hbox-restarts))
      (gtk_container_add win vbox-tot)
      (gtk_widget_show_all win)
    
      (with-event-loop my-event
        (win (return))
        (t (let ((choose (gethash (format nil "~a" my-event) mapping)))
             (when choose
               (gtk_widget_destroy win)
               (return choose))))))))
