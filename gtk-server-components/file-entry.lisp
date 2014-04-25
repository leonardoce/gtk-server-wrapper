(in-package :gtk-server-components)

;;;; -------------------------------------------------------------------------
;;;; ------------------------------------------------- File Entry Component --
;;;; -------------------------------------------------------------------------
(defclass file-entry (gui-component)
  ((en-file :accessor fe-en-file)
   (bt-file :accessor fe-bt-file)
   (dialog-type :accessor fe-dialog-type :initform 'load :initarg :dialog-type)
   (button-caption :accessor fe-button-caption :initform "Load" :initarg :button-caption)
   (dialog-title :accessor fe-dialog-title :initform "Choose file..." :initarg :dialog-title)
   (parent :accessor fe-parent :initform (error "Must give parent") :initarg :parent)))
(defgeneric fe-get-current-file (self)
  (:documentation "Get the current selected file (a get_text from the entry field)"))
(defgeneric fe-set-current-file (self file)
  (:documentation "Set the current selected file (a set_text to the entry field)"))
(defgeneric handle-events (self event)
  (:documentation "Generic event handler function for a gui component. You should
pass the component and the received event"))

(defmethod initialize-component ((self file-entry))
  "Initializing file-entry component"
  (with-accessors ((en-file fe-en-file) (bt-file fe-bt-file) (widget widget)
                   (button-caption fe-button-caption)) self
     (with-components ((widget (hbox 0 2) :nodecl)
                       (bt-file (button button-caption) :nodecl)
                       (en-file (entry) :nodecl))
       (with-pack-in widget
         ((:padding 2 :expand 1 :fill 1) en-file)
         ((:padding 2 :expand 0 :fill 0) bt-file)))))

(defmethod handle-events ((self file-entry) event)
  "Handle events for the file-entry component"
  (with-accessors ((bt-file fe-bt-file) (parent fe-parent)
                   (dialog-title fe-dialog-title) (button-caption fe-button-caption)
                   (dialog-type fe-dialog-type) (en-file fe-en-file)) self
      (when (eventp event bt-file)
        ;; Show file dialog
        (let* ((correct-action (cond
                                 ((string= dialog-type 'load) gtk-server::+gtk_file_chooser_action_open+)
                                 ((string= dialog-type 'save) gtk-server::+gtk_file_chooser_action_save+)
                                 (t (error "Unknown action"))))
               (chooser-dialog (gtk_file_chooser_dialog_new dialog-title
                                                            parent
                                                            correct-action
                                                            "Cancel" 0
                                                            button-caption 1))
               (chooser-response (gtk_dialog_run chooser-dialog)))
          (when (= chooser-response 1)
            (gtk_entry_set_text en-file (gtk_file_chooser_get_filename chooser-dialog)))
          (gtk_widget_destroy chooser-dialog)))))

(defmethod fe-get-current-file ((self file-entry))
  "Get the current selected file (a get_text from the entry field)"
  (gtk_entry_get_text (fe-en-file self)))

(defmethod fe-set-current-file ((self file-entry) file)
  "Set the current selected file (a set_text to the entry field)"
  (gtk_entry_set_text (fe-en-file self) file))

(defun create-file-entry (caption dialog-type parent dialog-title)
  "File entry component creator function"
  (let ((self (make-instance 'file-entry
                             :dialog-type dialog-type
                             :button-caption caption
                             :parent parent
                             :dialog-title dialog-title)))
    (initialize-component self)
    self))

(defcomponent file-entry (&key (caption "Load") (dialog-type 'load) parent
                               (dialog-title "Choose file..."))
  "File entry component composed of an entry and a button. When the user
press the button this component shows an open/save dialog."
  (create-file-entry caption dialog-type parent dialog-title))

(defcomponent label-file-entry (&key (label "File:") (caption "Load") (dialog-type 'load) parent
                               (dialog-title "Choose file..."))
  "File entry component composed of an entry and a button. When the user
press the button this component shows an open/save dialog. This component
also has a label placed on the left"
  (let ((self (create-file-entry caption dialog-type parent dialog-title))
        (hbox (gtk_hbox_new 0 2)))
    (with-pack-in hbox
      ((:padding 0 :fill 0 :expand 0) (gtk_label_new label))
      ((:padding 2 :fill 1 :expand 1) self))
    (setf (widget self) hbox)
    self))

(defun file-entry-test ()
  (with-gtk-server '()
      (with-components ((win (window 0))
                        (bt-quit (button "Quit"))
                        (vbox (vbox 0 2))
                        (fe-test (label-file-entry :caption "Loading" :dialog-type 'load
                                                   :label "Choose File:"
                                                   :parent win))
                        (fe-test2 (label-file-entry :caption "Loading" :dialog-type 'load
                                                   :parent win) ))
        (with-pack-in vbox
          ((:padding 2 :fill 0 :expand 1) fe-test)
          ((:padding 2 :fill 0 :expand 1) fe-test2)
          ((:padding 2 :fill 0 :expand 0) bt-quit))
        (gtk_container_add win vbox)
        (gtk_window_set_title win "Testing File-Entry Component")
        (gtk_widget_show_all win)

        (with-event-loop my-event
          (win (return))
          (bt-quit (return))
          (t (handle-events fe-test my-event)
             (handle-events fe-test2 my-event))))))


