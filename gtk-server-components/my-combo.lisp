(in-package :gtk-server-components)

(defclass my-combo-box (gui-component)
  ((widget :accessor widget :documentation "Widget ID" :initarg :widget)
   (item-count :accessor item-count :documentation "Item count" :initform 0))
  (:documentation "Extented text combo box"))

(defgeneric my-combo-box-clear (self)
  (:documentation "Clear the contents of this combo box"))
(defgeneric my-combo-box-add (self string)
  (:documentation "Add a string in this combo box"))
(defgeneric my-combo-box-get-active (self)
  (:documentation "Return the active string of this combo box"))
(defgeneric initialize-component (self)
  (:documentation "Initialize a component"))

(defmethod my-combo-box-add ((self my-combo-box) string)
  (gtk_combo_box_append_text self string)
  (with-accessors ((count item-count)) self
    (incf count)))

(defmethod my-combo-box-clear ((self my-combo-box))
  (with-accessors ((count item-count) (widget widget)) self
    (dotimes (i count)
      (gtk_combo_box_remove_text widget 0))
    (gtk_combo_box_set_active widget -1)))

(defmethod my-combo-box-get-active ((self my-combo-box))
  (gtk_combo_box_get_active_text (widget self)))

(defmethod initialize-component ((self my-combo-box))
  (with-components ((cb (combo)))
    (setf (widget self) cb)))

(defcomponent my-combo-box ()
  "New combo box"
  (let ((comp (make-instance 'my-combo-box)))
    (initialize-component comp)
    comp))

(defun test-combo-box ()
  "Combo box text"
  (with-gtk-server '()
    (with-components ((win (window 0))
		      (cb (my-combo-box))
		      (en (label-entry "Combo Box Entry"))
		      (bt-add (button "Add"))
		      (bt-clear (button "Clear"))
		      (bt-get-active (button "Get Active"))
		      (hbox (hbox 0 2))
		      (vbox (vbox 0 2)))
      ;; Hbox
      (with-pack-in hbox
	((:padding 2 :fill 1 :expand 1) bt-add)
	((:padding 2 :fill 1 :expand 1) bt-clear)
	((:padding 2 :fill 1 :expand 1) bt-get-active))
      ;; Vbox
      (with-pack-in vbox
	((:padding 2 :fill 0 :expand 0) cb)
	((:padding 2 :fill 0 :expand 0) en)
	((:padding 2 :fill 0 :expand 0) hbox))
      ;; Window
      (gtk_window_set_title win "Combo Box & Label & Entry test")
      (gtk_container_add win vbox)
      (gtk_widget_show_all win)
      ;; Event loop
      (with-event-loop my-event
	(win
	 (return))
	(bt-add
	 (let ((text (get-text en)))
	   (unless (= 0 (length (string-trim " " text)))
	     (my-combo-box-add cb text))))
	(bt-clear
	 (my-combo-box-clear cb))
	(bt-get-active
	 (set-text en (my-combo-box-get-active cb)))))))
