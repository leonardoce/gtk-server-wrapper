(in-package :gtk-server-components)

(defclass label-entry (gui-component)
  ((wid-entry :accessor wid-entry :documentation "Entry widget")
   (widget :accessor widget :documentation "Box that contains the label and the entry")))

(defgeneric get-text (self)
  (:documentation "Get the text in a label-entry widget"))
(defgeneric set-text (self text)
  (:documentation "Set the text in a label-entry widget"))

(defmethod get-text ((self label-entry))
  (gtk_entry_get_text (wid-entry self)))

(defmethod set-text ((self label-entry) text)
  (gtk_entry_set_text (wid-entry self) text))

(defmethod initialize-instance :after ((self label-entry) &key (label-text nil label-text-p) (initial-contents ""))
  (unless label-text-p
    (error "Must give the label-text key property to make-instance"))
  (with-components ((hbox (hbox 0 2))
		    (lb (label label-text))
		    (en (wid-entry)))
    (with-pack-in hbox
      ((:padding 0 :fill 0 :expand 0) lb)
      ((:padding 0 :fill 1 :expand 1) en))
    (setf (wid-entry self) en)
    (setf (widget self) hbox)
    (gtk_entry_set_text en initial-contents)))

(defcomponent label-entry (label-text &optional (initial-contents ""))
  "New label & entry component with the specified label"
  (make-instance 'label-entry :label-text label-text :initial-contents initial-contents))

