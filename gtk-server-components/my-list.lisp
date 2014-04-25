(in-package :gtk-server-components)

(defclass my-list (gui-component)
  ((columns :accessor columns
            :initarg :columns
            :initform (error "Must pass columns key parameter number"))
   (current-data :accessor current-data
		 :initform (make-hash-table :test #'equal)))
  (:documentation "A component who represent a gtk_treeview and a gtk_list_store to
make a list with columns. When you create this component you must pass the columns
names to the creator function. As example (create-component :my-list 'one' 'two' 'tree')"))

(defgeneric my-list-add-row (self &rest text)
  (:documentation "Add a row in the list"))
(defgeneric my-list-clear (self)
  (:documentation "Remove all the rows in the list"))
(defgeneric my-list-get-selection (self)
  (:documentation "Get the selected row. Nil if there are no selected rows"))

(defmethod my-list-add-row ((self my-list) &rest text)
  (let ((row (gtk_frame_new))
        (model (gtk_tree_view_get_model (widget self))))
    (gtk_list_store_append model row)
    (loop for element in text
      for number from 0
      do (gtk_list_store_set model row number element -1))
    ;; Store data in the hash-table
    (let ((chiave (gtk_tree_model_get_string_from_iter model row))
      (valore text))
      (setf (gethash chiave (current-data self)) valore))))

(defmethod my-list-clear ((self my-list))
  (gtk_list_store_clear (gtk_tree_view_get_model (widget self)))
  (clrhash (current-data self)))

(defmethod my-list-get-selection ((self my-list))
  (let ((sel (gtk_tree_view_get_selection (widget self)))
	(row (gtk_frame_new))
	(model (gtk_tree_view_get_model (widget self))))
    (cond
      ((= 0 (gtk_tree_selection_get_selected sel "0" row))
       ;; No selection found....
       nil)
      (t
       ;; Selection found!
       (let ((chiave (gtk_tree_model_get_string_from_iter model row)))
	 (gethash chiave (current-data self)))))))

(defmethod initialize-instance :after ((self my-list) &key)
  "The my-list constructor"

  (with-accessors ((columns columns) (widget widget)) self
        (let ((model (apply #'gtk_list_store_new (loop repeat (length columns) collecting GTK-SERVER::+G_TYPE_STRING+))))
          (setf widget (gtk_tree_view_new))
              
          ;; Fill the columns
          (loop for column in columns
                for column-number from 0
                do (gtk_tree_view_insert_column_with_attributes widget -1
                                                                column (gtk_cell_renderer_text_new)
                                                                "text" column-number))
          ;; Set the model
          (gtk_tree_view_set_model widget model)
          (loop for column-number from 0 below (length columns)
                do (gtk_tree_view_column_set_sizing (gtk_tree_view_get_column widget column-number)
                                                    gtk-server::+GTK_TREE_VIEW_COLUMN_AUTOSIZE+))
              
          ;; Set single mode selection
          (let ((selection (gtk_tree_view_get_selection widget)))
            (gtk_tree_selection_set_mode selection GTK-SERVER::+GTK_SELECTION_SINGLE+))
              
          ;; Unref model and we go
          (g_object_unref model))))

(defcomponent my-list (&rest columns)
  (let ((self (make-instance 'my-list :columns columns)))
    self))
