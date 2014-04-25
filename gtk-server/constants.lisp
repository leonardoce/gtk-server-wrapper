(in-package :gtk-server)

;; Helper macro
(defmacro defenum (&rest enum-vars)
  "Define something like C enumerations"
  `(progn ,@(loop for i below (length enum-vars)
     for item in enum-vars
     collecting `(defconstant ,item ,i))))

;; GtkMessageDialog constants
(defenum
  +GTK_MESSAGE_INFO+
  +GTK_MESSAGE_WARNING+
  +GTK_MESSAGE_QUESTION+
  +GTK_MESSAGE_ERROR+)

(defenum
  +GTK_BUTTONS_NONE+
  +GTK_BUTTONS_OK+
  +GTK_BUTTONS_CLOSE+
  +GTK_BUTTONS_CANCEL+
  +GTK_BUTTONS_YES_NO+
  +GTK_BUTTONS_OK_CANCEL+)

;; GtkTreeSelection constants
(defenum
  +GTK_SELECTION_NONE+                           
  +GTK_SELECTION_SINGLE+
  +GTK_SELECTION_BROWSE+
  +GTK_SELECTION_MULTIPLE+)

;; GtkTreeView constants
(defenum
  +GTK_TREE_VIEW_COLUMN_GROW_ONLY+
  +GTK_TREE_VIEW_COLUMN_AUTOSIZE+
  +GTK_TREE_VIEW_COLUMN_FIXED+)



(defconstant +GTK_SELECTION_EXTENDED+ +GTK_SELECTION_MULTIPLE+)

;; Some G-types
(defconstant +G_TYPE_STRING+ 64)

;; Gtk Response constants
(defconstant +GTK_RESPONSE_NONE+ -1)
(defconstant +GTK_RESPONSE_REJECT+ -2)
(defconstant +GTK_RESPONSE_ACCEPT+ -3)
(defconstant +GTK_RESPONSE_DELETE_EVENT+ -4)
(defconstant +GTK_RESPONSE_OK+ -5)
(defconstant +GTK_RESPONSE_CANCEL+ -6)  
(defconstant +GTK_RESPONSE_CLOSE+ -7)
(defconstant +GTK_RESPONSE_YES+ -8)
(defconstant +GTK_RESPONSE_NO+ -9)
(defconstant +GTK_RESPONSE_APPLY+ -10)
(defconstant +GTK_RESPONSE_HELP+ -11)

;; GtkDialog constants
(defconstant +GTK_DIALOG_MODAL+ 1)
(defconstant +GTK_DIALOG_DESTROY_WITH_PARENT+ 2)
(defconstant +GTK_DIALOG_NO_SEPARATOR+ 4)

;; GtkFileChooser constants
(defenum
    +GTK_FILE_CHOOSER_ACTION_OPEN+
    +GTK_FILE_CHOOSER_ACTION_SAVE+
    +GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER+
    +GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER+)

