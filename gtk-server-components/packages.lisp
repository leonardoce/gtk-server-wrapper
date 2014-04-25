(in-package :common-lisp)

(defpackage :gtk-server-components
  (:use :common-lisp :gtk-server)
  (:export

   ;; Label-entry
   :label-entry
   :get-text
   :set-text

   ;; My-Combo
   :my-combo-box
   :my-combo-box-add
   :my-combo-box-clear
   :my-combo-box-get-active

   ;; My-List
   :my-list
   :my-list-add-row
   :my-list-clear
   :my-list-get-selection

   ;; File-Entry
   :file-entry
   :fe-get-current-file
   :fe-set-current-file
   

   ;; Others
   :initialize-component
   :handle-events

   ;; Boxes
   :my-vbox
   :my-vbox-fill
   :my-hbox
   :my-hbox-fill
   :my-scrolled-window
   :my-window
   :my-modal-window
   ))

(in-package :gtk-server-components)

