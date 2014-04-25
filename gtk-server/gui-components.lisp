(in-package :gtk-server)

(defmacro define-component-from-call (&rest pair-list)
  "Fill an hash table with key-value pairs"
  (let ((spec (copy-list pair-list)))
    `(progn
	  ,@(loop while spec
                  collecting `(defun ,(pop spec) (&rest args)
                               "Please see the gtk documentation"
                               (apply ,(pop spec) args))))))

(define-component-from-call
    button #'gtk_button_new_with_label
    button-empty #'gtk_button_new
    combo #'gtk_combo_box_new_text
    entry #'gtk_entry_new
    frame #'gtk_frame_new
    hbox #'gtk_hbox_new
    image #'gtk_image_new
    image-f #'gtk_image_new_from_file
    label #'gtk_label_new
    menu #'gtk_menu_new
    menubar #'gtk_menu_bar_new
    menuitem #'gtk_menu_item_new
    menuitem-l #'gtk_menu_item_new_with_label
    menuitem-m #'gtk_menu_item_new_with_mnemonic
    scrolledwindow #'gtk_scrolled_window_new
    table #'gtk_table_new
    textview #'gtk_text_view_new
    vbox #'gtk_vbox_new
    window #'gtk_window_new)

