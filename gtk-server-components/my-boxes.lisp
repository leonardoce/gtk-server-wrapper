(in-package :gtk-server-components)

(defcomponent my-vbox (&rest childs)
  "Simple component who packs all the childs in a vbox component with
expand, fill disabled and padding 2"
  (with-components ((vbox (vbox 0 2)))
    (loop for child in childs
          do (gtk_box_pack_start vbox child 0 0 0))
    vbox))

(defcomponent my-vbox-fill (&rest childs)
  "Simple component who packs all the child in a vbox component with
expand, fill enabled and padding 2"
  (with-components ((vbox (vbox 0 2)))
    (loop for child in childs
          do (gtk_box_pack_start vbox child 1 1 0))
    vbox))

(defcomponent my-hbox (&rest childs)
  "Simple component who packs all the child in a hbox component with
expand and fill disabled and padding 2"
  (with-components ((hbox (hbox 0 2)))
    (loop for child in childs
          do (gtk_box_pack_start hbox child 0 0 0))
    hbox))

(defcomponent my-hbox-fill (&rest childs)
  "Simple component who packs all the childs in a hbox component with
expand and fill enabled and padding 2"
  (with-components ((hbox (hbox 0 2)))
    (loop for child in childs
          do (gtk_box_pack_start hbox child 1 1 0))
    hbox))

(defcomponent my-scrolled-window (child)
  "Simple component who represent a scrolledwindow who have as child the
passed component"
  (with-components ((scroll (scrolledwindow)))
    (gtk_container_add scroll child)
    scroll))

(defcomponent my-window (child &key (title "No title") (hsize 0 hsize-p) (vsize 0 vsize-p))
  "Simple window component with a child inserted with gtk_container_add.
It's possibile to pass other parameters to customize the window behavior.
The default size of the window is set only if you pass the hsize and the vsize parameter"
  (with-components ((win (window 0)))
    (gtk_window_set_title win title)
    (when (and hsize-p vsize-p)
      (gtk_window_set_default_size win hsize vsize))
    (gtk_container_add win child)
    win))

(defcomponent my-modal-window (child parent &key (title "No title") (hsize 0 hsize-p) (vsize 0 vsize-p))
  "Simple window component with a child inserted with gtk_container_add.
It's possibile to pass other parameters to customize the window behavior.
The default size of the window is set only if you pass the hsize and the vsize parameter.
This window is modal and transient for the parent"
  (with-components ((win (window 0)))
    (gtk_window_set_title win title)
    (when (and hsize-p vsize-p)
      (gtk_window_set_default_size win hsize vsize))
    (gtk_container_add win child)
    (gtk_window_set_modal win 1)
    (gtk_window_set_transient_for win parent)
    win))
