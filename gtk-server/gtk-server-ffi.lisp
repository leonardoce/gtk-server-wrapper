(cl:in-package :gtk-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library gtk-server
    (:windows "gtk-server.dll")
    (t "libgtk-server.so")))

(cffi:use-foreign-library gtk-server)

(cffi:defcfun ("gtk" %gtk :library gtk-server) :string
  (command-string :string))

