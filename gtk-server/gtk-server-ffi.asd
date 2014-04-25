(defsystem "gtk-server-ffi"
    :description "Extension to Gtk-Server Wrapper to access via ffi"
    :version "1.0"
    :licence "Public Domain"
    :components ((:file "gtk-server-ffi"))
    :depends-on (:cffi :gtk-server))
