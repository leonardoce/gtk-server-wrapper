(defsystem "gtk-server"
    :description "Funzioni per accedere al GtkServer"
    :version "1.0"
    :licence "Public Domain"
    :components ((:file "def-package")
                 (:file "base64" :depends-on ("def-package"))
		 (:file "utf8" :depends-on ("def-package"))
		 (:file "gtk-server" :depends-on ("def-package"))
		 (:file "constants" :depends-on ("gtk-server"))
		 (:file "encdec" :depends-on ("gtk-server" "base64" "utf8"))
		 (:file "macros" :depends-on ("encdec"))
		 (:file "mapping" :depends-on ("macros"))
		 (:file "gui-generator" :depends-on ("macros" "mapping"))
                 (:file "gui-components" :depends-on ("gui-generator"))
		 (:file "utilities" :depends-on ("mapping" "constants"))
		 (:file "textual-gui" :depends-on ("mapping"))
                 (:file "guarded-gtk-server" :depends-on ("mapping"))))
