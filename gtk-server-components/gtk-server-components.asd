;; (defpackage :gtk-server-components
;;   (:use :asdf :common-lisp))
;; (in-package :gtk-server-components)

(defsystem "gtk-server-components"
  :description "Gtk Server Custom Components"
  :depends-on ("gtk-server")
  :version "1.0"
  :licence "Public Domain"
  :author "Leonardo Cecchi"
  :components ((:file "packages")
	       (:file "label-entry" :depends-on ("packages"))
	       (:file "my-combo" :depends-on ("packages"))
	       (:file "my-list" :depends-on ("packages"))
               (:file "file-entry" :depends-on ("packages"))
               (:file "my-boxes" :depends-on ("packages"))))