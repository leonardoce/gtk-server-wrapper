(in-package :gtk-server)

(defun gtk-server-default-handler (condition)
  "Default Gtk-Server condition handler"
  (let* ((decline-message "Decline")
         (available-restarts (compute-restarts))
         (choosed-restart (show-choose-between (format nil "Received error: ~a" condition)
                                               (append (list decline-message) available-restarts)
                                               :title "Default Condition Handler")))
    (cond
      ((and choosed-restart
            (not (equal choosed-restart decline-message))) (invoke-restart choosed-restart))
      (t nil))))

(defmacro with-guarded-gtk-server ((&rest options) &body body)
  `(with-gtk-server ,options
    (handler-bind ((condition #'gtk-server-default-handler))
          ,@body)))
