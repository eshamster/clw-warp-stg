(in-package :cl-user)
(defpackage clw-warp-stg/t/main
  (:use :cl
        :clw-warp-stg
        :rove))
(in-package :clw-warp-stg/t/main)

(defvar *port* 21464)

(deftest test-connection
  (unwind-protect
       (progn
         (clw-warp-stg:start :port *port*)
         (handler-case
             (let ((connected nil))
               (dotimes (i 5)
                 (when (dex:get (format nil "http://localhost:~D" *port*))
                   (setf connected t)
                   (return))
                 (sleep 1))
               (ok connected)) 
           (error (e)
             (fail (format nil "~A" e)))))
    (clw-warp-stg:stop)))
