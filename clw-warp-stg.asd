#|
  This file is a part of clw-warp-stg project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  A 2D shooting game where player is moved by click

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem clw-warp-stg
  :version "0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack
               :clw-warp-stg/main)
  :description "A 2D shooting game where player is moved by click"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clw-warp-stg/t))))

(defsystem clw-warp-stg/t
  :class :package-inferred-system
  :depends-on (:ps-experiment
               :ps-experiment/t
               :rove
               :alexandria
               :cl-js
               "clw-warp-stg/t/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
