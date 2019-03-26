(defpackage clw-warp-stg/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param
           :get-depth
           :get-state-param))
(in-package :clw-warp-stg/game/parameter)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar.ps+ field-height 600)
  (defvar.ps+ field-width (/ (* field-height 4.0) 3))

  (defun.ps+ calc-absolute-length (relative-length base-length)
    (* relative-length base-length 0.001))

  "#Ex1. '#lx500' represents a half length of the field width."
  "#Ex2. '#ly500' represents a half length of the field height."
  (set-dispatch-macro-character
   #\# #\l
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (case (peek-char nil stream)
         (#\x (read-char stream)
              `(calc-absolute-length ,(read stream) field-width))
         (#\y (read-char stream)
              `(calc-absolute-length ,(read stream) field-height))
         (t (error "Not recognized character after #l"))))))

(defvar.ps+ *params*
  (convert-to-layered-hash
   (:player (:r #lx6
             :marker (:r #lx12)
             :shot (:width #lx40 :height #ly7
                    :speed #lx15
                    :interval 8
                    :num-once 5
                    :barrel-dist #lx40
                    :barrel-angle (* 2/3 PI))))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))

(defvar.ps+ *depth*
    (convert-to-layered-hash
     (:background -1000
      :player 0
      :shot -20
      :marker -10)))

(defmacro.ps+ get-depth (&rest keys)
  `(get-layered-hash *depth* ,@keys))

(defvar.ps+ *state-params*
  (convert-to-layered-hash
   ()))

(defmacro.ps+ get-state-param (&rest keys)
  `(get-layered-hash *state-params* ,@keys))
