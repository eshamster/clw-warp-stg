(defpackage clw-warp-stg/game/parameter
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :get-param
           :get-depth
           :get-state-param
           :get-collision-target))
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
                    :barrel-angle (* 1/3 PI)))
    :enemy (:shield (:r #lx30
                     :duration 8
                     :rotate-speed (/ PI 120)
                     :shield (:width #lx12 :height #lx90 :dist #lx70))
            :warp-creep (:r #lx8
                         :duration 2)
            :bomb (:r #lx35
                   :duration 24
                   :shot (:num 16 :speed (:min #lx10 :max #lx12)
                          :spread-angle (* PI 1/8)
                          :gravity #lx0.5))))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *params* ,@keys))

(defvar.ps+ *depth*
    (convert-to-layered-hash
     (:background -1000
      :player 0
      :shot -20
      :marker 200
      :block 100
      :enemy 50
      :enemy-shot 50)))

(defmacro.ps+ get-depth (&rest keys)
  `(get-layered-hash *depth* ,@keys))

(defvar.ps+ *state-params*
  (convert-to-layered-hash
   ()))

(defun.ps+ get-collision-target (kind)
  (ecase kind
    (:player '(:block :enemy :enemy-shot))
    (:shot '(:block :enemy))
    (:block '(:shot :player :enemy-shot))
    (:enemy '(:shot :player))
    (:enemy-shot '(:player :block))))
