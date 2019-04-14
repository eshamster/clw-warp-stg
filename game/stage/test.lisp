(defpackage :clw-warp-stg/game/stage/test
  (:use :cl
        :parenscript
        :ps-experiment)
  (:import-from :cl-web-2d-game
                :generate-stage
                :stage)
  (:export :generate-test-stage))
(in-package :clw-warp-stg/game/stage/test)

(defun.ps+ generate-test-stage ()
  (generate-stage
    #|
    (dotimes (i 6)
      (stage (:enemy-shield :time (* i 10)
                            :x (+ #lx200 (* i #lx120)) :y #ly200)))
    |#
    (let ((num 12)
          (interval 4)
          (dist #lx120)
          (diff-dist #lx10))
      (dotimes (i num)
        (stage
         (:enemy-warp-creep :time (* i interval)
                            :first-dist (- dist (/ (* diff-dist i) num))
                            :diff-dist diff-dist
                            :first-angle (/ (* 2 PI i) num)
                            :diff-angle (* PI 1/10)
                            :warp-interval (* num interval)))))
    ))
