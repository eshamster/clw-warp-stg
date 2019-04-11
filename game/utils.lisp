(defpackage clw-warp-stg/game/utils
  (:use :cl
        :ps-experiment
        :cl-web-2d-game)
  (:export :out-of-screen-p))
(in-package :clw-warp-stg/game/utils)

(defun.ps+ out-of-screen-p (point margin)
  (with-slots (x y) point
    (or (< x (- 0 margin))
        (> x (+ #lx1000 margin))
        (< y (- 0 margin))
        (> y (+ #ly1000 margin)))))
