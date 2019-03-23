(defpackage clw-warp-stg/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game))
(in-package :clw-warp-stg/game/state/main)

(def-game-state main ()
  :start-process
  (lambda (_this)
    (declare (ignore _this))
    t)
  :process
  (lambda (_this)
    (declare (ignore _this))
    nil))
