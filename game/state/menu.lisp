(defpackage clw-warp-stg/game/state/menu
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game))
(in-package :clw-warp-stg/game/state/menu)

(def-game-state menu ()
  :start-process
  (state-lambda ()
    t)
  :process
  (state-lambda ()
    (make-state :main))
  :end-process
  (state-lambda ()
    t))
