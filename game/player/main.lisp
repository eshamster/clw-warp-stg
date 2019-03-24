(defpackage clw-warp-stg/game/player/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-player)
  (:import-from :clw-warp-stg/game/player/type1
                :init-player-type1))
(in-package :clw-warp-stg/game/player/main)

(defun.ps+ init-player (&optional (type :type1))
  (ecase type
    (:type1 (init-player-type1))))


