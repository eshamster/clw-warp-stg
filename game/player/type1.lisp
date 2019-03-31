(defpackage clw-warp-stg/game/player/type1
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-player-type1)
  (:import-from :clw-warp-stg/game/player/utils
                :init-basic-player
                :warp-player-to
                :move-target-to))
(in-package :clw-warp-stg/game/player/type1)

(defun.ps+ init-player-type1 ()
  (let ((player (init-basic-player)))
    (add-ecs-component-list
     player
     (make-script-2d :func (lambda (entity)
                             (control-player entity))))))

(defun.ps+ control-player (player)
  (when (or (key-down-now-p :a)
            (= (get-left-mouse-state) :down-now))
    (warp-player-to
     player (get-mouse-x) (get-mouse-y)))
  (when (key-down-p :b)
    (move-target-to
     player (get-mouse-x) (get-mouse-y))))
