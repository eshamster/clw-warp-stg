(defpackage clw-warp-stg/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth)
  (:import-from :clw-warp-stg/game/player/main
                :init-player))
(in-package :clw-warp-stg/game/state/main)

(def-game-state main ((parent (make-ecs-entity)))
  :start-process
  (state-lambda (parent)
    (with-ecs-entity-parent (parent)
      (let ((background (make-ecs-entity)))
        (add-ecs-component-list
         background
         (make-point-2d :x #lx0 :y #ly0)
         (make-model-2d
          :model (make-solid-rect :width #lx1000
                                  :height #ly1000
                                  :color #xeeeeee)
          :depth (get-depth :background)))
        (add-ecs-entity background))
      (init-player))
    t)
  :process
  (state-lambda (parent)
    (with-ecs-entity-parent (parent)
      )
    nil)
  :end-process
  (state-lambda (parent)
    (delete-ecs-entity parent)
    t))
