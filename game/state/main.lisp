(defpackage clw-warp-stg/game/state/main
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth)
  (:import-from :clw-warp-stg/game/block
                :make-rect-block
                :make-circle-block)
  (:import-from :clw-warp-stg/game/player/main
                :init-player)
  ;; TODO: Move to more appropriate package
  ;;       (The followings are only to load)
  (:import-from :clw-warp-stg/game/enemy/bomb
                :add-bomb-enemy)
  (:import-from :clw-warp-stg/game/enemy/shield
                :add-shield-enemy)
  (:import-from :clw-warp-stg/game/enemy/warp-creep
                :add-warp-creep-enemy)
  (:import-from :clw-warp-stg/game/stage/package
                :generate-ws-stage))
(in-package :clw-warp-stg/game/state/main)

(def-game-state main ((parent (make-ecs-entity))
                      stage-name
                      stage)
  :start-process
  (state-lambda (parent stage stage-name)
    (stack-default-ecs-entity-parent parent)
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
    (init-player)
    ;; --- Test --- ;;
    (setf stage (generate-ws-stage stage-name))
    t)
  :process
  (state-lambda (stage)
    (process-stage stage)
    nil)
  :end-process
  (state-lambda (parent)
    (let ((got-parent (pop-default-ecs-entity-parent)))
      (assert (eq parent got-parent)))
    (delete-ecs-entity parent)
    t))
