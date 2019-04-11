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
  (:import-from :clw-warp-stg/game/enemy/shield
                :add-shield-enemy)
  (:import-from :clw-warp-stg/game/enemy/warp-creep
                :add-warp-creep-enemy))
(in-package :clw-warp-stg/game/state/main)

(def-game-state main ((parent (make-ecs-entity))
                      stage)
  :start-process
  (state-lambda (parent stage)
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
    (setf stage
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
