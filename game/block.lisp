(defpackage clw-warp-stg/game/block
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block
           :make-circle-block)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth
                :get-collision-target))
(in-package :clw-warp-stg/game/block)

(defun.ps+ make-rect-block (&key point width height)
  (let ((offset (make-point-2d :x (* -1/2 width)
                               :y (* -1/2 height))))
    (make-block :point point
                :physic (make-physic-rect :width width
                                          :height height
                                          :offset offset)
                :model (make-model-2d :model (make-solid-rect
                                              :width width :height height
                                              :color *default-color*)
                                      :offset offset
                                      :depth (get-depth :block)))))

(defun.ps+ make-circle-block (&key point r)
  (let ((offset (make-point-2d :x (* -1 r)
                               :y (* -1 r))))
    (make-block :point point
                :physic (make-physic-circle :r r
                                            :offset offset)
                :model (make-model-2d :model (make-solid-circle
                                              :r r :color *default-color*)
                                      :offset offset
                                      :depth (get-depth :block)))))

;; --- internal --- ;;

(defvar.ps+ *default-color* #x888888)

(defun.ps+ make-block (&key point physic model)
  (setf (physic-2d-target-tags physic)
        (get-collision-target :block))
  (let ((block (make-ecs-entity)))
    (add-entity-tag block :block)
    (add-ecs-component-list
     block
     physic model point)
    block))
