(defpackage clw-warp-stg/game/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :out-of-screen-p
           :make-simple-rect-entity
           :make-simple-circle-entity))
(in-package :clw-warp-stg/game/utils)

(defun.ps+ out-of-screen-p (point margin)
  (with-slots (x y) point
    (or (< x (- 0 margin))
        (> x (+ #lx1000 margin))
        (< y (- 0 margin))
        (> y (+ #ly1000 margin)))))

(defun.ps+ make-simple-rect-entity (&key width height point color depth)
  (let ((entity (make-ecs-entity))
        (offset (make-point-2d :x (* -1/2 width)
                               :y (* -1/2 height))))
    (add-ecs-component-list
     entity
     point
     (make-physic-rect :width width :height height
                       :offset offset)
     (make-model-2d :model (make-solid-rect
                            :width width :height height
                            :color color)
                    :offset offset
                    :depth depth))
    entity))

(defun.ps+ make-simple-circle-entity (&key r point color depth)
  (let ((entity (make-ecs-entity)))
    (add-ecs-component-list
     entity
     point
     (make-physic-circle :r r)
     (make-model-2d :model (make-solid-circle :r r :color color)
                    :depth depth))
    entity))
