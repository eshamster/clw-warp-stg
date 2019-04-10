(defpackage clw-warp-stg/game/enemy/shield
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :add-shield-enemy)
  (:import-from :clw-warp-stg/game/block
                :make-rect-block)
  (:import-from :clw-warp-stg/game/enemy/common
                :make-circle-enemy)
  (:import-from :clw-warp-stg/game/parameter
                :get-param))
(in-package :clw-warp-stg/game/enemy/shield)

(defun.ps+ add-shield-enemy (&key point)
  (let* ((enemy (make-circle-enemy
                 :r (get-my-param :r)
                 :duration (get-my-param :duration)
                 :point point))
         (shield-dist (get-my-param :shield :dist))
         (shield (make-rect-block
                  :width (get-my-param :shield :width)
                  :height (get-my-param :shield :height)
                  :point (make-point-2d :x shield-dist :y 0))))
    (add-ecs-component-list
     enemy
     (make-script-2d :func #'rotate-body))
    (add-ecs-entity-to-buffer enemy)
    (add-ecs-entity-to-buffer shield enemy)))

(def-stage-element-interpreter.ps+ :enemy-shield ((x 0) (y 0) (angle 0))
  (add-shield-enemy :point (make-point-2d :x x :y y :angle angle)))

;; --- internal --- ;;

(defmacro.ps+ get-my-param (&rest rest)
  `(get-param :enemy :shield ,@rest))

(defun.ps+ rotate-body (enemy)
  (with-global-point (point enemy)
    (let ((player (find-a-entity-by-tag :player)))
      (assert player)
      (setf (point-2d-angle point)
            (rotate-to-target-angle
             (point-2d-angle point)
             (vector-2d-angle (sub-vector-2d
                               (calc-global-point player) point))
             (get-my-param :rotate-speed))))))
