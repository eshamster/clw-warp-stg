(defpackage clw-warp-stg/game/enemy/bomb
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :add-bomb-enemy)
  (:import-from :clw-warp-stg/game/enemy/common
                :make-circle-enemy
                :add-on-die-callback)
  (:import-from :clw-warp-stg/game/enemy/shot
                :make-circle-enemy-shot)
  (:import-from :clw-warp-stg/game/parameter
                :get-param))
(in-package :clw-warp-stg/game/enemy/bomb)

(defun.ps+ add-bomb-enemy (&key point)
  (let* ((enemy (make-circle-enemy
                 :r (get-my-param :r)
                 :duration (get-my-param :duration)
                 :point point)))
    (add-on-die-callback enemy #'explode)
    (add-ecs-entity-to-buffer enemy)))

(def-stage-element-interpreter.ps+ :enemy-bomb ((x 0) (y 0) (angle 0))
  (add-bomb-enemy :point (make-point-2d :x x :y y :angle angle)))

;; --- internal --- ;;

(defmacro.ps+ get-my-param (&rest rest)
  `(get-param :enemy :bomb ,@rest))

(defun.ps-only random1 ()
  (random))
(defun random1 ()
  (random 1.0))

(defun.ps+ explode (enemy)
  (let* ((num (get-my-param :shot :num))
         (enemy-point (calc-global-point enemy))
         (player-point
          (calc-global-point (find-a-entity-by-tag :player)))
         (base-angle (vector-2d-angle
                      (sub-vector-2d player-point enemy-point)))
         (spread-angle (get-my-param :shot :spread-angle)))
    (dotimes (i num)
      (let ((angle (+ base-angle
                      (lerp-scalar (* -1/2 spread-angle)
                                   (*  1/2 spread-angle)
                                   (random1))))
            (speed (lerp-scalar
                    (get-my-param :shot :speed :min)
                    (get-my-param :shot :speed :max)
                    (random1)))
            (shot (make-circle-enemy-shot
                   :point (clone-point-2d enemy-point)
                   :r #lx8)))
        (add-ecs-component-list
         shot
         (make-speed-2d :x (* speed (cos angle))
                        :y (* speed (sin angle))))
        (add-ecs-entity-to-buffer shot)))))
