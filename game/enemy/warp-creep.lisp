(defpackage clw-warp-stg/game/enemy/warp-creep
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :add-warp-creep-enemy)
  (:import-from :clw-warp-stg/game/enemy/common
                :make-circle-enemy)
  (:import-from :clw-warp-stg/game/parameter
                :get-param))
(in-package :clw-warp-stg/game/enemy/warp-creep)

(defun.ps+ add-warp-creep-enemy (&key first-dist diff-dist
                                      first-angle diff-angle
                                      warp-interval)
  (let* ((point (calc-warp-point first-dist first-angle))
         (enemy (make-circle-enemy :point point
                                   :r (get-my-param :r)
                                   :duration (get-my-param :duration))))
    (add-ecs-component-list
     enemy
     (make-script-2d :func #'warp-creep))
    (set-entity-param enemy
                      :dist first-dist
                      :diff-dist diff-dist
                      :angle first-angle
                      :diff-angle diff-angle
                      :rest-to-warp warp-interval
                      :warp-interval warp-interval)
    (add-ecs-entity-to-buffer enemy)))

(def-stage-element-interpreter.ps+ :enemy-warp-creep
    ((first-dist #lx120) (diff-dist #lx20)
     (first-angle 0) (diff-angle 0)
     (warp-interval 60))
  (add-warp-creep-enemy
   :first-dist first-dist :diff-dist diff-dist
   :first-angle first-angle :diff-angle diff-angle
   :warp-interval warp-interval))

;; --- internal --- ;;

(defun.ps+ warp-creep (enemy)
  (aset-entity-param enemy :rest-to-warp (1- it))
  (when (<= (get-entity-param enemy :rest-to-warp) 0)
    (set-entity-param enemy :rest-to-warp
                      (get-entity-param enemy :warp-interval))
    (update-dist enemy)
    (update-angle enemy)
    (copy-point-2d-to (get-ecs-component 'point-2d enemy)
                      (calc-warp-point (get-entity-param enemy :dist)
                                       (get-entity-param enemy :angle)))))

(defun.ps+ update-dist (enemy)
  (aset-entity-param enemy :dist
                     (max (- it (get-entity-param enemy :diff-dist)) 0)))

(defun.ps+ update-angle (enemy)
  (aset-entity-param enemy :angle
                     (+ it (get-entity-param enemy :diff-angle))))

(defun.ps+ calc-warp-point (dist angle)
  (let* ((player (find-a-entity-by-tag :player))
         (player-point (calc-global-point player)))
    (assert player)
    (make-point-2d
     :x (+ (point-2d-x player-point) (* dist (cos angle)))
     :y (+ (point-2d-y player-point) (* dist (sin angle))))))

(defmacro.ps+ get-my-param (&rest rest)
  `(get-param :enemy :warp-creep ,@rest))
