(defpackage clw-warp-stg/game/player/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :get-player-param
           :get-player
           :init-basic-player
           :warp-player-to)
  (:import-from :clw-warp-stg/game/utils
                :make-simple-circle-entity
                :add-on-collision-callback
                :add-collision-target-list)
  (:import-from :clw-warp-stg/game/parameter
                :get-param
                :get-depth
                :get-collision-target)
  (:import-from :clw-warp-stg/game/player/target-marker
                :make-target-marker)
  (:import-from :clw-warp-stg/game/player/shot
                :make-shot-maker)
  (:import-from :clw-warp-stg/game/player/shot-line
                :make-shot-line)
  (:import-from :ps-experiment/common-macros
                :setf-with
                :with-slots-pair))
(in-package :clw-warp-stg/game/player/utils)

(defun.ps+ init-basic-player ()
  (let* ((marker (make-target-marker))
         (player (make-player-entity marker))
         (fn-get-player-point (lambda () (calc-global-point player)))
         (fn-get-target-point (lambda () (calc-global-point marker)))
         (shot-maker (make-shot-maker
                      :fn-get-player-point fn-get-player-point
                      :fn-get-target-point fn-get-target-point))
         (shot-line (make-shot-line
                     :fn-get-player-point fn-get-player-point
                     :fn-get-target-point fn-get-target-point)))
    (add-ecs-entity player)
    (add-ecs-entity shot-maker player)
    (add-ecs-entity shot-line player)
    (add-ecs-entity marker)
    (values player marker)))

(defmacro.ps+ get-player-param (&rest keys)
  `(get-param :player ,@keys))

(defun.ps+ get-player ()
  (find-a-entity-by-tag :player))

(defun.ps+ warp-player-to (player target-x target-y)
  (setf-with (get-ecs-component 'point-2d player)
    x (min #lx1000 (max #lx0 target-x))
    y (min #ly1000 (max #ly0 target-y))))

;; --- internal --- ;;

(defun.ps+ make-player-entity (target-marker)
  (let ((player (make-simple-circle-entity
                 :point (make-point-2d :x #lx500 :y #ly500)
                 :r (get-player-param :r)
                 :color #x0000ff
                 :depth (get-depth :player))))
    (add-entity-tag player :player)
    (with-ecs-components (physic-2d) player
      (add-on-collision-callback
       physic-2d #'process-on-collision)
      (add-collision-target-list
       physic-2d (get-collision-target :player)))
    (set-entity-param player
                      :target-marker target-marker)
    player))

(defun.ps+ process-on-collision (mine other)
  (declare (ignore mine))
  (add-to-event-log
   (+ "player collides to: " (ecs-entity-id other))))

