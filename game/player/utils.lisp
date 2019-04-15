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
  (:import-from :ps-experiment/common-macros
                :setf-with
                :with-slots-pair))
(in-package :clw-warp-stg/game/player/utils)

(defun.ps+ init-basic-player ()
  (let* ((marker (make-target-marker))
         (player (make-player-entity marker))
         (line (make-line-to-marker player marker))
         (shot-maker (make-shot-maker
                      :fn-get-player-point (lambda ()
                                             (calc-global-point player))
                      :fn-get-target-point (lambda ()
                                             (calc-global-point marker)))))
    (add-ecs-entity player)
    (add-ecs-entity shot-maker player)
    (add-ecs-entity marker)
    (add-ecs-entity line)
    (values player marker line)))

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

(defun.ps+ make-line-to-marker (player marker)
  (let ((line (make-ecs-entity)))
    (add-entity-tag line :player-to-target-line)
    (add-ecs-component-list
     line
     (make-point-2d)
     (make-line-to-marker-model player marker)
     (make-script-2d
      :func (lambda (entity)
              (update-model-2d
               entity (find-model-2d-by-label entity :line)
               (make-line-to-marker-model player marker)))))
    line))

(defun.ps+ make-line-to-marker-model (player marker)
  (flet ((get-xy-list (entity)
           (let ((point (calc-global-point entity)))
             (list (point-2d-x point)
                   (point-2d-y point)))))
    (make-model-2d
     :model (make-line :pos-a (get-xy-list player)
                       :pos-b (get-xy-list marker)
                       :color #xddddee)
     :depth (1- (get-depth :marker))
     :label :line)))
