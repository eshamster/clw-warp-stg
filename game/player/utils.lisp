(defpackage clw-warp-stg/game/player/utils
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :get-player-param
           :get-player
           :init-basic-player
           :warp-player-to
           :move-target-to
           :get-target-marker)
  (:import-from :clw-warp-stg/game/parameter
                :get-param
                :get-depth)
  (:import-from :clw-warp-stg/game/player/shot
                :make-shot-maker)
  (:import-from :ps-experiment/common-macros
                :setf-with))
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

(defun.ps+ get-target-marker ()
  (find-a-entity-by-tag :marker))

(defun.ps+ warp-player-to (player target-x target-y)
  (setf-with (get-ecs-component 'point-2d player)
    x target-x
    y target-y))

(defun.ps+ move-target-to (player target-x target-y)
  (setf-with (get-ecs-component
              'point-2d
              (get-entity-param player :target-marker))
    x target-x
    y target-y))

;; --- internal --- ;;

(defun.ps+ make-player-entity (target-marker)
  (let ((player (make-ecs-entity)))
    (add-entity-tag player :player)
    (add-ecs-component-list
     player
     (make-point-2d :x #lx500 :y #ly500)
     (make-model-2d
      :model (make-solid-circle :r (get-player-param :r)
                                :color #x0000ff)
      :depth (get-depth :player))
     (init-entity-params :target-marker target-marker))
    player))

(defun.ps+ make-target-marker ()
  (let ((marker (make-ecs-entity))
        (r (get-player-param :marker :r))
        (color #x0000ff)
        (depth (get-depth :marker)))
    (add-entity-tag marker :marker)
    (add-ecs-component-list
     marker
     (make-point-2d :x #lx200 :y #ly500)
     (make-model-2d
      :model (make-wired-circle
              :r r
              :color color)
      :depth depth)
     (make-model-2d
      :model (make-line :pos-a (list (* -1 r) 0)
                        :pos-b (list r 0)
                        :color color)
      :depth depth)
     (make-model-2d
      :model (make-line :pos-a (list 0 (* -1 r))
                        :pos-b (list 0 r)
                        :color color)
      :depth depth))
    marker))

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
