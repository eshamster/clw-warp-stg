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

(defun.ps+ get-target-marker ()
  (find-a-entity-by-tag :marker))

(defun.ps+ warp-player-to (player target-x target-y)
  (setf-with (get-ecs-component 'point-2d player)
    x (min #lx1000 (max #lx0 target-x))
    y (min #ly1000 (max #ly0 target-y))))

;; XXX: Should calc current-point as global point.
;;      And set it as local point of marker.
;;      Currently they are same.
(defun.ps+ move-target-to (player target-x target-y)
  (let* ((target (get-entity-param player :target-marker))
         (current-point (get-ecs-component 'point-2d target))
         (player-point (calc-global-point player)))
    (with-slots (x y) current-point
      (setf x target-x
            y target-y)
      (flet ((adjust-point (x-p value)
               (let ((new-point (calc-point-on-line
                                 player-point current-point x-p value)))
                 (setf x (point-2d-x new-point)
                       y (point-2d-y new-point)))))
        (when (< x #lx0)
          (adjust-point t #lx0))
        (when (> x #lx1000)
          (adjust-point t #lx1000))
        (when (< y #ly0)
          (adjust-point nil #ly0))
        (when (< y #ly0)
          (adjust-point nil #ly1000))))))

;; --- internal --- ;;

(defun.ps+ calc-point-on-line (pnt1 pnt2 x-p value)
  (with-slots-pair (((x1 x) (y1 y)) pnt1
                    ((x2 x) (y2 y)) pnt2)
    (assert (or (and x-p       (not (= x1 x2)))
                (and (not x-p) (not (= y1 y2)))))
    (let ((lerp-alpha
           (if x-p
               (/ (- value x1) (- x2 x1))
               (/ (- value y1) (- y2 y1)))))
      (make-point-2d :x (lerp-scalar x1 x2 lerp-alpha)
                     :y (lerp-scalar y1 y2 lerp-alpha)))))

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
