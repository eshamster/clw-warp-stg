(defpackage clw-warp-stg/game/player/shot
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-shot-maker)
  (:import-from :clw-warp-stg/game/parameter
                :get-param
                :get-depth
                :get-collision-target))
(in-package :clw-warp-stg/game/player/shot)

(defun.ps+ make-shot-maker (&key fn-get-player-point
                                 fn-get-target-point)
  (let ((maker (make-ecs-entity)))
    (add-ecs-component-list
     maker
     (make-script-2d :func #'process-shot-maker)
     (init-entity-params
      :interval (get-shot-param :interval)
      :rest-interval 0
      :barrel-dist (get-shot-param :barrel-dist)
      :barrel-angle (get-shot-param :barrel-angle)
      :num-once (get-shot-param :num-once)
      :fn-get-player-point fn-get-player-point
      :fn-get-target-point fn-get-target-point))
    maker))

;; --- internal --- ;;

(defun.ps+ process-shot-maker (maker)
  (aset-entity-param maker :rest-interval (1- it))
  (flet ((my-param (key)
           (get-entity-param maker key)))
    (when (<= (my-param :rest-interval) 0)
      (set-entity-param maker :rest-interval (my-param :interval))
      (let* ((player-point
              (funcall (my-param :fn-get-player-point)))
             (target-point
              (funcall (my-param :fn-get-target-point)))
             (angle-to-target (vector-2d-angle
                               (sub-vector-2d target-point player-point)))
             (barrel-angle (my-param :barrel-angle))
             (barrel-dist (my-param :barrel-dist))
             (num-once (my-param :num-once)))
        (dotimes (i num-once)
          (let* ((diff-angle (+ (* barrel-angle -1/2)
                                (* barrel-angle
                                   (/ i (1- num-once)))))
                 (shot-angle (+ angle-to-target diff-angle)))
            (make-one-shot
             (add-vector-2d player-point
                            (make-vector-2d :x (* barrel-dist (cos shot-angle))
                                            :y (* barrel-dist (sin shot-angle))))
             target-point)))))))

(defun.ps+ make-one-shot (start-point target-point)
  (let* ((shot (make-ecs-entity))
         (angle (vector-2d-angle
                 (sub-vector-2d target-point start-point)))
         (cos-val (cos angle))
         (sin-val (sin angle))
         (speed-abs (get-shot-param :speed))
         (width (get-shot-param :width))
         (height (get-shot-param :height)))
    (add-entity-tag shot :player-shot)
    (add-ecs-component-list
     shot
     (transformf-point (make-point-2d :x (* -1/2 width)
                                      :y (* -1/2 height))
                       (make-point-2d :x (vector-2d-x start-point)
                                      :y (vector-2d-y start-point)
                                      :angle angle))
     (make-speed-2d :x (* speed-abs cos-val)
                    :y (* speed-abs sin-val))
     (make-model-2d :model (make-solid-rect :width width
                                            :height height
                                            :color #x0000ff)
                    :depth (get-depth :shot))
     (make-physic-rect :width width :height height
                       :target-tags (get-collision-target :shot)
                       :on-collision (lambda (mine other)
                                       (declare (ignore other))
                                       (register-next-frame-func
                                        (lambda ()
                                          (when (find-the-entity mine)
                                            (delete-ecs-entity mine))))))
     (make-script-2d :func #'delete-if-out-of-screen)
     (init-entity-params :width width
                         :height height))
    (add-ecs-entity-to-buffer shot)))

(defun.ps+ delete-if-out-of-screen (shot)
  (let ((margin (* 2 (max (get-entity-param shot :width)
                          (get-entity-param shot :height)))))
    (with-slots (x y) (calc-global-point shot)
      (when (or (< x (- 0 margin))
                (> x (+ #lx1000 margin))
                (< y (- 0 margin))
                (> y (+ #ly1000 margin)))
        (register-next-frame-func
         (lambda ()
           (when (find-the-entity shot)
             (delete-ecs-entity shot))))))))

(defmacro.ps+ get-shot-param (&rest keys)
  `(get-param :player :shot ,@keys))
