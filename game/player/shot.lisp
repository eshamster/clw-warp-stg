(defpackage clw-warp-stg/game/player/shot
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-shot-maker)
  (:import-from :clw-warp-stg/game/parameter
                :get-param
                :get-depth
                :get-collision-target)
  (:import-from :clw-warp-stg/game/utils
                :out-of-screen-p))
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
      :barrel-r (get-shot-param :barrel-r)
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
      (make-shots (funcall (my-param :fn-get-player-point))
                  (funcall (my-param :fn-get-target-point))
                  (my-param :barrel-angle)
                  (my-param :barrel-dist)
                  (my-param :barrel-r)
                  (my-param :num-once)))))

(defun.ps+ make-shots (player-point target-point barrel-angle barrel-dist barrel-r num-once)
  (let* ((shot-center (calc-shot-center
                       player-point target-point barrel-dist barrel-r))
         (dist (calc-dist shot-center target-point))
         ;; Angle from a line passing through center-point and target-point
         (max-angle (/ (* barrel-r (sin barrel-angle))
                       (- dist (* barrel-r (cos barrel-angle))))))
    (dotimes (i num-once)
      (make-one-shot
       (calc-start-vector shot-center target-point barrel-r
                          (+ (* max-angle -1)
                             (* 2 max-angle (/ i (1- num-once)))))
       target-point))))

(defun.ps+ calc-shot-center (player-point target-point barrel-dist barrel-r)
  ;; The barrel-dist means minimum distance to the barrel circle
  (let ((to-target (sub-vector-2d target-point player-point)))
    (setf-vector-2d-abs to-target (- barrel-dist barrel-r))
    (add-vector-2d player-point to-target)))

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
                                       (declare (ignore other))                                       (register-next-frame-func
                                        (lambda ()
                                          (when (find-the-entity mine)
                                            (delete-ecs-entity mine))))))
     (make-script-2d :func #'delete-if-out-of-screen)
     (init-entity-params :width width
                         :height height))
    (add-ecs-entity-to-buffer shot)))

(defun.ps+ calc-start-vector (shot-center target-point barrel-r angle)
  (let* ((dist (calc-dist shot-center target-point))
         (r barrel-r)
         (base-angle (vector-2d-angle
                      (sub-vector-2d target-point shot-center)))
         (vec (calc-start-vector% r dist angle)))
    (incf-rotate-diff vec r (vector-2d-angle vec) base-angle)
    (add-vector-2d shot-center vec)))

(defun.ps+ calc-start-vector% (r dist angle)
  "Calculate start point
when center of shot circle is origin and shot target is on x axis.
  - r: Radious of circle.
  - dist: Distance to target from origin.
  - angle: Shot angle from x axis."
  ;; The result (x, y) statisfy the followings.
  ;;   - (dist - x) * tan(angle) = y
  ;;   - x^2 + y^2 = r^2
  ;;   - x > 0
  (let* ((tan (tan angle))
         (tan2 (expt tan 2))
         (d (+ (expt r 2)
               (* (expt r 2) tan2)
               (* -1 (expt dist 2) tan2)))
         (sd (sqrt d)))
    (make-vector-2d :x (/ (+ (* dist tan2) sd)
                          (+ tan2 1))
                    :y (/ (* tan (- dist sd))
                          (+ tan2 1)))))

(defun.ps+ delete-if-out-of-screen (shot)
  (let ((margin (* 2 (max (get-entity-param shot :width)
                          (get-entity-param shot :height)))))
    (when (out-of-screen-p (calc-global-point shot) margin)
      (register-next-frame-func
       (lambda ()
         (when (find-the-entity shot)
           (delete-ecs-entity shot)))))))

(defmacro.ps+ get-shot-param (&rest keys)
  `(get-param :player :shot ,@keys))
