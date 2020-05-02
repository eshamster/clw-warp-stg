(defpackage clw-warp-stg/game/player/target-marker
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-target-marker
           :move-target-to
           :lock-on-enemy
           :lock-on-enemy-p)
  (:import-from :clw-warp-stg/game/parameter
                :get-param
                :get-depth)
  (:import-from :ps-experiment/common-macros
                :with-slots-pair))
(in-package :clw-warp-stg/game/player/target-marker)

(defun.ps+ make-target-marker ()
  (let ((marker (make-ecs-entity))
        (r (get-marker-param :r))
        (depth (get-depth :marker)))
    (add-entity-tag marker :marker)
    (let ((fn-enable-free-model (add-free-model marker r depth))
          (fn-enable-lock-on-model (add-lock-on-model marker r depth)))
      (add-ecs-component-list
       marker
       (make-point-2d :x #lx200 :y #ly500)
       (make-script-2d :func #'process-lock-on)
       (make-script-2d :func (lambda (entity)
                               (let ((lock-on-p (get-entity-param
                                                 entity :lock-on-enemy)))
                                 (funcall fn-enable-free-model (not lock-on-p))
                                 (funcall fn-enable-lock-on-model lock-on-p))))
       (init-entity-params :lock-on-enemy nil)))
    marker))

;; XXX: Should calc current-point as global point.
;;      And set it as local point of marker.
;;      Currently they are same.
(defun.ps+ move-target-to (player target-x target-y)
  (let* ((marker (get-target-marker-from-player player))
         (current-point (get-ecs-component 'point-2d marker))
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
          (adjust-point nil #ly1000))))
    (unlock-enemy-if-lock-on marker)))

(defun.ps+ lock-on-enemy (player enemy)
  (let ((marker (get-entity-param player :target-marker)))
    (set-entity-param marker :lock-on-enemy enemy)))

(defun.ps+ lock-on-enemy-p (player)
  (let ((marker (get-entity-param player :target-marker)))
    (get-entity-param marker :lock-on-enemy)))

;; --- internal --- ;;

(defun.ps+ unlock-enemy-if-lock-on (marker)
  (set-entity-param marker :lock-on-enemy nil))

(defun.ps+ get-target-marker-from-player (player)
  (get-entity-param player :target-marker))

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

(defun.ps+ add-free-model (marker r depth)
  (add-marker-model marker r depth #x0000ff))

(defun.ps+ add-lock-on-model (marker r depth)
  (add-marker-model marker r depth #xff0000))

(defun.ps+ add-marker-model (marker r depth color)
  (let* ((models (list (make-model-2d
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
                        :depth depth))))
    (dolist (model models)
      (add-ecs-component model marker))
    (lambda (enable-p)
      (dolist (model models)
        (if enable-p
            (enable-model-2d marker :target-model-2d model)
            (disable-model-2d marker :target-model-2d model))))))

(defun.ps+ process-lock-on (marker)
  (let ((enemy (get-entity-param marker :lock-on-enemy)))
    (when enemy
      (unless (find-the-entity enemy)
        ;; TODO: release lock-on
        (unlock-enemy-if-lock-on marker)
        (return-from process-lock-on))
      ;; Note: ignore angle
      (copy-vector-2d-to (get-ecs-component 'point-2d marker)
                         (calc-global-point enemy)))))

(defmacro.ps+ get-marker-param (&rest rest)
  `(get-param :player :marker ,@rest))
