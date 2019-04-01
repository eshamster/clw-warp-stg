(defpackage clw-warp-stg/game/enemy/common
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-primitive-enemy
           :make-rect-enemy
           :make-circle-enemy)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth
                :get-collision-target))
(in-package :clw-warp-stg/game/enemy/common)

(defun.ps+ make-rect-enemy (&key point width height duration)
  (let ((offset (make-point-2d :x (* -1/2 width)
                               :y (* -1/2 height))))
    (make-primitive-enemy
     :point point
     :physic (make-physic-rect :width width
                               :height height
                               :offset offset)
     :model (make-model-2d :model (make-solid-rect
                                   :width width :height height
                                   :color *default-color*)
                           :offset offset
                           :depth (get-depth :block))
     :duration duration)))

(defun.ps+ make-circle-enemy (&key point r duration)
  (make-primitive-enemy
   :point point
   :physic (make-physic-circle :r r)
   :model (make-model-2d :model (make-solid-circle
                                 :r r :color *default-color*)
                         :depth (get-depth :block))
   :duration duration))

(defun.ps+ make-primitive-enemy (&key point physic model
                                      duration)
  (with-slots (target-tags on-collision) physic
    (let ((on-collision-org on-collision))
      (setf on-collision
            (lambda (mine other)
              (funcall on-collision-org mine other)
              (when (has-entity-tag other :player-shot)
                (on-collision-to-shot mine other)))))
    (dolist (tag (get-collision-target :enemy))
      (push tag target-tags)))
  (let ((enemy (make-ecs-entity)))
    (add-entity-tag enemy :enemy)
    (add-ecs-component-list
     enemy
     physic model point
     (init-entity-params :rest-duration duration))
    enemy))

;; --- internal --- ;;

(defun.ps+ on-collision-to-shot (mine other)
  (declare (ignore other))
  (aset-entity-param mine :rest-duration (1- it))
  (when (<= (get-entity-param mine :rest-duration) 0)
    (register-next-frame-func
     (lambda ()
       (when (find-the-entity mine)
         (delete-ecs-entity mine))))))

(defvar.ps+ *default-color* #xee22222)
