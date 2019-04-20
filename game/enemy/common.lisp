(defpackage clw-warp-stg/game/enemy/common
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-primitive-enemy
           :make-rect-enemy
           :make-circle-enemy
           :add-on-die-callback)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth
                :get-collision-target)
  (:import-from :clw-warp-stg/game/utils
                :make-simple-rect-entity
                :make-simple-circle-entity
                :add-on-collision-callback
                :add-collision-target-list))
(in-package :clw-warp-stg/game/enemy/common)

(defun.ps+ make-rect-enemy (&key point width height duration)
  (make-primitive-enemy
   :basic-entity (make-simple-rect-entity
                  :width width :height height
                  :point point :color *default-color*
                  :depth (get-depth :enemy))
   :duration duration))

(defun.ps+ make-circle-enemy (&key point r duration)
  (make-primitive-enemy
   :basic-entity (make-simple-circle-entity
                  :r r
                  :point point :color *default-color*
                  :depth (get-depth :enemy))
   :duration duration))

(defun.ps+ make-primitive-enemy (&key basic-entity duration)
  (assert (and duration (> duration 0)))
  (add-entity-tag basic-entity :enemy)
  (with-ecs-components (physic-2d) basic-entity
    (add-on-collision-callback
     physic-2d
     (lambda (mine other)
       (when (has-entity-tag other :player-shot)
                  (on-collision-to-shot mine other))))
    (add-collision-target-list
     physic-2d (get-collision-target :enemy)))
  (set-entity-param basic-entity
                    :rest-duration duration
                    :on-die (list))
  basic-entity)

(defun.ps+ add-on-die-callback (enemy callback)
  "The callback takes enemy"
  (push callback (get-entity-param enemy :on-die)))

;; --- internal --- ;;

(defun.ps+ on-collision-to-shot (mine other)
  (declare (ignore other))
  (aset-entity-param mine :rest-duration (1- it))
  (when (<= (get-entity-param mine :rest-duration) 0)
    (register-next-frame-func
     (lambda ()
       (delete-enemy mine)))))

(defvar.ps+ *default-color* #xee22222)

(defun.ps+ delete-enemy (enemy)
  (when (find-the-entity enemy)
    (dolist (callback (get-entity-param enemy :on-die))
      (funcall callback enemy))
    (delete-ecs-entity enemy)))
