(defpackage clw-warp-stg/game/enemy/shot
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-enemy-shot
           :make-circle-enemy-shot)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth
                :get-collision-target)
  (:import-from :clw-warp-stg/game/utils
                :make-simple-rect-entity
                :make-simple-circle-entity
                :add-on-collision-callback
                :add-collision-target-list))
(in-package :clw-warp-stg/game/enemy/shot)

(defun.ps+ make-rect-enemy-shot (&key point width height)
  (make-primitive-enemy-shot
   :basic-entity (make-simple-rect-entity
                  :width width :height height
                  :point point :color *default-color*
                  :depth (get-depth :enemy))))

(defun.ps+ make-circle-enemy-shot (&key point r)
  (make-primitive-enemy-shot
   :basic-entity (make-simple-circle-entity
                  :r r
                  :point point :color *default-color*
                  :depth (get-depth :enemy))))

(defun.ps+ make-primitive-enemy-shot (&key basic-entity)
  (add-entity-tag basic-entity :enemy-shot)
  (with-ecs-components (physic-2d) basic-entity
    (add-on-collision-callback
     physic-2d
     (lambda (mine other)
       (when (or (has-entity-tag other :player)
                 (has-entity-tag other :block))
         (register-next-frame-func
          (lambda ()
            (delete-ecs-entity mine))))))
    (add-collision-target-list
     physic-2d (get-collision-target :enemy-shot)))
  basic-entity)

;; --- internal --- ;;

(defvar.ps+ *default-color* #xff8800)

