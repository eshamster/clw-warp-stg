(defpackage clw-warp-stg/game/block
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-rect-block
           :make-circle-block)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth
                :get-collision-target)
  (:import-from :clw-warp-stg/game/utils
                :make-simple-rect-entity
                :make-simple-circle-entity))
(in-package :clw-warp-stg/game/block)

(defun.ps+ make-rect-block (&key point width height)
  (make-block (make-simple-rect-entity
               :width width :height height
               :point point :color *default-color*
               :depth (get-depth :block))))

(defun.ps+ make-circle-block (&key point r)
  (make-block (make-simple-circle-entity
               :r r
               :point point :color *default-color*
               :depth (get-depth :block))))

;; --- internal --- ;;

(defvar.ps+ *default-color* #x888888)

(defun.ps+ make-block (basic-entity)
  (add-entity-tag basic-entity :block)
  (with-ecs-components (physic-2d) basic-entity
    (setf (physic-2d-target-tags physic-2d)
          (get-collision-target :block)))
  basic-entity)
