(defpackage clw-warp-stg/game/player/shot-line
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :make-shot-line)
  (:import-from :clw-warp-stg/game/parameter
                :get-depth))
(in-package :clw-warp-stg/game/player/shot-line)

(defun.ps+ make-shot-line (&key fn-get-player-point
                                fn-get-target-point)
  (let ((line (make-ecs-entity)))
    ;; Temporally, set it out of screen
    (add-ecs-component-list
     line
     (make-point-2d)
     (make-model-2d :model (make-line :pos-a '(0 0)
                                      :pos-b (list (+ #lx1000 #ly1000) 0)
                                      :color #xccccdd)
                    :depth (get-depth :shot-line))
     (make-script-2d :func #'process-shot-line)
     (init-entity-params :fn-get-player-point fn-get-player-point
                         :fn-get-target-point fn-get-target-point))
    line))

;; --- internal --- ;;

(defun.ps+ process-shot-line (line)
  (let ((player-point
         (funcall (get-entity-param line :fn-get-player-point)))
        (target-point
         (funcall (get-entity-param line :fn-get-target-point))))
    (with-ecs-components (model-2d) line
      (let ((offset (model-2d-offset model-2d)))
        (setf (point-2d-angle offset)
              (vector-2d-angle
               (sub-vector-2d target-point player-point)))))))
