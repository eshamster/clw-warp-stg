(defpackage clw-warp-stg/game/game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func)
  (:import-from :clw-warp-stg/game/state/package
                :init-clw-warp-stg-state))
(in-package :clw-warp-stg/game/game)

(defun.ps+ init-func (scene)
  (init-clw-warp-stg-state)
  (init-default-systems :scene scene)
  (init-input)
  (setf-collider-model-enable t)
  (setf-collider-model-depth 100))

(defun.ps+ update-func ()
  (let ((count 0))
    (do-ecs-entities entity
      (incf count))
    (add-to-monitoring-log (+ "Entity count: " count)))
  (process-game-state))
