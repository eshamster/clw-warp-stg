(defpackage clw-warp-stg/game/player/type1
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-player-type1)
  (:import-from :clw-warp-stg/game/player/target-marker
                :move-target-to
                :lock-on-enemy
                :lock-on-enemy-p)
  (:import-from :clw-warp-stg/game/player/utils
                :init-basic-player
                :warp-player-to
                :get-player-param))
(in-package :clw-warp-stg/game/player/type1)

(defun.ps+ init-player-type1 ()
  (let ((player (init-basic-player)))
    (add-ecs-component-list
     player
     (make-script-2d :func (lambda (entity)
                             (control-player entity))))))

;; --- internal --- ;;

(defun.ps+ control-player (player)
  (when (or (key-down-now-p :a)
            (= (get-left-mouse-state) :down-now))
    (unless (or (find-block-under-mouse)
                (find-enemy-under-mouse))
      (warp-player-to
       player (get-mouse-x) (get-mouse-y))))
  (flet ((move-target-to-mouse ()
           (move-target-to player (get-mouse-x) (get-mouse-y))))
    (when (key-down-now-p :b)
      (let ((enemy (find-enemy-under-mouse)))
        (if enemy
            (lock-on-enemy player enemy)
            (move-target-to-mouse))))
    (when (and (key-down-p :b)
               (not (lock-on-enemy-p player)))
      (move-target-to-mouse))))

;; TODO: This should be shared with other player types.
(defun.ps+ find-enemy-under-mouse ()
  (find-target-under-mouse :enemy))

(defun.ps+ find-block-under-mouse ()
  (find-target-under-mouse :block))

(defun.ps+ find-target-under-mouse (target-tag)
  (let ((mouse-entity (make-ecs-entity)))
    (add-ecs-component-list
     mouse-entity
     (make-point-2d :x (get-mouse-x) :y (get-mouse-y))
     (make-physic-circle :r (get-player-param :r)))
    (do-tagged-ecs-entities (target target-tag)
      (when (collide-entities-p mouse-entity target)
        (return-from find-target-under-mouse target)))))
