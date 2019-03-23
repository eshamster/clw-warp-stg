(defpackage clw-warp-stg/game/state/package
  (:use :cl
        :ps-experiment
        ;; The followings are required to make package-inferred-system to recognize them
        :clw-warp-stg/game/state/global-init
        :clw-warp-stg/game/state/main)
  (:export :init-clw-warp-stg-state)
  (:import-from :cl-web-2d-game
                :init-game-state
                :make-state))
(in-package :clw-warp-stg/game/state/package)

(defun.ps+ init-clw-warp-stg-state ()
  (init-game-state (make-state :global-init)))
