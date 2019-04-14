(defpackage :clw-warp-stg/game/stage/package
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :generate-ws-stage)
  (:import-from :clw-warp-stg/game/stage/test
                :generate-test-stage))
(in-package :clw-warp-stg/game/stage/package)

(defun.ps+ generate-ws-stage (name)
  (ecase name
    (:test (generate-test-stage))))
