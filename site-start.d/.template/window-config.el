;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window system Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system

  ;; 初期フレーム設定
  (setq initial-frame-alist
        (append (list
                 '(width . 118)
                 '(height . 60)
                 '(top . 0)
                 '(left . 580)
                 ) initial-frame-alist))

  )


;;;
;;; EOF
;;;
