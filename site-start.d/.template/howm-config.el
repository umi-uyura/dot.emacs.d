;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;

;;;; howm settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq howm-menu-lang 'ja)
(require 'howm)

; メモはmemo-modeで開く
(add-to-list 'auto-mode-alist '("\\.[Hh][Oo][Ww][Mm]$" . memo-mode))

; howmディレクトリ
(setq howm-directory "~/.howm/")

; howmキーワードファイル
(setq howm-keyword-file (format "%s/%s" howm-directory ".howm-keys"))

; ファイルフォーマット（1日1ファイル）
(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

; ToDoメニュー表示対象（"."を排除）
(setq howm-todo-menu-types "[-+~!]")

; 検索履歴を残さない
(setq howm-history-limit 0)

; 一覧のソート基準：日付順
(setq howm-normalizer 'howm-sort-items-by-reverse-date)

; 一覧からメモを開いた場合、一覧バッファを削除（C-u Enterで残す）
;(setq howm-view-summary-persistent nil)

; スイッチ
(setq action-lock-switch-default '("{ }" "{*}" "{-}" "{O}" "{X}" "{?}"))

;; 予定表表示範囲
(setq howm-menu-schedule-days-before 1)                 ; 1日前から
(setq howm-menu-schedule-days 14)                       ; 2週間後まで

;; homwバッファは隠しバッファ（非表示にする）
(setq howm-menu-name-format " *howmM:%s*")              ; メニュー
(setq howm-view-summary-name " *howmS:%s*")             ; 一覧
(setq howm-view-contents-name " *howmC:%s*")            ; 内容

;; 一覧バッファの色付け
(setq howm-view-font-lock-keywords-personal
      '(
        ("^= .*$" . 'howm-mode-title-face)
        ("^\* .*$" . 'howm-mode-wiki-face)
        ))

;; howmメニューで"d"でcalendar起動
(add-hook 'howm-menu-hook
          '(lambda ()
             (local-set-key "d" 'calendar)
             ))

;; キーバインド
(global-set-key "\C-c,," 'howm-menu)
(add-hook 'howm-mode-on-hook
          '(lambda()
             (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
             (define-key howm-mode-map [(shift tab)] 'action-lock-goto-previous-link)
             (yas/minor-mode-on)        ; Yasnippetと連携
             ))

;;;
;;; EOF
;;;
