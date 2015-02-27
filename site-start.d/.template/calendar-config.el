;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;

;;;; calendar settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;(add-hook 'window-setup-hook 'calendar)                                ; Emacs起動時にカレンダ
(defvar calendar-date-display-form '(year "-" month "-" day))           ; 日付表示形式(ISO standard)
(defvar calendar-time-display-form '(24-hours ":" minutes))             ; 時刻表示形式(24時間表示)
(defvar calendar-week-start-day 1)                                      ; カレンダ開始曜日(月曜日から)
(defvar calendar-weekend '(0 6))                                        ; 週末設定(土日を休日として扱う)

;; 休日設定
(setq holiday-general-holidays nil)
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-other-holidays nil)
(defvar holiday-soloar-holidays nil)
(setq holiday-local-holidays nil)
(load "japanese-holidays")

(add-hook 'today-visible-calendar-hook 'japanese-holiday-mark-weekend)  ; カレンダに当日が表示された場合、週末をマーク


;;;; diary settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defvar diary-file "~/.howm/DIARY")					; ダイアリファイル
(defvar view-diary-entries-initially t)                                 ; カレンダ表示時、ダイアリも表示
(defvar number-of-diary-entries [1 7 6 5 4 3 2])                        ; ダイアリ表示数の指定
(defvar mark-diary-entries-in-calendar t)                               ; ダイアリエントリがある日をマーク

;; ダイアリ解析日付フォーマット
(defvar diary-date-forms
  '(
    (year "-" month "-" day)						; add ISO standard format
    ("\\[" year "-" month "-" day "\\]")				; add howm date format
    (month "/" day "[^/0-9]")
    (month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W")
    ))

(add-hook 'diary-display-hook 'fancy-diary-display)                     ; 装飾日誌表示
(add-hook 'list-diary-entries-hook 'sort-diary-entries)                 ; 日誌記録を時刻順でソート
(add-hook 'list-diary-entries-hook 'include-other-diary-files)		; 装飾日誌表示時に外部日誌ファイルを読み込み
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)		; 〃


;;;
;;; EOF
;;;
