;; メイリオ・フォントの利用
;; (w32-logfont "メイリオ" 0 22 400 0 nil nil nil 0 1 3 49)
;; の 22 の部分がフォントの大きさ

(w32-add-font
 "fontset-meirio-gothic"
 '((strict-spec
	((:char-spec ascii :height any)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 400 0 nil nil nil 0 1 3 49))
	((:char-spec ascii :height any :weight bold)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 700 0 nil nil nil 0 1 3 49))
	((:char-spec ascii :height any :slant italic)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 400 0 t nil nil 0 1 3 49))
	((:char-spec ascii :height any :weight bold :slant italic)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 700 0 t nil nil 0 1 3 49))
	((:char-spec japanese-jisx0208 :height any)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 400 0 nil nil nil 128 1 3 49))
	((:char-spec japanese-jisx0208 :height any :weight bold)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 700 0 nil nil nil 128 1 3 49)
	 ((spacing . -1)))
	((:char-spec japanese-jisx0208 :height any :slant italic)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 400 0 t nil nil 128 1 3 49))
	((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 700 0 t nil nil 128 1 3 49)
	 ((spacing . -1)))
	((:char-spec katakana-jisx0201 :height any)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 400 0 nil nil nil 128 1 3 49))
	((:char-spec katakana-jisx0201 :height any :weight bold)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 700 0 nil nil nil 128 1 3 49)
	 ((spacing . -1)))
	((:char-spec katakana-jisx0201 :height any :slant italic)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 400 0 t nil nil 128 1 3 49))
	((:char-spec katakana-jisx0201 :height any :weight bold :slant italic)
	 (w32-logfont "MeiryoKe_Gothic" 0 14 700 0 t nil nil 128 1 3 49)
	 ((spacing . -1)))
	) )
 )

;; フォントセット追加
(setq default-frame-alist
	  (append (list
			   '(font . "fontset-meirio-gothic")
			   )
			  default-frame-alist))
