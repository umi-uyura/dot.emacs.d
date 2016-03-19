;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customize Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 環境識別子定義 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;; バージョン識別子

;; メジャーバージョン
(defvar run-emacs24 (equal emacs-major-version 24))

;;;; プラットフォーム識別子

;; Mac OS X/GNU-Darwin
(defvar run-darwin (equal system-type 'darwin))

;; Unix/Linux
(defvar run-unix (or (equal system-type 'gnu)
                     (equal system-type 'gnu/linux)
                     (equal system-type 'gnu/kfreebsd)
                     (equal system-type 'usg-unix-v)))

;; Windows
(defvar run-windows (or (equal system-type 'windows-nt)
                        (equal system-type 'ms-dos)
                        (equal system-type 'cygwin)))

;; CLI
(defvar run-cli (not window-system))


;;;; パッケージ識別子

;; Cocoa Emacs (Mac OS X)用
(defvar run-cocoa-emacs (featurep 'ns))


;;;; Emacs-Cask ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'cask)
(cask-initialize)


;;;; パス定義 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; .emacs関連パス定義
(defconst DOT_EMACS_HOME (concat (getenv "HOME") "/.emacs.d"))
(defconst DOT_EMACS_SITE_START_D (concat DOT_EMACS_HOME "/site-start.d"))
(defconst DOT_EMACS_LISP (concat DOT_EMACS_HOME "/lisp"))
(defconst DOT_EMACS_ELISP (concat DOT_EMACS_HOME "/elisp"))
(defconst DOT_EMACS_SITE_LISP (concat DOT_EMACS_HOME "/site-lisp"))
(defconst DOT_EMACS_ETC (concat DOT_EMACS_HOME "/etc"))
(defconst DOT_EMACS_VAR (concat DOT_EMACS_HOME "/var"))
(defconst DOT_EMACS_CUSTOM_D (concat DOT_EMACS_HOME "/custom.d"))


;; ;; Lispパス追加
(when run-emacs24
  ;; load-pathに指定したディレクトリ（サブディレクトリを含んで）追加する関数
  (defun add-to-load-path (&rest paths)
    (let (path)
      (dolist (path paths paths)
        (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
          (add-to-list 'load-path default-directory)
          (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
              (normal-top-level-add-subdirs-to-load-path))))))

  (add-to-load-path "/site-start.d"
                    "/lisp"
                    "/elisp"
                    "/site-lisp"
                    )
)

;; exec-pathに追加
(add-to-list 'exec-path (concat DOT_EMACS_HOME "/bin"))

;; シェルから環境変数を読み込む
(exec-path-from-shell-initialize)

;; PATHにexec-pathを追加
(setenv "PATH" (mapconcat 'identity exec-path ":"))


;;;; auto-install定義 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defvar auto-install-directory (concat DOT_EMACS_ELISP "/"))    ; 最後にスラッシュが必要・・・

(when (require 'auto-install nil t)
  (setq auto-install-use-wget t)
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Language Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-language-environment "Japanese")           ; 日本語環境設定

;; Mac OS X用 ------------------------------------
(when run-darwin

  (prefer-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  (setq default-input-method "MacOSX")

  ;; Google日本語入力連携
  (mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" `title "あ")

  ;; '\' を優先
  ;(mac-translate-from-yen-to-backslash)

  ;; minibuffer 内は英数モードにする
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
)
;; -------------------------------------Mac OS X用


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)                ; 起動時画面非表示

;; ユーザ情報
(load "user-config")

;; エディタ関連
(setq scroll-conservatively 1)                  ; スクロール量
(setq-default transient-mark-mode t)            ; 一時マークモードの自動有効化
(delete-selection-mode 1)                       ; リージョン選択時、置換入力有効
(blink-cursor-mode 0)                           ; カーソルを点滅させない
(show-paren-mode t)                             ; 対応する括弧の強調表示
(global-hl-line-mode t)                         ; 現在行をハイライト
(setq-default truncate-lines nil)               ; 折り返しあり
(setq-default tab-width 4)                      ; タブ幅
(setq-default indent-tabs-mode nil)             ; インデントにタブ文字を使用しない
(setq next-line-add-newlines nil)               ; バッファ最終行でnext-line時、行挿入抑止
(setq visible-bell t)                           ; エラー時、画面をフラッシュ
(fset 'yes-or-no-p 'y-or-n-p)                   ; "yes or no"を"y or n"にする
(setq read-buffer-completion-ignore-case t)     ; バッファー絞り込み時に大文字小文字の区別をしない

;; 行の扱い (Emacs 22.xの場合、screen-lines.elを利用)
(when run-emacs24
  (setq line-move-visual t)                     ; カーソル移動：論理行
)

;; タイトルバー
(setq frame-title-format
      `("\"%b\" on " invocation-name "@" system-name))  ; "バッファ名" on アプリ名@システム名

;; モードライン
(line-number-mode t)                            ; 行番号表示
(column-number-mode t)                          ; 桁番号表示
(defvar display-time-24hr-format t)             ; 時刻表示(24時間表記)

;; ミニバッファ
(setq resize-mini-windows `grow-only)           ; 必要に応じて拡縮

; 自動保存機能無効
(setq auto-save-list-file-name nil)             ; 自動保存情報を保存するファイル：なし
(setq auto-save-list-file-prefix nil)           ; 自動保存時に付与する接頭辞：なし

(setq make-backup-files nil)                    ; バックアップファイル("...~")の作成無効

;; その他
(setq yank-excluded-properties t)               ; テキストの追加属性削除
(ffap-bindings)                                 ; Find-File拡張

;; Mac OS X用 ------------------------------------
(when run-darwin
  (defvar browse-url-generic-program "/usr/bin/open")     ; URLオープン設定
)
;; -------------------------------------Mac OS X用


;;;; IMEの設定 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;



;;;; 略語展開関連 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;; abbrev - 静的略語展開

;; 省略形テーブル保存先
(setq abbrev-file-name (concat DOT_EMACS_VAR "/abbrev/abbrev_defs"))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keymap Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; キーバインド変更
(define-key global-map (kbd "C-h") 'delete-backward-char)           ; Back Space
(define-key global-map (kbd "C-x ?") 'help-command)                 ; ヘルプ
(define-key global-map (kbd "C-/") 'undo)                           ; Undo
(define-key global-map (kbd "C-c t") 'toggle-truncate-lines)        ; 行折り返しのON/OFF
(define-key global-map (kbd "M-n") 'next-error)                     ; 次のエラー行へジャンプ
(define-key global-map (kbd "M-p") 'previous-error)                 ; 前のエラー行へジャンプ
(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)         ; インクリメンタルサーチ中でもキーワード削除可能


;; 分割したウィンドウ間をShift+カーソルキーで移動可能とする
(windmove-default-keybindings)

;; Mac OS X用 ------------------------------------
(when run-darwin

  ;; キーバインド変更
  (define-key global-map (kbd "C-_") 'toggle-input-method)          ; IME切り替え
  (define-key global-map (kbd "C-M-_") 'indent-region)              ; 選択範囲のインデント(WindowsでのC-M-\に合わせる)
  (define-key global-map (kbd "M-¥") [?\\])                         ; '\'を入力
  (define-key global-map (kbd "C-c o o") 'browse-url-default-macosx-browser)	; MacのデフォルトブラウザでURLを開く

  ;; command/optionキーの変更
  ;; ※標準キー配置からcommandキーとoptionキーを入れ替えている前提 (for HHKL2)
  (setq mac-option-modifier 'meta)      ; option => meta(command)
  (setq mac-command-modifier 'super)    ; command => option

  ;; 日本語入力時でもショートカットキーが使えるようにする。
  (defvar mac-pass-control-to-system nil)
  (defvar mac-pass-command-to-system nil)
  (defvar mac-pass-option-to-system nil)

)
;; -------------------------------------Mac OS X用


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Command Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mac OS X用 ------------------------------------
(when run-darwin

  ;; grep設定
  (setq grep-program "lgrep")
  (setq grep-command (format "%s -n -Ku8 -Ia -Ou8 -Du8 " grep-program))
  (setq grep-find-command
        (format "%s . -name '*.*' -type f -print0 | xargs -0 %s" find-program grep-command))

)
;; -------------------------------------Mac OS X用


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window system Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when window-system

  ;; Rictyフォント設定
  (set-face-attribute 'default nil
                      :family "Ricty Discord"
                      :height 140)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (cons "Ricty Discord" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0212
                    (cons "Ricty Discord" "iso10646-1"))
  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    (cons "Ricty Discord" "iso10646-1"))

  ;; Emojiフォント設定
  (set-fontset-font
   t 'symbol
   (font-spec :family "Apple Color Emoji") nil 'prepend)

;; Mac OS X用 ------------------------------------
(when run-darwin

  ;; 初期フレーム設定
  (load "window-config")

  ;; デフォルトフレーム設定（初期フレームを含む全てのフレームへ影響）
  (setq default-frame-alist
        (append (list
                 '(line-spacing . 0)            ; 行間
                 '(tool-bar-lines . 0)          ; ツールバー非表示
                 ) default-frame-alist))
)
;; -------------------------------------Mac OS X用


;; フォント色設定
(set-face-foreground 'font-lock-comment-face "darkgreen")           ; コメント
(set-face-foreground 'font-lock-comment-delimiter-face "darkgreen") ; コメントの区切り
(set-face-foreground 'font-lock-doc-face "darkgreen")               ; ドキュメント（複数行コメント）
(set-face-foreground 'font-lock-string-face  "magenta")             ; 文字列
(set-face-foreground 'font-lock-keyword-face "blue")                ; キーワード
(set-face-foreground 'font-lock-function-name-face "blue")          ; 関数
(set-face-bold 'font-lock-function-name-face t)                     ; 関数（ボールド指定）
(set-face-foreground 'font-lock-variable-name-face "DodgerBlue")    ; 変数
(set-face-foreground 'font-lock-type-face "blue")                   ; 型、クラス
(set-face-foreground 'font-lock-builtin-face "purple")              ; 組み込み関数
(set-face-foreground 'font-lock-constant-face "purple")             ; 定数、ラベル
(set-face-foreground 'font-lock-warning-face "blue")                ; 警告
(set-face-bold 'font-lock-warning-face nil)                         ; 警告（ボールド解除）
(set-face-foreground 'font-lock-preprocessor-face "red")            ; プリプロセッサ

;; リージョン（暫定マーク）色設定
;(set-face-foreground 'region "white")                              ; 前景色
(set-face-background 'region "lightgray")                           ; 背景色

;; モードライン色設定
;(set-face-foreground 'modeline "snow")                             ; 前景色
;(set-face-background 'modeline "black")                            ; 背景色

;; フレーム透過設定
(set-frame-parameter (selected-frame) 'alpha '(100 60))

)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; package.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(when (require 'package nil t)
  ;; 追加パッケージリポジトリサイト
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  ;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))

  ;; 初期化
  (package-initialize)
)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;(load-theme 'tango-dark t)
;(load-theme 'solarized-dark t)
;(load-theme 'alect-dark t)

(load-theme 'ample-zen t)
(custom-theme-set-faces 'ample-zen
                        '(region ((t (:background "#4c4c4c"))))     ; 選択範囲
                        )


;;;; emacs server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Mac OS X用 ------------------------------------
(when run-darwin
  (server-start)
)
;; -------------------------------------Mac OS X用


;;;; edit-server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(when (locate-library "edit-server")
  (defvar edit-server-new-frame nil)
  (edit-server-start))


;;;; sudo-ext.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Require:
;; use emacs server (or gnuserver)
;;
;; Usage:
;;
;; M-x sudoedit ... root権限でファイルを編集する
;; C-x # ... sudoeditの編集を終了
;;
;; M-! sudo <command> ... <command>をroot権限で実行
;;
;; Remark:
;;   動かない場合、tramp.elを利用する方法がある。
;;   /sudo:root@localhost:/etc/apache2/httpd.conf
;;

(require 'sudo-ext)


;;;; cua-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 矩形編集機能のみ使う ※(C-RET) で開始
;;

(cua-mode t)
(setq cua-enable-cua-keys nil)        ; CUAキーバインドは無効


;;;; diff/ediff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; unified diff形式
(setq diff-switches "-u")

;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;;; icomplete-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;(icomplete-mode t)


;;;; tempbuf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'magit-mode-hook 'turn-on-tempbuf-mode)


;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(global-undo-tree-mode t)
(define-key global-map (kbd "C-M-/") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-_") nil)


;;;; rainbow-delimiters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 色を濃い目に
;; Refer: http://d.hatena.ne.jp/murase_syuka/20140815/1408061850
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))


;;;; smart-cursor-color ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(smart-cursor-color-mode 1)


;;;; semantic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; キャッシュ作成先
(defvar semanticdb-default-save-directory (concat DOT_EMACS_VAR "/semantic"))


;;;; uniquify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バッファ名をユニークなものにする
;;

(require 'uniquify)

;; filename<dir>形式にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; *〜*に囲まれたバッファは除く
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;;; bookmark.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defvar bookmark-file (concat DOT_EMACS_VAR "/emacs.bmk"))
(defvar bookmark-save-flag 1)                                         ; 更新時に即保存
(defvar bookmark-sort-flag nil)                                       ; ソート無効（超整理法のため）

;; 最後に選んだものをリストの上にする。超整理法
;; see: http://cgi.NetLaputa.ne.jp/~kose/diary/?200209b&to=200209124
(defadvice bookmark-jump (after bookmark-jump-after activate)
  "最後に選んだものをリストの上にする。超整理法!"
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))


;;;; recentf-ext.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイル履歴管理拡張
;;

(custom-set-variables
 '(recentf-save-file (concat DOT_EMACS_VAR "/recentf"))
 '(recentf-max-saved-items 500)
 '(recentf-exclude '("^/[^/:]+:"             ; Trampリモートファイル形式を除外
                     "!semantic.cache$"))    ; Semanticキャッシュファイルを除外
 )

;(require 'recentf-ext)
;(define-key global-map (kbd "C-x F") 'recentf-open-files)           ; 最近使用したファイルから開く


;;;; helm-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(helm-mode 1)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x F") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-x F") 'helm-recentf)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; kill-bufferはhelmを使わない
(add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

; バッファ名を長く表示
(setq helm-buffer-max-length 50)


;;;; color-moccur.el + moccur-edit.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (install-elisp-from-emacswiki "moccur-edit.el")
;; color-moccur.el is Cask install
;;

(require 'color-moccur)
;(require 'moccur-edit)

(setq moccur-split-word t);             ; 複数単語指定（スペース区切り）：有効

(define-key global-map (kbd "C-x m") 'occur-by-moccur)
(define-key global-map (kbd "C-x M") 'moccur)


;;;; igrep.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'igrep)

(setq igrep-program "/usr/local/bin/lgrep")             ; コマンドをlgrepに変更
;(setq igrep-options nil)
(setq igrep-regex-option "-Ku8 -Ia -Ou8 -Du8")
(setq igrep-find-use-xargs nil)                         ; xargsの使用：未使用（lgrepが未対応のため）
(setq igrep-use-zgrep nil)

(define-key global-map (kbd "C-x g") 'igrep)            ; igrep
(define-key global-map (kbd "C-x G") 'igrep-find)       ; igrep-find


;;;; grep-edit.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (install-elisp-from-emacswiki "grep-edit.el")
;;
;; Usage:
;;   C-c C-e : apply the highlighting changes to files.
;;             * Save is C-x s(save-some-buffers).
;;   C-c C-u : abort
;;   C-c C-r : Remove the highlight in the region (The Changes doesn't apply to files)
;;

;; (require 'grep-edit)


;;;; grep-a-lot.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Usage:
;;   M-g = : 現在のgrepバッファを開く
;;   M-g ] : 次のgrepバッファを開く
;;   M-g [ : 前のgrepバッファを開く
;;   M-g - : 現在のgrepバッファを削除する
;;   M-g _ : 全grepバッファを削除する
;;

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; igrep.elを使用する場合
(grep-a-lot-advise igrep)


;;;; ac-emoji ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'markdown-mode-hook 'ac-emoji-setup)
(add-hook 'git-commit-mode-hook 'ac-emoji-setup)


;;;; jaword ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'jaword)
(global-jaword-mode)


;;;; open-junk-file.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'open-junk-file)
(setq open-junk-file-format (concat DOT_EMACS_VAR "/junk/%Y-%m-%d-%H%M%S."))
(define-key global-map (kbd "C-c C-j") 'open-junk-file)             ; ジャンクファイル作成


;;;; relative-buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(global-relative-buffers-mode)


;;;; Projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(projectile-global-mode)

(defconst PROJECTILE_TEMP (concat DOT_EMACS_VAR "/projectile"))

(custom-set-variables
 '(projectile-cache-file (concat PROJECTILE_TEMP "/projectile.cache"))
 '(projectile-known-projects-file (concat PROJECTILE_TEMP "/projectile-bookmarks.eld"))
 )


;;;; for Titanium develop

;; プロジェクトルートを識別するファイルを追加
(add-to-list 'projectile-project-root-files "tiapp.xml")
(add-to-list 'projectile-project-root-files "timodule.xml")

;; 関連するファイルの紐付け
(add-to-list 'projectile-other-file-alist '("js" . ("jade" "styl")))
(add-to-list 'projectile-other-file-alist '("jade" . ("js" "styl" "xml")))
(add-to-list 'projectile-other-file-alist '("styl" . ("js" "jade" "tss")))
(add-to-list 'projectile-other-file-alist '("xml" . ("jade")))
(add-to-list 'projectile-other-file-alist '("tss" . ("styl")))


;;;; osx-dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(global-set-key (kbd "C-c i d") 'osx-dictionary-search-pointer)
(global-set-key (kbd "C-c i D") 'osx-dictionary-search-input)


;;;; mac-open-folder.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カレントディレクトリをFinder/Terminal/iTerm2で開く (for Mac OS X)
;;

(when run-darwin
  (load "mac-open-folder")
  (define-key global-map (kbd "C-c o f") 'open-finder)
  (define-key global-map (kbd "C-c o t") 'open-terminal)
  (define-key global-map (kbd "C-c o i") 'open-iterm)
)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; major-mode Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 新しいバッファのデフォルトメジャーモード
(setq-default major-mode 'text-mode)


;;;; calendar and diary settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(load "calendar-config")


;;;; shell-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Mac OS X用 ------------------------------------
(when run-darwin
  (defun osx-shell-mode-hooks()
    (set-buffer-process-coding-system 'utf-8-hfs 'utf-8-hfs)
    ;(set-buffer-file-coding-system 'utf-8-hfs)
    )
  (add-hook 'shell-mode-hook 'osx-shell-mode-hooks)
  )
;; -------------------------------------Mac OS X用


;; shell-commandで補完を有効にする
(require 'shell-command)
(shell-command-completion-mode)

;; shell-modeでANSIカラーを有効にする
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;; dired-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'dired-x)

;; GNU coreutilsのls(gls)を使う
(when run-darwin
  (let ((gls "/usr/local/bin/gls"))
        (if (file-exists-p gls) (setq insert-directory-program gls)))
)

;; wdired
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)           ; ファイル名編集モード

; 2つのdiredバッファを開いている場合、移動/コピーの対象がもう一方のバッファになる
(setq dired-dwim-target t)

; インクリメンタルサーチはファイル名だけ対象
(setq dired-isearch-filenames t)


;;;; generic settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Windows向け、Unix向け両方読み込む
(setq generic-define-mswindows-modes t)
(setq generic-define-unix-modes t)

(require 'generic-x)


;;;; shell-script-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun my-shell-script-mode-hooks()
  (setq sh-basic-offset 2
        sh-indentation 2)
  )
(add-hook 'sh-mode-hook 'my-shell-script-mode-hooks)


;;;; cc-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; cc-mode共通フック
(defun my-c-mocde-common-hook ()
  (c-set-style "stroustrup")                                    ; コーディングスタイル
  (setq c-basic-offset 4)                                       ; 基本オフセット
  (setq c-auto-newline nil)                                     ; 全自動インデント機能：無効
;  (c-toggle-auto-state)                                        ; 自動インデント機能切り替え：有効
  (c-toggle-hungry-state)                                       ; 左方向空白の一括削除切り替え：有効
;  (setq completion-mode nil)                                   ; タブ‐スペース展開：無効
;  (indent-tabs-mode nil)                                       ; TABキーでインデント（スペース入力）：無効
;  (setq c-tab-always-indent nil)                               ; TABキーでインデント（スペース入力）実施：無効
  (setq comment-empty-lines t)                                  ; 複数行コメント化時、空行も対象とする：有効
;  (setq comment-style 'multi-line)                             ; コメントスタイル
  (which-function-mode t)                                       ; 現在位置の関数名を表示するマイナーモード（see: which-func-mode settings)
  (which-func-show-header)
  ;; キーバインド
  (define-key c-mode-base-map [f1] 'manual-entry)               ; カーソル下の単語のman
;;   (define-key c-mode-base-map [f4] 'next-error)                  ; 次のエラー行へジャンプ（VC++風）
;;   (define-key c-mode-base-map [(shift f4)] 'previous-error)      ; 前のエラー行へジャンプ（VC++風）
  )
(add-hook 'c-mode-common-hook 'my-c-mocde-common-hook)

;; c/c++-mode用フック
(defun my-c/c++-mode-hook ()
  (c-set-style "stroustrup")                                    ; コーディングスタイル
  (setq tab-width 4)                                            ; タブ幅
  (setq c-basic-offset tab-width)                               ; インデント幅
  (setq c-tab-always-indent nil)                                ; TABキーでインデント（スペース入力）実施：無効
  (setq completion-mode nil)                                    ; タブ‐スペース展開：無効
  )
(add-hook 'c-mode-hook 'my-c/c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c/c++-mode-hook)

;; *.hもc++モードで開く
(add-to-list 'auto-mode-alist '("\\.[Hh]$" . c++-mode))


;;;; ruby-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)

;; ruby-end
(require 'ruby-end)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (abbrev-mode 1)
             (electric-pair-mode t)
             (electric-indent-mode t)
             (electric-layout-mode t)))

;; inf-ruby.el
(autoload 'run-ruby "run-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))


;;;; js2(javascript)-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'js2-mode "js2-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))    ; for JSX
(add-to-list 'auto-mode-alist '("\\.jmk" . js2-mode))           ; for Appcelerator Alloy JS Makefile

(add-hook 'js2-mode-hook
          '(lambda ()
             (setq tab-width 2
                   js2-basic-offset 2
                   indent-tabs-mode nil)
             ))

;; js2-modeの文法チェックを無効
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)
;; (setq js2-strict-trailing-comma-warning nil)
;; (setq js2-strict-missing-semi-warning nil)              ; 過剰な"missing ; after statement"を抑制(for .json)
;; (setq js2-strict-inconsistent-return-warning nil)
;; (setq js2-strict-cond-assign-warning nil)
;; (setq js2-strict-var-redeclaration-warning nil)
;; (setq js2-strict-var-hides-function-arg-warning nil)
;; (setq js2-missing-semi-one-line-override nil)


;; for shell scripts running via node.js
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; for JavaScript linting
(add-hook 'js-mode-hook 'js2-minor-mode)

;; auto-complete
; "File mode specification error: (invalid-function ac-define-source)" が出るため、一時的にコメントアウト
;; (add-hook 'js2-mode-hook 'ac-js2-mode)


;;;; tern-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'js-mode-hook
          '(lambda ()
             (when (locate-library "tern")
               (setq tern-command '("tern" "--no-port-file"))
               (tern-mode t)
               (eval-after-load 'tern
                 '(progn
                    (require 'tern-auto-complete)
                    (tern-ac-setup))))))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


;;;; json-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'json-mode "json-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.json" . json-mode))
(add-to-list 'auto-mode-alist '("\\.tss" . json-mode))      ; for Appcelerator Alloy

(add-hook 'json-mode-hook
          '(lambda ()
             (setq js-indent-level 2
                   indent-tabs-mode nil)
             ))

;; json-reformat customize
(custom-set-variables
 '(json-reformat:indent-width 2))


;;;; whitespace-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; automatically clean up bad whitespace when file is save
(setq whitespace-action '(auto-cleanup))

;; only show bad whitespace
(setq whitespace-style '(trailing
                         indentation
                         empty
                         space-before-tab
                         space-after-tab
                         ))

(global-whitespace-mode 1)


;;;; coffee-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(custom-set-variables
 '(coffee-tab-width 2)
 '(coffee-args-compile '("-c" "--bare")))

(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)))


;;;; nxml-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'nxml-mode)
(add-to-list 'auto-mode-alist '("\\.xml" . nxml-mode))

(custom-set-faces
 '(nxml-text-face                           ; テキスト
   ((t (:foreground "black" :background "#e5e5e5"))))
 '(nxml-comment-content-face                ; コメント
   ((t (:foreground "darkgreen"))))
 '(nxml-comment-delimiter-face              ; ＜!-- --＞
   ((t (:foreground "darkgreen"))))
 '(nxml-delimiter-face                      ; ＜＞ ＜? ?＞
   ((t (:foreground "blue"))))
 '(nxml-delimited-data-face                 ; 属性値やDTD引数値など
   ((t (:foreground "magenta"))))
 '(nxml-attribute-value-delimiter-face      ; ""
   ((t (:foreground "magenta"))))
 '(nxml-element-local-name-face             ; 要素名
   ((t (:inherit nxml-name-face :foreground "blue"))))
 '(nxml-name-face                           ; 属性名など
   ((t (:foreground "purple"))))
 '(nxml-element-colon-face                  ; :(xsl:paramなど)
   ((t (:foreground "black"))))
 '(nxml-ref-face                            ; &lt;など
   ((t (:foreground "red"))))
 '(nxml-tag-slash-face                      ; 終了タグのスラッシュ
   ((t (:inherit nxml-name-face :foreground "blue" :bold t))))
 )


;;;; web-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq web-mode-enable-current-element-highlight t)    ; 現在のHTMLエレメントを強調する
  (setq web-mode-enable-current-column-highlight t)     ; 現在の縦列を強調する
  (lambda ()
    (when (equal web-mode-content-type "jsx")
      (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))))
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; auto-complete
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (
                         ;; attribute-value better to be first
                         ac-source-html-attribute-value
                         ac-source-html-tag
                         ac-source-html-attribute)))


;;;; jade-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'sws-mode)
(require 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))


;;;; css-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq css-indent-offset 2)


;;;; less-css-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'less-css-mode-hook 'ac-css-mode-setup)


;;;; rainbow-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'rainbow-mode)
(add-hook 'web-mode-hook 'rainbow-mode)
;; (add-hook 'javascript-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'less-css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
;; (add-hook 'php-mode-hook 'rainbow-mode)


;;;; markdown-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . gfm-mode) auto-mode-alist))

(setq markdown-command "multimarkdown")
(setq markdown-open-command "openmacdown")
(setq markdown-indent-on-enter nil)

;; Hooks
(defun my-markdown-mode-hook ()
  (set (make-local-variable 'whitespace-action) nil)    ; 自動空白除去を無効
  )
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)


;;;; writeroom-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'writeroom-mode)


;;;; yaml-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;;;; objc-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base: cc-mode
;;

(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

;; Magicコメント
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; Hooks
(defun my-objc-mode-hook ()
  (c-set-style "stroustrup")                        ; コーディングスタイル
  (setq c-basic-offset 2)                           ; インデント幅
  (indent-tabs-mode t)                              ; TABキーでインデント（スペース入力）：有効
  (setq c-tab-always-indent t)                      ; TABキーでインデント（スペース入力）実施：有効
  )
(add-hook 'objc-mode-hook 'my-objc-mode-hook)


;;;; sql-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq sql-sqlite-program "sqlite3")


;;;; php-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'php-mode "php-mode")
(add-to-list 'magic-mode-alist '("<%php" . php-mode))

(add-hook 'php-mode-hook
    '(lambda()
       (setq php-manual-url "http://www.php.net/manual/ja/")
       ))


;;;; url settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; ※ewwのキャッシュディレクトリを変更したい…が、うまくいかない？
(custom-set-variables
 '(url-configuration-directory (concat DOT_EMACS_VAR "/url"))
 '(url-cache-directory (concat DOT_EMACS_VAR "/url/cache"))
 '(url-cookie-file (concat DOT_EMACS_VAR "/url/cookies"))
 )


;;;; eww settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'eww)

;; browse-urlはewwで開く
(when (fboundp 'eww)
  (setq browse-url-browser-function 'eww-browse-url)
  )

;; 画像表示を抑制
(setq shr-inhibit-images t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; minor-mode Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; scratch-re-create.el settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(load "scratch-re-create")


;;;; YASnippet  settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'yasnippet)

;; スニペット配置
(setq yas-snippet-dirs
      '("~/.emacs.d/etc/snippets"                   ;;  (concat DOT_EMACS_ETC "/snippets")
        "~/.emacs.d/site-lisp/yasnippet/snippets"   ;;  (concat DOT_EMACS_SITE_LISP "/yasnippet/snippets")
        yas-installed-snippets-dir
        ))

;; yas起動
(yas-global-mode 1)


;; キーバインド
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-c C-s i") 'yas-insert-snippet)


;;;; auto-complete settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'auto-complete-config)
(ac-config-default)

(dolist (mode '(git-commit-mode
                markdown-mode
                gfm-mode
                fundamental-mode
                text-mode))
  (add-to-list 'ac-modes mode))

(custom-set-variables
 '(ac-comphist-file (concat DOT_EMACS_VAR "/ac-comphist.dat"))
 '(ac-auto-start 3)
 '(ac-quick-help-delay 1.0)
 '(ac-use-fuzzy t)
 )


;;;; flycheck settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-hook 'after-init-hook 'global-flycheck-mode)

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs)))

(with-eval-after-load 'flycheck
  (defconst flycheck-error-list-format [("Line" 4 flycheck-error-list-entry-< :right-align t)
                                        ("Col" 3 nil :right-align t)
                                        ("Level" 8 flycheck-error-list-entry-level-<)
                                        ("ID" 20 t)
                                        ("Message" 0 t)
                                        (" (Checker)" 8 t)])
  (flycheck-pos-tip-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              (flycheck-mode))))

(add-hook 'js2-jsx-mode-hook
          (lambda ()
            (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
            (flycheck-mode)))


;;;; howm settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(load "howm-config")


;;;; memo-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'memo-mode "memo-mode" "Memo mode" t)
(autoload 'memo-new-entry "memo-mode" "Memo mode" t)

(setq memo-mode-hook 'turn-on-auto-fill)
(setq memo-mode-hook 'howm-mode)
;; (add-hook 'memo-mode-hook
;;        '(lambda()
;;           (howm-mode)
;;           ))

(setq memo-indent-level 4)                                              ; インデントレベル
(setq memo-item-default "* ")                                           ; メモアイテム既定形式
(setq memo-entry-separated-tag "^=\\( +\\(.*\\)\\|\\)$")                ; セパレータ
(setq memo-fill-column 120)                                             ; フィルモードカラム数


;;;; magit-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'magit)

(global-set-key (kbd "C-c m s") 'magit-status)         ; git status


;;;; dsvn settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(with-eval-after-load "dsvn"
  (setenv "LC_ALL" "ja_JP.UTF-8"))


;;;; nyan-mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(nyan-mode 0)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; External services Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; google-translate settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(require 'google-translate)
(require 'google-translate-default-ui)

(setq google-translate-default-source-language "en")    ;; 翻訳元のデフォルト
(setq google-translate-default-target-language "ja")    ;; 翻訳先のデフォルト

;; 翻訳パターンリスト
(setq google-translate-translation-directions-alist
      '(
        ("en" . "ja")
        ("ja" . "en")
        ))

(global-set-key (kbd "C-c g t") 'google-translate-at-point)
(global-set-key (kbd "C-c g T") 'google-translate-smooth-translate)


;;;; gmail-message settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(custom-set-variables
 '(ham-mode-markdown-to-html-command
   (quote
    ; GitHub flavored Markdownを使う
    ("/usr/local/bin/pandoc" "--from" "markdown_github" "--to" "html" "--email-obfuscation" "none" file)))
 )


;;;; dash-at-point settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-to-list 'dash-at-point-mode-alist
             '(css-mode . "css,bootstrap,foundation,less,awesome,cordova,phonegap,titanium"))
(add-to-list 'dash-at-point-mode-alist '(jade-mode . "jade,titanium"))
(add-to-list 'dash-at-point-mode-alist '(js2-mode . "javascript,backbone,angularjs,nodejs,titanium"))
(add-to-list 'dash-at-point-mode-alist '(stylus-mode . "css,stylus,titanium"))

(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c e") 'dash-at-point-with-docset)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customize Configration
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat DOT_EMACS_HOME "/custom.el"))
(if (file-exists-p (expand-file-name custom-file))
    (load (expand-file-name custom-file) t nil nil))


;;;
;;; EOF
;;;

(provide 'init)
;;; init.el ends here
