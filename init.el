
;;; package --- summary:

(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; 初期化
(package-initialize)


;; locad-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path))))))

;;elispとconfdirectoryをsubdirectryごとにload-pathに追加
(add-to-load-path "elisp" "conf")

;; (install-elisp "https://www.emacswiki.org/emacs/download/auto-install.el")
(when (require 'auto-install nil t)
  ;;install directoryを設定する 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  ;; いちいちnetworkに繋るため、起動が遅くなる. comment outしておく（現状）
  ;;(auto-install-update-emacswiki-package-name t)
  ;;install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))


;;;emacsの表示に関する設定
;;対応する括弧を強調表示する
										;paren-mode 対応する括弧を強調して表示する
(setq show-paren-delay 0) ;表示までの秒数。初期値は0.125
(show-paren-mode t) ;有効化
										;parenのスタイル expressionは括弧内も強調表示
(setq show-paren-style 'expression)
										;フェイス（後述）を変更
(set-face-background 'show-paren-match-face "yellow")
;;行番号を表示する
(global-linum-mode)
;;インテンドの設定
(setq default-tab-width 4)
										; ホックを使った設定
(defun my-c-c++-mode-init ()
  (setq c-basic-offset 4)
  )
(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; web-modeでのインテンド設定
(setq web-mode-markup-indent-offset 2)


;;インストールしたもの設定
;;(install-elisp "http://www.emacswiki.org/emacs/douwnload/redo+.el")
(when (require 'redo+ nil t)
  ;; global-map
  (global-set-key (kbd "C-x r") 'redo)) ;C-x uにredoを割り当てる
;;(install-auto-complete https://github.com/rdiankov/emacs-config.git)
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;flycheck
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(web-mode php-mode moccur-edit color-moccur undohist anything flycheck-pos-tip))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;package-install anything
;;(require 'anything-config)
(define-key global-map (kbd "C-x b") 'anything) ;anythingをC-x bで開くようにする
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.1
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.1
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)
  (and (equal current-language-environment "Japanese")
       (executable-find "cmigemo")
       (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    ;; M-xによる補完をAnythingで行なう
    ;; (anything-read-string-mode 1)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install))

  (require 'anything-grep nil t))

;;package-install undohist
										;(when (require 'undohist nil t)
										;(undohist-initialize))

;; package-install undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; package-install color-moccur
(when (require 'color-moccur nil t)
  ;;グローバルマップにoccur-by-moccurを割り当て
  (define-key global-map (kbd "C-q") 'occur-by-moccur))

;;tramp
(require 'tramp)						;sshでサーバにあるファイルを直接変更できる
(setq tramp-default-method "ssh")

;;package-install php-mode
(require 'php-mode)

;; web mode
;; http://web-mode.org/
;; http://yanmoo.blogspot.jp/2013/06/html5web-mode.html

(setq auto-mode-alist
      (append '(
                ("\\.\\(html\\|xhtml\\|shtml\\|tpl\\)\\'" . web-mode)
                ("\\.php\\'" . php-mode)
                )
              auto-mode-alist))

;;==========================================================
;;         web-modeの設定
;;==========================================================
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; 変更日時の自動修正
  (setq time-stamp-line-limit -200)
  (if (not (memq 'time-stamp write-file-hooks))
      (setq write-file-hooks
            (cons 'time-stamp write-file-hooks)))
  (setq time-stamp-format " %3a %3b %02d %02H:%02M:%02S %:y %Z")
  (setq time-stamp-start "Last modified:")
  (setq time-stamp-end "$")
  ;; web-modeの設定
  (setq web-mode-markup-indent-offset 2) ;; html indent
  (setq web-mode-css-indent-offset 2)    ;; css indent
  (setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..)
  ;; htmlの内容をインデント
  ;; TEXTAREA等のä¸­身をインデントすると副作用が起こったりするので
  ;; デフォルトではインデントしない
  ;;(setq web-mode-indent-style 2)
  ;; コメントのスタイル
  ;;   1:htmlのコメントスタイル(default)
  ;;   2:テンプレートエンジンのコメントスタイル
  ;;      (Ex. {# django comment #},{* smarty comment *},{{-- blade comment --}})
  (setq web-mode-comment-style 2)
  ;; 終了タグの自動補完をしない
  ;;(setq web-mode-disable-auto-pairing t)
  ;; color:#ff0000;等とした場合に指定した色をbgに表示しない
  ;;(setq web-mode-disable-css-colorization t)
  ;;css,js,php,etc..の範囲をbg色で表示
  ;; (setq web-mode-enable-block-faces t)
  ;; (custom-set-faces
  ;;  '(web-mode-serer-face
  ;;    ((t (:background "grey"))))                  ; template Blockの背景色
  ;;  '(web-mode-css-face
  ;;    ((t (:background "grey18"))))                ; CSS Blockの背景色
  ;;  '(web-mode-javascript-face
  ;;    ((t (:background "grey36"))))                ; javascript Blockの背景色
  ;;  )
  ;;(setq web-mode-enable-heredoc-fontification t)
  )
(add-hook 'web-mode-hook  'web-mode-hook)
;; 色の設定
(custom-set-faces
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46"))))                          ; doctype
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))             ; 要素名
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))                          ; 属性名など
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))                          ; 属性値
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))                          ; コメント
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))                          ; コメント
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))                          ; cssのタグ
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))                          ; css 疑似クラス
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))                          ; cssのタグ
 )


