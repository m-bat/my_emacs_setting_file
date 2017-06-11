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



