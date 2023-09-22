;; -*- lexical-binding:t -*-

;; fontset
(add-hook 'after-make-frame-functions
          (progn
            (set-fontset-font "fontset-default" 'han "Noto Sans CJK SC")
            (set-fontset-font "fontset-default" 'cjk-misc "Noto Sans CJK SC"))

(custom-set-variables
 '(auto-fill-function 'do-auto-fill)
 '(custom-enabled-themes '(modus-operandi))
 '(default-input-method "german-postfix")
 '(electric-pair-mode t)
 '(global-prettify-symbols-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(modus-themes-bold-constructs t)
 '(modus-themes-inhibit-reload nil)
 '(modus-themes-italic-constructs t)
 '(pixel-scroll-precision-mode t)
 '(prettify-symbols-unprettify-at-point nil)
 '(preview-auto-cache-preamble t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(shr-cookie-policy nil)
 '(shr-inhibit-images t)
 '(shr-use-colors nil)
 '(tool-bar-mode nil)
 '(user-mail-address "yguo@posteo.net")
 '(use-package-always-defer t))

;; swap backspace and C-h
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map [?\C-?] [?\C-h])
(define-key key-translation-map [?\M-h] [?\M-\d])
(define-key key-translation-map [?\M-\d] [?\M-h])
;; swap backspace and C-h ends here

(add-hook 'text-mode-hook 'variable-pitch-mode)
(add-hook 'Info-mode-hook 'variable-pitch-mode)

;; ispell, multilingual spellchecking
;; https://www.monotux.tech/posts/2021/02/hunspell-multi-lang/
(with-eval-after-load "ispell"
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "de_DE,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_US")
  (setq ispell-personal-dictionary "~/nixos-config/personal-dictionary.txt")
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

(use-package dired
  :config
  (defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))
  :bind
  (:map dired-mode-map
        ("C-o" . dired-open-file)))

(use-package gnus
  :custom
  (gnus-select-method '(nnimap "posteo.de"))
  (gnus-generate-tree-function 'gnus-generate-horizontal-tree)
  (gnus-message-replysign t)
  (message-send-mail-function 'smtpmail-send-it)
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "posteo.de")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl)
  (mail-envelope-from 'header)
  (mail-host-address "lan")
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header))

(defun my-LaTeX-math-bb (char dollar)
  "Insert a {\\mathbb CHAR}."
  (interactive "*c\nP")
  (insert "\\mathbb{" (char-to-string char) "}"))

(use-package tex
  :hook
  ((LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (TeX-after-compilation-finished-functions
    . TeX-revert-document-buffer)
   (LaTeX-mode . variable-pitch-mode))
  :custom
  (LaTeX-electric-left-right-brace t)
  (TeX-auto-save t)
  (TeX-engine 'luatex)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-electric-sub-and-superscript t)
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "Zathura")))
  :config
  (eval-after-load "LaTeX"
    '(progn
       (setq ispell-tex-skip-alists
             (list (append
                    (car ispell-tex-skip-alists)
                    ;; https://emacs.stackexchange.com/a/19650
                    '(("\\\\[[]" . "\\\\[]]")))
                   (cadr ispell-tex-skip-alists)))
       (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{K}" . ?𝕂))
       (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{C}" . ?ℂ))
       (add-to-list 'tex--prettify-symbols-alist '("\\colon" . ?:))
       (put 'LaTeX-narrow-to-environment 'disabled nil)
       (define-key LaTeX-math-mode-map (kbd "` 8") 'my-LaTeX-math-bb))))

;; zh-cn input engine
(use-package pyim
  :ensure pyim-basedict
  :init
  (pyim-basedict-enable))
;; zh-cn input engine ends here

;; magit
(use-package magit)
