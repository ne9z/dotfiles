;; -*- lexical-binding:t -*-
(set-fontset-font "fontset-startup" 'latin "NewComputerModernMono10")
(set-fontset-font "fontset-startup" 'han "Noto Sans Mono CJK SC")
(set-fontset-font "fontset-startup" 'cjk-misc "Noto Sans Mono CJK SC")

(add-to-list 'default-frame-alist
             '(font . "fontset-startup"))

(custom-set-variables
 '(auto-fill-function 'do-auto-fill)
 '(default-input-method "german-postfix")
 '(electric-pair-mode t)
 '(global-prettify-symbols-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(prettify-symbols-unprettify-at-point nil)
 '(preview-auto-cache-preamble t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(ring-bell-function 'ignore)
 '(shr-cookie-policy nil)
 '(shr-inhibit-images t)
 '(shr-use-colors nil)
 '(tool-bar-mode nil)
 '(use-package-always-defer t)
 '(user-mail-address "yguo@posteo.net")
 '(visible-bell t))

;; swap backspace and C-h
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map [?\C-?] [?\C-h])
(define-key key-translation-map [?\M-h] [?\M-\d])
(define-key key-translation-map [?\M-\d] [?\M-h])
;; swap backspace and C-h ends here

;; ispell, multilingual spellchecking
;; https://www.monotux.tech/posts/2021/02/hunspell-multi-lang/
(use-package ispell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "de_DE,en_US")
  (ispell-personal-dictionary
   "~/nixos-config/personal-dictionary.txt")
  :config
  (setenv "LANG" "en_US.UTF-8")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_US"))

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

(add-hook 'text-mode-hook 'variable-pitch-mode)
(add-hook 'Info-mode-hook 'variable-pitch-mode)

(use-package tex
  :hook
  ((LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . variable-pitch-mode)
   (TeX-after-compilation-finished-functions
    . TeX-revert-document-buffer))
  :custom
  (LaTeX-electric-left-right-brace t)
  (TeX-auto-save t)
  (TeX-debug-bad-boxes t)
  (TeX-debug-warnings t)
  (TeX-engine 'luatex)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (TeX-electric-sub-and-superscript t)
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "Zathura")))
  (LaTeX-math-list
   '(("o r" "mathbb{R}" nil nil)
     (?= "coloneq" nil nil)
     ("o c" "mathbb{C}" nil nil)))
  (ispell-tex-skip-alists
   (list
    (append
     (car ispell-tex-skip-alists)
     ;; https://emacs.stackexchange.com/a/19650
     '(("\\\\[[]" . "\\\\[]]")))
    (cadr ispell-tex-skip-alists)))
  :config
  (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{K}" . ?𝕂))
  (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{C}" . ?ℂ))
  (add-to-list 'tex--prettify-symbols-alist '("\\colon" . ?:)))

;; zh-cn input engine
(use-package pyim
  :init
  (pyim-basedict-enable))
;; zh-cn input engine ends here

(pdf-tools-install :no-query)
