;; -*- lexical-binding:t -*-

;;  this section contains variables defined in C source code
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-fill-function 'do-auto-fill t)
 '(default-input-method "german-postfix")
 '(face-ignored-fonts
   '("Noto Serif CJK HK"
     "Noto Serif CJK KR"
     "Noto Serif CJK TC"
     "Noto Serif CJK JP"
     "Noto Sans CJK"
     "Noto Sans Mono CJK"
     "Unifont"
     "Fixed"
     "ARPH"
     "ADBE") t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mode-line-compact 'long)
 '(read-buffer-completion-ignore-case t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(visible-bell t))

(use-package shr
  :custom
  (shr-cookie-policy nil)
  (shr-inhibit-images t)
  (shr-use-colors nil))

(use-package files
  :custom
  (require-final-newline t))

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t))

(use-package mwheel
  :custom
  (mouse-wheel-mode nil))

(use-package simple
  :custom
  (indent-tabs-mode nil))

(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t)
  (modus-themes-variable-pitch-ui t))

;; must be after modus-themes
(use-package custom
  :config
  ;; swap backspace and C-h
  (define-key key-translation-map [?\C-h] [?\C-?])
  (define-key key-translation-map [?\M-h] [?\M-\d])
  (define-key key-translation-map [?\M-\d] [?\M-h])
  ;; swap backspace and C-h ends here
  :custom
  (custom-enabled-themes '(modus-operandi)))

(use-package auth-source
  :custom
  (auth-sources '("~/.password-store/authinfo.gpg")))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package battery
  :custom
  (display-battery-mode t))

(use-package time
  :custom
  (display-time-mode t))

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
  (user-mail-address "yguo@posteo.net")
  (gnus-treat-display-smileys nil)
  (gnus-inhibit-images t)
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

(use-package face-remap
  :hook
  (text-mode . variable-pitch-mode)
  (Info-mode . variable-pitch-mode))

(use-package latex
  :hook
  ((LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (TeX-after-compilation-finished-functions
    . TeX-revert-document-buffer)
   (LaTeX-mode . LaTeX-math-mode))
  :custom-face
  (font-latex-math-face ((t (:family "Monospace"))))
  :custom
  (prettify-symbols-unprettify-at-point nil)
  (TeX-source-correlate-start-server t)
  ;; disable preamble caching; fails with luatex
  (preview-auto-cache-preamble nil)
  (preview-scale-function 2)
  (preview-image-type 'dvipng)
  (TeX-view-program-selection '((output-pdf "Zathura")))
  (TeX-PDF-mode t)
  (TeX-PDF-from-DVI "Dvipdfmx")
  (TeX-engine 'default)
  (LaTeX-electric-left-right-brace t)
  (TeX-auto-save t)
  (TeX-debug-bad-boxes t)
  (TeX-debug-warnings t)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-electric-sub-and-superscript t)
  (reftex-plug-into-AUCTeX t)
  (LaTeX-math-list
   '(("o r" "mathbb{R}" nil nil)
     ("o Q" "qquad" nil nil)
     ("o q" "quad" nil nil)
     ("o n" "mathbb{N}" nil nil)
     (?= "coloneq" nil nil)
     ("o c" "mathbb{C}" nil nil)))
  (ispell-tex-skip-alists
   (list
    (append
     (car ispell-tex-skip-alists)
     ;; https://emacs.stackexchange.com/a/19650
     '(("\\\\[[]" . "\\\\[]]")))
    (cadr ispell-tex-skip-alists)))
  :bind
  (:map TeX-mode-map
        ("<f8>" . preview-at-point))
  :config
  (with-eval-after-load "preview"
    (add-to-list 'preview-default-preamble
                 "\\PreviewEnvironment*{frame}" t))
  (dolist (symb
           '(("\\(" . ?⌜)
             ("\\)" . ?⌟)
             ("\\colon" . ?:)
             ("\\mathbb{C}" . ?ℂ)
             ("\\mathbb{K}" . ?𝕂)))
    (add-to-list 'tex--prettify-symbols-alist symb)))

;; zh-cn input engine
(use-package pyim
  :init
  (pyim-basedict-enable))
;; zh-cn input engine ends here

(defun yc-larger-font ()
  (interactive)
  (custom-set-faces '(default ((t (:height 151))))))
