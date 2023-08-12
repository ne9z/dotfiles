(setq use-package-always-defer t)
(custom-set-variables
 '(auto-fill-function 'do-auto-fill)
 '(default-input-method "german-postfix")
 '(electric-pair-mode t)
 '(global-prettify-symbols-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(interprogram-cut-function 'wl-copy t)
 '(interprogram-paste-function 'wl-paste t)
 '(menu-bar-mode nil)
 '(message-send-mail-function 'smtpmail-send-it)
 '(message-sendmail-envelope-from 'header)
 '(prettify-symbols-unprettify-at-point nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(send-mail-function 'smtpmail-send-it)
 '(shr-cookie-policy nil)
 '(shr-inhibit-images t)
 '(smtpmail-smtp-server "posteo.de")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type 'ssl)
 '(tool-bar-mode nil)
 '(user-mail-address "yuchen@posteo.net"))

;; swap backspace and C-h
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map [?\C-?] [?\C-h])
(define-key key-translation-map [?\M-h] [?\M-\d])
(define-key key-translation-map [?\M-\d] [?\M-h])
;; swap backspace and C-h ends here

;; wayland paste
;; credit: yorickvP on Github
(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process
	(make-process
	 :name "wl-copy"
         :buffer nil
         :command '("wl-copy" "-f" "-n")
         :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
;; wayland paste ends here

(use-package modus-themes
  :defer nil
  :custom
  (custom-enabled-themes '(modus-operandi))
  (modus-themes-bold-constructs t)
  (modus-themes-inhibit-reload nil)
  (modus-themes-italic-constructs t))

(use-package ace-window
  :defer nil)

(use-package gnus
  :custom
  (gnus-asynchronous t)
  (gnus-check-new-newsgroups nil)
  (gnus-select-method '(nnimap "posteo.de"))
  (gnus-generate-tree-function 'gnus-generate-horizontal-tree)
  (gnus-interactive-exit 'quiet)
  (gnus-message-replysign t)
  (gnus-permanently-visible-groups "INBOX")
  (gnus-read-active-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-save-killed-list nil)
  (gnus-save-newsrc-file nil)
  (ssl-certificate-verification-policy 1)
  (gnus-secondary-select-methods
   '((nntp "news.gmane.io")
     (nntp "news.eternal-september.org"
           (nntp-authinfo-user "m0p")
           (nntp-port-number 563)
           (nntp-open-connection-function nntp-open-ssl-stream)))))

(use-package tex
  :map LaTeX-math-mode-map ((kbd "` 8") . my-LaTeX-math-bb)
  :config
  (defun my-LaTeX-math-bb (char dollar)
    "Insert a {\\mathbb CHAR}."
    (interactive "*c\nP")
    (insert "\\mathbb{" (char-to-string char) "}"))
  (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{K}" . ?𝕂))
  (add-to-list 'tex--prettify-symbols-alist '("\\mathbb{C}" . ?ℂ))
  (add-to-list 'tex--prettify-symbols-alist '("\\colon" . ?:))
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  :custom
  (LaTeX-electric-left-right-brace t)
  (TeX-auto-save t)
  (TeX-electric-math '("\\(" . "\\)"))
  (TeX-electric-sub-and-superscript t)
  (TeX-engine 'luatex)
  (TeX-view-program-selection '((output-pdf "Zathura"))))

;; zh-cn input engine
(use-package pyim
  :ensure pyim-basedict
  :init
  (pyim-basedict-enable))
;; zh-cn input engine ends here

;; magit
(use-package magit)
