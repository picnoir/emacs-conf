;;; package -- Ninjatrappeur's Emacs configuration
;;; Commentary:
;;; My Emacs conf
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)
)

(load-theme 'monokai t)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(set-frame-font "Hack")
(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq-default sentence-end-double-space nil
              indent-tabs-mode nil
              ispell-program-name "@spelling@/bin/ispell")
(add-hook 'text-mode-hook 'flyspell-mode)
;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 3)


(defun loader-after-plugins ()
  (setq global-linum-mode t
        column-number-mode t
        nix-indent-function 'nix-indent-line
        evil-want-keybinding nil
        git-link-use-commit t)
  (use-package protobuf-mode)
  (use-package envrc)
  (use-package helm)
  (use-package helm-ls-git)
  (use-package rg)
  (use-package evil)
  (use-package evil-collection)
  (use-package cmake-mode)
  (use-package flycheck)
  (use-package flycheck-haskell)
  (use-package git-link)
  (use-package haskell-mode)
  (use-package jenkinsfile-mode)
  (use-package magit)
  (use-package nix-mode
    :hook (nix-mode . lsp-deferred)
    :ensure t)
  (use-package go-mode)
  (use-package projectile)
  (use-package racket-mode)
  (use-package notmuch)
  (use-package org)
  (use-package org-modern)
  (use-package which-key)
  (use-package lsp-mode)
  (use-package lsp-ui)
  (use-package my-repo-pins)
  (use-package dockerfile-mode)
  (use-package langtool)
  (use-package yaml-mode)
  (use-package rust-mode)
  (use-package company)
  (use-package company-box)
;  (use-package tree-sitter)
;  (use-package tree-sitter-langs)
  (use-package geiser)
  (use-package geiser-guile)
  (use-package terraform-mode)
  (use-package olivetti)
  (use-package lsp-haskell)
  (use-package yasnippet)
  (use-package lsp-nix
    :ensure lsp-mode
    :after (lsp-mode)
    :demand t
    :custom
    (lsp-nix-nil-formatter ["nixfmt"])
  )
  ; ===================
  ; Olivetti setup
  ; ===================
  (setq olivetti-body-width 0.7
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t)

  (yas-global-mode 1)
  ; ===================
  ; Evil-related stuff=
  ; ===================
  (evil-mode 1)
  (evil-collection-init 'magit)

  (autoload 'notmuch "notmuch" "notmuch mail" t)

  ; Basic movements
  (define-key evil-normal-state-map "c" 'left-char    )
  (define-key evil-normal-state-map "r" 'right-char   )
  (define-key evil-normal-state-map "t" 'next-line    )
  (define-key evil-normal-state-map "s" 'previous-line)
  (define-key evil-visual-state-map "c" 'left-char    )
  (define-key evil-visual-state-map "r" 'right-char   )
  (define-key evil-visual-state-map "t" 'next-line    )
  (define-key evil-visual-state-map "s" 'previous-line)

  ; Windows movement
  (define-key evil-normal-state-map (kbd "T") 'windmove-down )
  (define-key evil-normal-state-map (kbd "S") 'windmove-up   )
  (define-key evil-normal-state-map (kbd "C") 'windmove-left )
  (define-key evil-normal-state-map (kbd "R") 'windmove-right)
  ; Jumps
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  ; Operations
  (define-key evil-normal-state-map "h" 'evil-replace)
  (define-key evil-visual-state-map "h" 'evil-replace)

  ; Compile
  (defun ninjatrappeur--compile-cargo-build ()
    "Set cargo build as compile command."
    (interactive)
    (compile "cargo build"))

  (add-hook
   'rust-mode-hook
   #'(lambda ()
       (local-set-key (kbd "C-c C-c") 'ninjatrappeur--compile-cargo-build)))

;  (add-hook
;   'rust-mode-hook
;   #'tree-sitter-mode)

  (add-hook
   'rust-mode-hook
   #'lsp-mode)

  ;; Prevent lsp-mode from downloading a custom clangd for c/c++
  ;; codebases
  (setq lsp-clients-clangd-executable "clangd")

  ; =========================
  ; Helm-related stuff      =
  ;==========================
  (helm-mode 1)
  (define-key evil-normal-state-map (kbd "C-p") 'helm-ls-git)
  (define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "<f1>") 'helm-apropos)
  (define-key evil-normal-state-map (kbd "C-a") 'rg-project)

  ; =====================
  ; my-repo-pins related stuff
  ;=============================
  (setq my-repo-pins-code-root "~/code-root")
  (global-set-key (kbd "M-h") 'my-repo-pins)

  ;==========================
  ; org- mode related stuff =
  ;==========================
  (setq org-log-done t)
  (setq org-agenda-files (list "~/Notes/work-log-2021.org"))
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c t") (lambda () "Opens the current todolist" (interactive) (find-file "~/Notes/todo.org")))
  ; Org modern
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  ; =========================
  ; Projectile-related stuff=
  ;==========================
  (projectile-mode +1)
  (define-key evil-normal-state-map (kbd "C-c p") 'projectile-command-map)

  ; ====================
  ; Magit-related stuff=
  ;=====================
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x b") 'magit-blame)

  ; =======================
  ; Flycheck-related stuff=
  ;========================
  (global-flycheck-mode)
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

  ; Load typescript mode
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

  ; =======================
  ; Direnv setup          =
  ;========================
  (envrc-global-mode)

  ; =======================
  ;  Which keys           =
  ;========================
  (which-key-mode)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  ; =======================
  ; LSP setup             =
  ;========================
  (add-hook 'go-mode-hook #'lsp-deferred)

  ; =======================
  ; NotMuch config        =
  ;========================
  (defun nin/notmuch-handle-attachments ()
    "Overrides the default action triggered when clicking on a notmuch attachment."
    (interactive)
    (let* ((handle (notmuch-show-current-part-handle)))
      ;(message "%s" handle)
      ; Handle looks like:
      ;( *notmuch-part* (application/ics) nil nil (attachment (filename . invite.ics)) nil nil nil)
      (unwind-protect
          (pcase (car (nth 1 handle))
            ("application/ics"
             (let ((tmpIcal (make-temp-file "notmuch-ical")))
               (mm-save-part-to-file handle tmpIcal)
               (call-process "merkuro-calendar" nil nil nil tmpIcal)
               (delete-file tmpIcal)))
            (_ (notmuch-show-save-part))))))

  (setq notmuch-search-oldest-first nil
      notmuch-show-part-button-default-action #'nin/notmuch-handle-attachments
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from 'header
      mail-envelope-from 'header
      mail-interactive t
      user-full-name "Félix Baylac Jacqué"
      user-mail-address "felix@alternativebit.fr"
      message-kill-buffer-on-exit t
      notmuch-always-prompt-for-sender t
      notmuch-show-indent-messages-width 4
      notmuch-fcc-dirs '(
                         ("felix@alternativebit.fr" . "alternativebit/Sent -inbox +sent -unread")
                         ("ninjatrappeur@alternativebit.fr" . "alternativebit/Sent -inbox +sent -unread")
                         )
      notmuch-saved-searches '((:name "TODO" :query "tag:todo")
                              (:name "Personal Inbox (p)" :query "tag:inbox and tag:personal" :key "p")
                              (:name "CONSULTING (c)" :query "tag:inbox and tag:consulting" :key "c")
                              (:name "Baionet (u)" :query "tag:baionet and tag:unread" :key "u")
                              (:name "Gemini (g)" :query "tag:gemini and tag:unread" :key "g")
                              (:name "guix (gu)" :query "tag:guix and tag:unread")
                              (:name "Nixos Discourse (d)" :query "tag:nixos-discourse and tag:unread")
                              (:name "GHC" :query "tag:ghc-devs and tag:unread")
                              (:name "All lists (l)" :query "tag:lists and tag:unread")
                              (:name "inbox" :query "tag:inbox")
                              (:name "unread" :query "tag:unread")
                              (:name "sent" :query "tag:sent" :key "s")
                              (:name "drafts" :query "tag:draft" :key "d")
                              (:name "all mail" :query "*" :key "a")))
  ; Outgoing email (msmtp + msmtpq)
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq  message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq  message-sendmail-f-is-evil 't)
  (setq  sendmail-program "/run/current-system/sw/bin/msmtp")
  (define-key notmuch-search-mode-map "i"
    (lambda ()
      "mark message as read"
      (interactive)
      (notmuch-search-tag (list "-unread" "-inbox")))
   )

  ; =========================
  ; Custom Functions        =
  ;==========================

  (defun add-current-directory-to-load-path ()
    "Add the current directory to the emacs load path. That's pretty
useful when interactively debugging/developping a emacs package."
    (interactive)
    (add-to-list 'load-path default-directory))

  ;==========================
  ; Backup Files
  ;==========================

  (let ((backup-dir "~/tmp/emacs/backups")
        (auto-saves-dir "~/tmp/emacs/auto-saves/"))
    (dolist (dir (list backup-dir auto-saves-dir))
      (when (not (file-directory-p dir))
        (make-directory dir t)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
          auto-save-list-file-prefix (concat auto-saves-dir ".saves-")))

  (setq backup-by-copying t    ; Don't delink hardlinks
        delete-old-versions t  ; Clean up the backups
        version-control t      ; Use version numbers on backups,
        kept-new-versions 5    ; keep some new versions
        kept-old-versions 2)   ; and some old ones, too

)



(add-hook 'after-init-hook #'loader-after-plugins)
;;; default.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-rust-all-features t)
 '(package-selected-packages
   '(yasnippet lsp-haskell olivetti terraform-mode geiser-guile geiser company-box company rust-mode yaml-mode langtool dockerfile-mode my-repo-pins lsp-ui which-key org-modern notmuch racket-mode projectile go-mode magit jenkinsfile-mode haskell-mode git-link flycheck-haskell flycheck cmake-mode evil-collection evil rg helm-ls-git helm envrc protobuf-mode lsp-mode nix-mode monokai-theme))
 '(safe-local-variable-values
   '((eval c-set-offset 'inlambda 0)
     (eval c-set-offset 'access-label '-)
     (eval c-set-offset 'substatement-open 0)
     (eval c-set-offset 'arglist-cont-nonempty '+)
     (eval c-set-offset 'arglist-cont 0)
     (eval c-set-offset 'arglist-intro '+)
     (eval c-set-offset 'inline-open 0)
     (eval c-set-offset 'defun-open 0)
     (eval c-set-offset 'innamespace 0)
     (indicate-empty-lines . t)
     (c-block-comment-prefix . "  "))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
