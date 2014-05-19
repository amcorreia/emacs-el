;;; .emacs --- Init file
;; Copyright (C) 2005-2014 Alessandro Madruga Correia
;; Author: Alessandro Madruga Correia <mutley.sandro@gmail.com>
;; Keywords: emacs, dotfile, config
;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;
;; This file is NOT part of GNU Emacs.
;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;
;;  |- globals
;;  | `|-> key-binding
;;  |  |-> hooks
;;  |- functions
;;  | `|-> My
;;  |  |-> by other people
;;  |- GNU Emacs
;;  | `|-> programming
;;  |  |-> packages
;;  |  |-> other
;;  |- non-GNU Emacs
;;  | `|-> packages
;;  |  |-> other
;;  |- Games
;;  |- other

;; ---[ GLOBALS ]---
(load-file "~/.emacs.d/emacs-el/site-lisp/cedet/cedet-devel-load.el")
;; dummy term don't fontify correct comment
(unless (eq window-system 'x) ; tty
  (defvar font-lock-comment-face '((:foreground "red" :slant normal :weight extra-light))))

; if X then
(when window-system
  ;(set-foreground-color "DarkGrey")
  ;(set-foreground-color "LightGrey")
  (set-foreground-color "white")
  (set-background-color "black")
  (set-cursor-color "cyan")
  (set-mouse-color "black")
  ;(defvar font-lock-comment-face       '((:foreground "red"    :slant italic :weight ultra-light)))
  (defvar font-lock-variable-name-face '((:foreground "yellow" :slant italic :weight light-ultra)))
;  (defvar font-lock-string-face        '((:foreground "green"  :slant italic :weight ultra-light)))
  (defvar x-pointer-shape x-pointer-top-left-arrow) ;; altera ponteiro o mouse
  ;(set-default-font "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-1")
  ; barra com os botoes salvar, novo arquivo etc...
  (tool-bar-mode        -1)
  (set-scroll-bar-mode  nil)
  )

;; ---[ globals ]---

(setq   inhibit-startup-message       t)   ; don't show statup message
(defvar kill-read-only-ok             t)   ; show message when try kill read-only text
(defvar font-lock-maximum-decoration  t)   ; maximum possible fontification.
(setq   scroll-step                   1)   ; don't make pager
(defvar scroll-conservatively         5)
(setq column-number-mode              t)
(setq line-number-mode                t)
(defvar display-time-day-and-date     t)
(defvar display-time-24hr-format    t)
(defvar battery-mode-line-format    " [%b%p%%,%d°C]")
(defvar visible-bell                  t) ; no X esse barulho eh chato.
(defvar user-full-name                "Alessandro Madruga Correia")
(defvar user-mail-address             "amcorreia@domain.com.br")
(defvar user-login-name               "madruga")
(defvar bookmark-default-file       "~/.emacs.d/tmp/emacs.bmk") ; bookmarks file
(defvar backup-by-copying           t)
(defvar backup-directory-alist        '(("." . "~/.emacs.d/tmp/backups")))
(defvar Info-default-directory-list   (append
				     '("~/usr/info/" "~/usr/share/info/")
				     Info-default-directory-list))
(eval-when-compile
  (defvar load-path (append '("~/.emacs.d/emacs-el/elisp/") load-path)))
(add-to-list 'load-path "~/.emacs.d/emacs-el/elisp/")

(eval-when-compile
  (let ((default-directory "~/.emacs.d/emacs-el/site-lisp"))
          (normal-top-level-add-subdirs-to-load-path)))

(let ((default-directory "~/.emacs.d/emacs-el/site-lisp"))
               (normal-top-level-add-subdirs-to-load-path))

(setq-default indent-tabs-mode nil)  ; set spaces instead tab to indent

(fset 'yes-or-no-p                  'y-or-n-p)   ; make all ask y-n

(global-font-lock-mode              t)   ; enable font-lock
(transient-mark-mode                t)   ; font-lock region
(show-paren-mode                    t)   ; ()
(display-time-mode                  t)   ; display current time
(blink-cursor-mode                  -1)
(display-battery-mode               t) ; show battery status

;;;;Require C-x C-c prompt. I've closed too often by accident.
;;;;http://www.dotemacs.de/dotfiles/KilianAFoth.emacs.htmls
(global-set-key [(control x) (control c)]
  (function 
   (lambda () (interactive) 
     (cond ((y-or-n-p "Deseja sair do Emacs? ")
            (save-buffers-kill-emacs))))))
; exit emacs    M- <Power>
(global-set-key ;[134219945]
                [2217] ; <Power>
		(function 
		 (lambda () (interactive) 
		   (cond ((y-or-n-p "Deseja sair do Emacs? ")
			  (save-buffers-kill-emacs))))))
;; enlarge or shrink windows more easily
(global-set-key [(control shift up)]    'enlarge-window)
(global-set-key [(control shift down)]  'shrink-window)
(global-set-key [(control shift left)]  'enlarge-window-horizontally)
(global-set-key [(control shift right)] 'shrink-window-horizontally)
;; make all visible windows the same height (approximately)
(global-set-key [(control shift end)]   'balance-windows)

;; switch buffer
(global-set-key [M-right]  'next-buffer)
(global-set-key [M-left]   'previous-buffer)
(global-set-key "\M-r"     '(lambda () (interactive) (switch-to-buffer nil)))
;; ses
(global-set-key "\C-cm1"  '(lambda () (interactive) (find-file "~/.emacs.d/config/ses/financas.ses")))
(global-set-key "\C-cm2"  '(lambda () (interactive) (find-file "~/.emacs.d/config/ses/almoco.ses")))
(global-set-key "\C-cm3"  '(lambda () (interactive) (find-file "~/.emacs.d/config/ses/bike.ses")))
(global-set-key "\M-k"    'kill-other-buffer)
;(global-set-key "\C-cma"  '(lambda () (interactive) (find-file "~/security/notificacao/ids-forms.form")))
    ;;; f1-f4   for global toggle
    ;;; f5-f8   for local  toggle
    ;;; f9-f12  for shorcut command
(global-set-key  [f9]    'dabbrev-expand)
(global-set-key  [home]  'beginning-of-buffer)
(global-set-key  [end]   'end-of-buffer)

(add-hook 'ses-mode-hook 'hl-line-mode)

;;{{{ --[ Hooks colors ]--------

(add-hook 'write-file-hooks           'time-stamp)     ; write ts

;;;     colors
(if (< emacs-major-version 24)
    (progn
      (message "23")
      (set-face-background 'modeline "skyblue")
      (set-face-background 'menu     "skyblue")
      (set-face-foreground 'modeline "black"))
  (progn
    (set-face-background 'mode-line "gray18")
    (set-face-foreground 'mode-line "cyan")
    ;(set-face-background 'mode-line "slate blue")
    (set-face-background 'mode-line-inactive "dark slate gray")
    ;(set-face-background 'minibuffer-prompt "dar slate gray")
    (set-face-foreground 'minibuffer-prompt "light steel blue")
    ;(set-face-background 'ac-candidate-face "gray28") 
    ;(set-face-foreground 'ac-candidate-face "yellow green")
;    (set-face-foreground 'mode-line "cyan")
    (set-face-background 'menu     "slate blue")))
    
(set-face-foreground 'region   "red")
(set-face-foreground 'menu     "black")

;; ---[ FUNCTIONS ]---
;; ---[ MY ]---
;;open my .gnus.el
(defun my-open-dot-gnus ()
  "Opening ~/.gnus.el"
  (interactive)
  (find-file "~/.gnus.el"))
(global-set-key [(shift f2)] 'my-open-dot-gnus)

(defun my-open-dot-emacs ()
 "Opening ~/.emacs.d/emacs-el/init.el"
 (interactive)
 (find-file "~/.emacs.d/emacs-el/init.el"))
(global-set-key [(shift f11)] 'my-open-dot-emacs)

;;{{{ correios
; um objeto
;   http://websro.correios.com.br/sro_bin/txect01$.QueryList?P_LINGUA=001&P_TIPO=001&P_COD_UNI=<CODIGO_DO_OBJETO>
; lista de objetos
;   http://websro.correios.com.br/sro_bin/txect01$.Inexistente?P_LINGUA=001&P_TIPO=003&P_COD_LIS=en438235576br;rc941739379br
(defvar *my-correios-link*          "http://websro.correios.com.br/sro_bin/txect01$.QueryList?P_LINGUA=001&P_TIPO=001&P_COD_UNI=%s")
(defvar *my-correios-link-l*        "http://websro.correios.com.br/sro_bin/txect01$.Inexistente?P_LINGUA=001&P_TIPO=003&P_COD_LIS=%s")
(defvar *my-lynx-command*           "lynx")
(defvar *my-lynx-opt*               "-dump -crawl -pauth=amcorreia:XXX")
(defvar *my-correios-buffer-name*   "*correios-track-trace*")

(defun my-correios-obj nil ""
  (interactive)
  (let ((obj (read-string "Digite o numero do objeto: ")))
    (shell-command (format "%s %s '%s'" *my-lynx-command* *my-lynx-opt* (format  *my-correios-link* obj)) *my-correios-buffer-name*))
  (save-excursion
    (let ((buf (current-buffer)))
      (with-temp-buffer *my-correios-buffer-name*
                        (switch-to-buffer-other-frame *my-correios-buffer-name*)
                        (save-excursion
                          (goto-char (point-min))
                          (kill-line 3)
                          (goto-char (point-max))
                          (search-backward "Postado")
                          ;(next-line)
			  (forward-line 1)
                          (kill-line 5))
                        (resize-temp-buffer-window)
                        (search-forward "Data")
                        ;(next-line 2)
			(forward-line 2)
                        (message "Situação=> %s" (buffer-substring (progn (beginning-of-line) (point)) (progn (end-of-line) (point)) )))
      (switch-to-buffer-other-frame buf))))

(defun my-correios-obj-lista nil ""
  (interactive)
  (let ((lista-obj (read-string "Digite a lista de numeros separados por ';': ")))
    (shell-command (format "%s %s '%s'" *my-lynx-command* *my-lynx-opt* (format  *my-correios-link-l* lista-obj)) *my-correios-buffer-name*))
  (save-excursion
    (let ((buf (current-buffer)))
      (with-temp-buffer *my-correios-buffer-name*
                        (if (one-window-p)
                            (split-window))
                        (switch-to-buffer-other-frame *my-correios-buffer-name*)
                        (save-excursion
                          (goto-char (point-min))
                          (kill-line 3)
                          (goto-char (point-max))
                          (resize-temp-buffer-window)))
      (switch-to-buffer-other-frame buf))))

;; ---[ By Others ]---------
;; These are written by Mike Baranski.
(defun kill-other-buffer ()
  "Kill other buffer, if more than one is showing.  If only one is showing, it'll kill that one."
  (interactive) (other-window 1) (kill-buffer nil) (delete-window))
;; kill buffer without confirmation
;(if not modified)
(defun my-kill-this-buffer ()
  "Kill the current buffer without confirmation" (interactive) (kill-buffer nil))
;; key binding
(global-set-key [(pause)] 'my-kill-this-buffer)
(defun my-leo (word) (interactive "sWord: ")
  (browse-url (format "http://dict.leo.org/?search=%s" word)))
;;{{{ funcoes de backup
;; make backup files in ~/.backups/ rather than scattered around all
;; over the filesystem.
(require 'dired)
(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
              (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

;; disable backups for files in /tmp or in my Mail or News directories.
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "~/tmp/" filename))))
;;}}}
;;{{{ --[ ASCII-table ]----

;;;; copied from http://www.emacswiki.org/cgi-bin/emacs-en/AsciiTable
(defun my-ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96))))))

;;; ASCII-table
;;;; copied from http://www.emacswiki.org/cgi-bin/emacs-en/EbcdicTable
(defvar my-ebcdic-ascii-xlate-tbl
  '( #x00  #x01  #x02  #x03  #x9c  #x09  #x86  #x7f
     #x97  #x8d  #x8e  #x0b  #x0c  #x0d  #x0e  #x0f
     #x10  #x11  #x12  #x13  #x9d  #x85  #x08  #x87
     #x18  #x19  #x92  #x8f  #x1c  #x1d  #x1e  #x1f
     #x80  #x81  #x82  #x83  #x84  #x0a  #x17  #x1b
     #x88  #x89  #x8a  #x8b  #x8c  #x05  #x06  #x07
     #x90  #x91  #x16  #x93  #x94  #x95  #x96  #x04
     #x98  #x99  #x9a  #x9b  #x14  #x15  #x9e  #x1a
     #x20  #xa0  #xa1  #xa2  #xa3  #xa4  #xa5  #xa6
     #xa7  #xa8  #x5b  #x2e  #x3c  #x28  #x2b  #x21
     #x26  #xa9  #xaa  #xab  #xac  #xad  #xae  #xaf
     #xb0  #xb1  #x5d  #x24  #x2a  #x29  #x3b  #x5e
     #x2d  #x2f  #xb2  #xb3  #xb4  #xb5  #xb6  #xb7
     #xb8  #xb9  #x7c  #x2c  #x25  #x5f  #x3e  #x3f
     #xba  #xbb  #xbc  #xbd  #xbe  #xbf  #xc0  #xc1
     #xc2  #x60  #x3a  #x23  #x40  #x27  #x3d  #x22
     #xc3  #x61  #x62  #x63  #x64  #x65  #x66  #x67
     #x68  #x69  #xc4  #xc5  #xc6  #xc7  #xc8  #xc9
     #xca  #x6a  #x6b  #x6c  #x6d  #x6e  #x6f  #x70
     #x71  #x72  #xcb  #xcc  #xcd  #xce  #xcf  #xd0
     #xd1  #x7e  #x73  #x74  #x75  #x76  #x77  #x78
     #x78  #x7a  #xd2  #xd3  #xd4  #xd5  #xd6  #xd7
     #xd8  #xd9  #xda  #xdb  #xdc  #xdd  #xde  #xdf
     #xe0  #xe1  #xe2  #xe3  #xe4  #xe5  #xe6  #xe7
     #x7b  #x41  #x42  #x43  #x44  #x45  #x46  #x47
     #x48  #x49  #xe8  #xe9  #xea  #xeb  #xec  #xed
     #x7d  #x4a  #x4b  #x4c  #x4d  #x4e  #x4f  #x50
     #x51  #x52  #xee  #xef  #xf0  #xf1  #xf2  #xf3
     #x5c  #x9f  #x53  #x54  #x55  #x56  #x57  #x58
     #x59  #x5a  #xf4  #xf5  #xf6  #xf7  #xf8  #xf9
     #x30  #x31  #x32  #x33  #x34  #x35  #x36  #x37
     #x38  #x39  #xfa  #xfb  #xfc  #xfd  #xfe  #xff))

(defun my-ebcdic-table ()
  "Display ebcdic table (0 thru 255)."
  (interactive)
  (switch-to-buffer "*EBCDIC*")
  (erase-buffer)
  (save-excursion
    (let ((i -1)
          (j 64))
      (insert "EBCDIC characters 0 thru 255.\n\n")
      (insert " Hex  Dec  Char | Hex  Dec  Char | Hex  Dec  Char | Hex  Dec  Char\n")
      (while (< i (1- j))
        (insert
         (format
          "%4x %4d %4s  |%4x %4d %4s  |%4x %4d %4s  |%4x %4d %4s\n"
          (setq i (+ 1 i)) i (single-key-description (elt my-ebcdic-ascii-xlate-tbl i))
          (setq i (+ j i)) i (single-key-description (elt my-ebcdic-ascii-xlate-tbl i))
          (setq i (+ j i)) i (single-key-description (elt my-ebcdic-ascii-xlate-tbl i))
          (setq i (+ j i)) i (single-key-description (elt my-ebcdic-ascii-xlate-tbl i))))
        (setq i (- i (* 3 j)))))))

;;}}}
;;{{{    Recreating killed buffers

;; If the *scratch* buffer is killed, recreate it automatically
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

;; If the *Messages* buffer is killed, recreate it automatically
(save-excursion
  (set-buffer (get-buffer-create "*Messages*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; Kill the current (*Messages*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *Messages* buffer
  (set-buffer (get-buffer-create "*Messages*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)

;;}}}
;;}}} [ by others ]


;;}}}
;;{{{ ---[ PACKAGES ]-----------------------------------------------------------
;;{{{ --[ Programming ]---------------------------------------------------------

;;;
;;; C-mode
;;;

;(add-hook 'c-initialization-hook                'my-c-mode-hook)
(add-hook 'c-mode-hook            'my-c-mode-hook)
(defconst my-c-style
  ;;(defvar c-comment-only-line-offset  4)
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-entry-open); before)))
                                   (substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (;brace-else-brace
                                   scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . (;(arglist-close . c-lineup-arglist)
                                   (c                     . c-lineup-C-comments)
                                   (comment-intro         . 0); 2 erro no Cparse
                                   (substatement-open     . 0)
                                   (statement-block-intro . 4)
                                   (case-label            . 2)
                                   (statement-case-intro  . 4)
                                   (block-open            . 0)
                                   (block-close           . 0)
                                   (inclass               . 4)
                                   (defun-block-intro     . 4)
                                   (arglist-cont-nonempty . ;'('(c-lineup-gcc-asm-reg c-lineup-arglist)))
                                                          c-lineup-arglist)
                                   (brace-list-open       . 4)
                                   (brace-list-close      . 0)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")


(defconst my-c-style-obsd
  '((c-basic-offset             . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist
     (statement-block-intro  . +)
     (knr-argdecl-intro      . +)
     (substatement-open      . 0)
     (substatement-label     . 0)
     (label                  . 0)
     (statement-cont         . +)
     (inline-open            . 0)
     (inexpr-class           . 0))
    ; add or remove whitespace
    (c-cleanup-list   .
                      (;brace-else-brace brace-elseif-brace list-close-comma
                       empty-defun-braces
                       defun-close-semi
                       space-before-funcall
                       compact-empty-funcall
                       )))
  "My OpenBSD C Programming Style")

(c-add-style "PERSONAL" my-c-style)
(c-add-style "my-obsd"  my-c-style-obsd)

(defun my-c-mode-common-hook ()
  (defvar c-basic-offset                   4)
  (defvar c-echo-syntactic-information-p   t)
  ;(defvar c-report-syntactic-errors        t)
  (c-toggle-auto-newline                 t)
  (c-toggle-electric-state               t))


(defun my-c-mode-hook ()
  (autoload 'libc-mode "libc-mode" "GNU C Library Info Explorer." t)

  (my-c-mode-common-hook)
  (defvar c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (other     . "bsd")))
  (c-set-style "my-obsd")
  (define-key c-mode-map [f12]        'compile)
  (define-key c-mode-map "\C-ch"      'man-follow)
  (define-key c-mode-map "\C-m"       'c-context-line-break)
  (define-key c-mode-map [?\C-\M-a]   'c-beginning-of-defun)
  (define-key c-mode-map [?\C-\M-e]   'c-end-of-defun)
  ;; f5-f8 for local toggle
  (define-key c-mode-map (kbd "ESC <f5>")
    '(lambda ()
       (interactive)
       (setq compile-command "make -k")
       (message "for toggle local compile See '<f5>'")))
  (define-key c-mode-map [f5]
    '(lambda (&optional opt)
       (interactive "Mtype optional argument for gcc (RET for none): ")
       (let* ((opt (or opt nil))
              (c (concat "gcc -Wall -g " opt " "
                         (buffer-file-name)
                         " -o "
                         (file-name-sans-extension (buffer-name)))))
         (setq compile-command c))
       (message "for toggle compile with Makefile See 'ESC <f5>'")))
  ;; f9-f12 shortcuts
  (local-set-key [f8]  'tempo-complete-tag)
  )
;; c-mode

;;;;
;;;;  Perl-mode
;;;;

(defvar my-cperl-mode-help nil
  "\\[cperl-perldoc] cperl-perldoc

Outline: (prefix M-o)
 \\[hide-sublevels]  hide-sublevels
 \\[hide-body]  hide-body
 \\[hide-other]  hide-other
 \\[hide-entry]  hide-entry
 \\[hide-leaves]  hide-leaves
 \\[hide-subtree]  hide-subtree
 \\[show-all]  show-all
 \\[show-entry]  show-entry
 \\[show-children]  show-children
 \\[show-branches]  show-branches
 \\[show-subtree]  show-subtree
 \\[outline-up-heading]  outline-up-heading
 \\[outline-next-visible-heading]  outline-next-visible-heading
 \\[outline-previous-visible-heading]  outline-previous-visible-heading
 \\[outline-forward-same-level]  outline-forward-same-level
 \\[outline-backward-same-level]  outline-backward-same-level
")
;;; cperl-mode is preferred to perl-mode
;;; "Brevity is the soul of wit" <foo at acm.org>
(defalias 'perl-mode 'cperl-mode)
(defvar cperl-hairy t) ;; Turns on most of the CPerlMode options

(autoload 'sepia-init "~/.emacs.d/site-lisp/net/cvs-packages/sepia")
;(add-hook 'cperl-mode-hook
;         (lambda ()
;           (local-set-key (kbd "C-h f") 'cperl-perldoc)))

(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

;(add-hook 'cperl-mode-hook
;         (lambda ()
;           (local-set-key (kbd "C-h f") 'cperl-perldoc)
;           (set (make-local-variable 'eldoc-documentation-function)
;                'my-cperl-eldoc-documentation-function)))

; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)

(defmacro join (join-char &rest others) `(mapconcat 'identity ',others ,join-char))

(defvar my-cperl-outline-regexp
      (concat
       "^"                              ; Start of line
       "[ \\t]*"                        ; Skip leading whitespace
       "\\("                            ; begin capture group \1
       (join "\\|"
             "=head[1-9]"                  ; POD header
             "package"                    ; package
             "=item"                      ; POD item
             "sub"                        ; subroutine definition
           )
       "\\)"                            ; end capture group \1
       "\\b"                            ; Word boundary
       ))


(defvar cperl-mode-hook 'my-cperl-customizations)

(defun my-cperl-customizations ()
  "cperl-mode customizations that must be done after cperl-mode loads"
  (outline-minor-mode)
  (abbrev-mode)

  (local-set-key (kbd "C-h f") 'cperl-perldoc)
  (set (make-local-variable 'eldoc-documentation-function)
       'my-cperl-eldoc-documentation-function)

  (defun cperl-outline-level ()
    (looking-at outline-regexp)
    (let ((match (match-string 1)))
      (cond
       ((eq match "=head1" ) 1)
       ((eq match "package") 2)
       ((eq match "=head2" ) 3)
       ((eq match "=item"  ) 4)
       ((eq match "sub"    ) 5)
       (t 7)
       )))

  (defvar cperl-outline-regexp  my-cperl-outline-regexp)
  (defvar outline-regexp        cperl-outline-regexp)
  (defvar outline-level        'cperl-outline-level)
)
;; perm-mode


;;{{{ --[ GDB ]---------------

(defvar gdb-many-windows            t)
(defvar gdb-show-main               t)
(defvar gdb-use-separate-io-buffer  t)

;;}}}

;;; python
;(require 'auto-pep8)
;(add-hook 'python-mode-hook (lambda () (auto-pep8-mode 1)))
(require 'flycheck)
(require 'pyvenv)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;{{{ --[ PHP-mode ]----------
(defun php-lint ()
  "Performs lint-check on the current buffer"
  (interactive)
  (shell-command (concat "/usr/bin/php -l " (buffer-file-name))))

(defun php-exec ()
  "Executes current buffer"
  (interactive)
  (shell-command (concat "/usr/bin/php " (buffer-file-name))))

(require 'php-mode)
(defvar auto-mode-alist (append '(("\\.php\\'" . php-mode))  auto-mode-alist))

(add-hook 'php-mode-hook
	  '(lambda ()
             (defvar php-documentation-url
                   "http://gw/docs/programacao/php/php-5.4-PT/")
             (defvar php-manual-url
                   "http://gw/docs/programacao/php/php-5.4-PT/")
             (defvar php-documentation-url-local t)
	     (define-key php-mode-map "\C-cact"       'tempo-complete-tag)
             (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
             
	     (tempo-use-tag-list                 'php-tempo-tags)))

(require 'cake2)
(global-cake2 t)
(cake2-set-default-keymap)
; 
; testar eval-after-load "cake2"
(add-hook 'cake2-hook
          #'(lambda()
              (defvar yas/mode-symbol 'cake2)
              ;(yas-activate-extra-mode 'cake2)
              (add-to-list 'yas--extra-modes 'cake2)
              (yas-activate-extra-mode 'cake2)
              (defvar cake-plural-rules
                    (append '(
                              ("^\\(.*\\)ao$" "\\1oes")
                              ("^\\(.*\\)\\(r\\|s\\|z\\)$" "\\1\\2es")
                              ("^\\(.*\\)\\(a\\|e\\|o\\|u\\)l$" "\\1\\2is")
                              ("^\\(.*\\)il$" "\\1is")
                              ("^\\(.*\\)\\(m\\|n\\)$" "\\1ns")
                              ("^\\(.*\\)$" "\\1s")
                              ) cake-plural-rules 
                                ))
              (defvar cake-singular-rules
                    (append '(
                              ("^\\(.*\\)\\(oes\\|aes\\|aos\\)$" "\\1ao")
                              ("^\\(.*\\)\\(a\\|e\\|o\\|u\\)is$" "\\1\\2l")
                              ("^\\(.*\\)e?is$" "\\1il")
                              ("^\\(.*\\)\\(r\\|s\\|z\\)es$" "\\1\\2")
                              ("^\\(.*\\)ns$" "\\1m")
                              ("^\\(.*\\)s$" "\\1")
                              ) cake-singular-rules
                                ))
                           ))


(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/emacs-el/snippets"
        "~/.emacs.d/emacs-el/site-lisp/emacs-cake2/snippets/"
        "~/.emacs.d/emacs-el/site-lisp/yasnippet/yasmate/snippets"
        "~/.emacs.d/emacs-el/site-lisp/yasnippet/snippets"
        ))
(yas-global-mode 1)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/emacs-el/site-lisp/php-auto-yasnippets/Create-PHP-YASnippet.php")

(require 'popup)
(require 'auto-complete-config)
(ac-config-default)


;(define-key ac-mode-map (kbd "<tab>") 'auto-complete)
;(ac-set-trigger-key "TAB")

;; dirty fix for having AC everywhere
;(define-globalized-minor-mode real-global-auto-complete-mode
;  auto-complete-mode (lambda ()
;                       (if (not (minibufferp (current-buffer)))
;                           (auto-complete-mode 1))
;                       ))
;(real-global-auto-complete-mode t)

(require 'web-mode)
(defvar web-mode-enable-auto-pairing t)
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))

;;; html
(add-hook 'html-mode-hook
	  (lambda ()
	    ;(defvar tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
	    ;64 68 72 76 80 84 88 92 96 100 104 108 112
	    ;116 120))
	    (defvar tab-stop-list (let ((stops '(8)))
				  (while (< (car stops) 120)
				    (defvar stops (cons (+ 8 (car stops)) stops)))
				  (nreverse stops)))
	    (defvar indent-line-function 'indent-relative)))

;; "funky stuff" ;; proceed with caution ;;
(defvar my-key-pairs nil)

(defvar my-key-pairs '((?! ?1) (?@ ?2) (?# ?3) (?$ ?4) (?% ?5)
                     (?^ ?6) (?& ?7) (?* ?8) (?( ?9) (?) ?0)
                     (?- ?_) (?\" ?') (?{ ?[) (?} ?]) ; (?| ?\\)
                     ))
(defun my-swap nil
  (interactive)
  (my-key-swap    my-key-pairs))

(defun my-key-swap (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard zapped!! %s to restore!"
               (substitute-command-keys "\\[my-restore]"))
    (progn
      (keyboard-translate (caar key-pairs)  (cadar key-pairs))
      (keyboard-translate (cadar key-pairs) (caar key-pairs))
      (my-key-swap (cdr key-pairs)))))

(defun my-restore nil
  (interactive)
  (my-key-restore my-key-pairs))

(defun my-key-restore (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard restored!! %s to Zap!"
               (substitute-command-keys "\\[my-swap]"))
    (progn
      (keyboard-translate (caar key-pairs)  (caar key-pairs))
      (keyboard-translate (cadar key-pairs) (cadar key-pairs))
      (my-key-restore (cdr key-pairs)))))
;; "funky stuff" ;; proceed with caution

;;    This function is funky but useful. Having swapped the pairs
;; ('[', '{'), ('-', '_') and (']', '}'), in order to type "->", we need
;; to type four characters ('Shift' followed by '-' followed by 'Shift'
;; followed by '>'). With the above code, all you need to type is two
;; underscores: '__'). Automagically, they are converted into
;; '->'). Similarly, two successive dots '..' are translated into '[]'
;; (for array indexing). I find that these combinations improve my
;; code-typing speed significantly.

;;   ,.   ->
;;   ,,   {}
;;   ..   []
(defun my-editing-function (first last len)
  (interactive)
  (if (and (boundp 'major-mode)
           (member major-mode
                   (list 'c-mode 'c++-mode 'gud-mode 'php-mode
                         'fundamental-mode 'ruby-mode 'python-mode))
           (= len 0)
           (> (point) 4)
           (= first (- (point) 1)))
      (cond
       ;; _
;;        ((and (string-equal (buffer-substring (point) (- (point) 2)) "__")
;;              (not (string-equal (buffer-substring (point) (- (point) 3)) "___")))
;;         (progn (delete-backward-char 2) (insert-char ?- 1) (insert-char ?> 1)))
       ;; ,. ->
       ((and (string-equal (buffer-substring (point) (- (point) 2)) ",.")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "___")))
        (progn (delete-char -2) (insert-char ?- 1) (insert-char ?> 1)))
       ;; ., =>
       ((and (string-equal (buffer-substring (point) (- (point) 2)) ".,")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "___")))
        (progn (delete-char -2) (insert-char ?= 1) (insert-char ?> 1)))

;;        ((string-equal (buffer-substring (point) (- (point) 3)) "->_")
;;         (progn (delete-backward-char 3) (insert-char ?_ 3)))
       ;; .
       ((and (string-equal (buffer-substring (point) (- (point) 2)) "..")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "...")))
        (progn (delete-char -2) (insert-char ?[ 1) (insert-char ?] 1)
               (backward-char 1)))

       ((and (> (point-max) (point))
             (string-equal (buffer-substring (+ (point) 1) (- (point) 2)) "[.]"))
        (progn (forward-char 1) (delete-char -3) (insert-char ?. 1) (insert-char ?. 1) ))
       ;; teste
       ;; .. []
       ((and (string-equal (buffer-substring (point) (- (point) 2)) ",,")
             (not (string-equal (buffer-substring (point) (- (point) 3)) ",,,")))
        (progn (delete-char -2) (insert-char ?{ 1) (insert-char ?} 1)
               (backward-char 1)))
       ;; ,, {}
       ((and (> (point-max) (point))
             (string-equal (buffer-substring (+ (point) 1) (- (point) 2)) "{.}"))
        (progn (forward-char 1) (delete-char -3) (insert-char ?, 1) (insert-char ?, 1) ))
       )
    nil))

(add-hook 'after-change-functions 'my-editing-function)


;;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;;}}}
;; magit
;(add-to-list 'load-path "~/.emacs.d/emacs-el/site-lisp/git-modes")
;(add-to-list 'load-path "~/.emacs.d/emacs-el/site-lisp/magit")
;(eval-after-load 'info
;  '(progn (info-initialize)
;          (add-to-list 'Info-directory-list "~/.emacs.d/emacs-el/site-lisp/magit")))
(require 'magit)

;(require 'ecb)


;;}}} --[ Programming ]
;;{{{ ---[ GNU-emacs ]----------------------------------------------------------

;;{{{ --[ Emacs-lisp ]--------

(add-hook 'emacs-lisp-mode-hook       'my-emacs-lisp-mode-hook)

(defun my-emacs-lisp-mode-hook nil
  (define-key emacs-lisp-mode-map "\C-c\C-r" 'comment-region)
  (define-key emacs-lisp-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key emacs-lisp-mode-map "\C-c\C-c" 'compile)

  ;(folding-mode)
  
  ;; f9-f12 shortcuts
  (local-set-key [f9]  'tempo-complete-tag)
  ;(tempo-use-tag-list  'emacs-tempo-tags) 
)

;;}}}

;;{{{ --[ calendar ]----------

;(defvar diary-file                      "~/.emacs.d/diario")
(defvar calendar-week-start-day         0)
(defvar european-style-calendar         t)
(defvar mark-diary-entries-in-calendar  t)
(defvar mark-holidays-in-calendar       t)
(defvar appt-display-mode-line          nil) ; se tiver um appointment mostra no modline.
(defvar calendar-latitude               -29.16) ; caxias do sul - RS
(defvar calendar-longitude              -51.18) ; [ 1 1 west]
(defvar calendar-location-name          "SAM|BR|BR019|CAXIAS DO SUL|")

(defvar calendar-day-name-array
  ["Domingo" "Segunda" "Terça" "Quarta" "Quinta" "Sexta" "Sábado"])
(defvar calendar-month-name-array
  ["Janeiro"   "Fevereiro"  "Março"     "Abril"
   "Maio"      "Junho"      "Julho"     "Agosto"
   "Setembro"  "Outubro"    "Novembro"  "Dezembro"])

;; remove some holidays
(defvar general-holidays nil)   ; get rid of too U.S.-centric holidays
(defvar hebrew-holidays nil)    ; get rid of religious holidays
(defvar islamic-holidays nil)   ; get rid of religious holidays
(defvar oriental-holidays nil)

(defvar local-holidays
      '(
	(holiday-fixed  1  1 "Ano Novo")
        (holiday-fixed  4 21 "Tiradentes")
        (holiday-fixed  6 12 "Dia dos Namorados")
        (holiday-fixed  6 15 "Corpus Christi")
        (holiday-fixed  9 7  "Proclamação da Independência")
        (holiday-fixed  9 20 "Revolução Farroupilha")
	(holiday-fixed 10 12 "Dia das crianças")
        (holiday-fixed 10 23 "Vacaria NAO TENHO CERTEZA")
        (holiday-fixed 11 1  "todos os Santos")
        (holiday-fixed 11 2  "Finados")
        (holiday-fixed 11 15 "Proclamação da república")
        (holiday-fixed 12 8  "Imaculada conceição")
        (holiday-fixed 12 21 "VERAO")
        (holiday-fixed 12 25 "Natal")

	;; feriados com datas variaveis
	(holiday-float 5 0 2 "Dia das Maes")
	(holiday-float 6 0 2 "Dia dos Pais")))

(defvar calendar-load-hook
      '(lambda()
         (european-calendar)
         (set-face-background 'diary-face           "black")
         (set-face-foreground 'diary-face           "red")
         (set-face-background 'holiday-face         "black")
         (set-face-foreground 'holiday-face         "green")
         (set-face-background 'calendar-today-face  "blue")
         (set-face-foreground 'calendar-today-face  "red")))

;(diary [0 2 2 2 2 2 2 0])

(add-hook 'diary-display-hook          'fancy-diary-display)
; change face of current date
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'list-diary-entries-hook     'sort-diary-entries t)
(add-hook 'diary-hook                  'appt-make-list)

;;}}} end calendar

;;{{{ --[ ispell ]------------
;(defvar ispell-program-name     "/home/amcorreia/bin/ispell")

(defun my-turn-on-flyspell-english ()
  "Unconditionally turn on Flyspell mode (in English)."
  (interactive)
  (flyspell-mode 1)
  (ispell-change-dictionary "english")
  (flyspell-buffer))

(defun my-turn-on-flyspell-portuguese ()
  "Unconditionally turn on Flyspell mode (in Portuguese)."
  (interactive)
  (defvar ispell-dictionary       "brasileiro")
  (defvar ispell-sort-corrections t)
  
  (defvar ispell-dictionary-alist
	(cons '("brasileiro" "[A-ZÁÉÍÑÓÚÜa-záéíñóúü]" "[^A-ZÁÉÍÑÓÚÜa-záéíñóúü]" "[---]" nil
		nil "~tex" iso-8859-1)  ispell-dictionary-alist))
  
  (flyspell-mode 1)
  (ispell-change-dictionary "brasileiro")
  (flyspell-buffer))

(defun my-turn-on-flyspell-spanish ()
  "Unconditionally turn on Flyspell mode (in Spanish)."
  (interactive)
  (message "Don't is available"))


;;}}}

;;}}} ---[ GNU-emacs ]
;;{{{ ----[ NON-GNU-emacs ]-----------------------------------------------------
;;{{{{ --[ smiley ]--------------
;; (eval-after-load "autosmiley"
;;   '(add-to-list 'smiley-regexp-alist	'("\\(\\[<o>]\\)" 1 "brasil")))
;; (require 'autosmiley "~/.emacs.d/site-lisp/misc/autosmiley")
;;}}}

;;{{{ --[ w3m ]---------------
(require 'w3m-load)
(require 'w3m)
(autoload 'w3m "w3m" "Navegador" t)
(autoload 'w3m-session "w3m-session")
(defvar w3m-home-page    "http://www.google.com")
(defvar w3m-use-cookies t)
(defvar w3m-default-display-inline-images t)
(defvar w3m-session-file "~/.emacs.d/w3m-sessions")

; (defun w3m-add-keys ()
;   (define-key w3m-mode-map "S" 'w3m-session-save)
;   (define-key w3m-mode-map "L" 'w3m-session-load))
; (add-hook 'w3m-mode-hook 'w3m-add-keys)


(defvar w3m-session-duplicate-tabs 'ask)
(defvar browse-url-browser-function 'w3m-browse-url)
(global-set-key "\C-xm" 'browse-url-at-point)

(eval-after-load "w3m-search"
  '(progn
     (add-to-list 'w3m-search-engine-alist
		  '("Google"
		    "http://www.google.com/search?q=%s"
		    nil))
     (add-to-list 'w3m-uri-replace-alist
		  '("\\`gg:" w3m-search-uri-replace "Google"))))

;;}}}

;; see: https://gist.github.com/amcorreia/10340725
(eval-when-compile
  (load-file "/home/amcorreia/.emacs.d/calendar-google.el"))
(load-file "/home/amcorreia/.emacs.d/calendar-google.el")
(require 'private-google-calendar)

(require 'calfw)
;(require 'calfw-howm)
(require 'calfw-org)
(require 'calfw-cal)
(require 'calfw-ical)
(cfw:open-ical-calendar my-ical-private-google)

;; Month
(defvar calendar-month-name-array
  ["Janeiro" "Fevereiro" "Março"     "Abril"   "Maio"      "Junho"
   "Julho"    "Agosto"   "Setembro" "Outubro" "Novembro" "Dezembro"])

;; Week days
(defvar calendar-day-name-array
      ["Domingo" "Segunda" "Terça" "Quarta" "Quinta" "Sexta" "Sábado"])

;; First day of the week
(defvar calendar-week-start-day 1) ; 0:Sunday, 1:Monday

 ;; Another unicode chars
(defvar cfw:fchar-junction ?╬)
(defvar cfw:fchar-vertical-line ?║)
(defvar cfw:fchar-horizontal-line ?═)
(defvar cfw:fchar-left-junction ?╠)
(defvar cfw:fchar-right-junction ?╣)
(defvar cfw:fchar-top-junction ?╦)
(defvar cfw:fchar-top-left-corner ?╔)
(defvar cfw:fchar-top-right-corner ?╗)
;; (defun my-open-calendar ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     (cfw:org-create-source "Green")  ; orgmode source
;;    ; (cfw:howm-create-source "Blue")  ; howm source
;;     (cfw:cal-create-source "Orange") ; diary source
;;     ;(cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
;;     ;(cfw:ical-create-source "gcal" "https://www.google.com/calendar/ical/.../basic.ics" "IndianRed") ; google calendar ICS
;;    )))

(require 'rainbow-delimiters)
(require 'mode-icons)
(mode-icons-mode)

(require 'gist)
(require 'grc)
;(require 'screen-lines)
;(require 'moinmoin-mode)
(require 'moomin)
(defvar moomin-wiki-url-base "http://wiki.zarathustra.com.br/MyWiki")

;;; ;; Assign keybind to 'helm-moomin and 'moomin-save-current-buffer as you like
(global-set-key (kbd "C-x w") 'helm-moomin)
(add-hook 'moinmoin-mode-hook
          (lambda ()
            (define-key moinmoin-mode-map (kbd "C-c C-c") 'moomin-save-current-buffer)))

;; ;;; WIKI
;; (require 'oddmuse)
;; (defvar oddmuse-directory "~/.emacs.d/config/oddmuse")

;; (defvar oddmuse-wikis (append '(("Zarathustra"
;;                                "http://wiki.zarathustra.com.br/MyWiki/FrontPage?action=edit&editor=text" utf-8)) oddmuse-wikis))
;; (defvar oddmuse-wikis  '("Zarathustra"
;;                                "http://wiki.zarathustra.com.br/MyWiki/FrontPage" utf-8))
;; (defvar oddmuse-get "")
;; (oddmuse-mode-initialize)
;(defvar  oddmuse-wikis    (append '(("TesteVia" "http://wikivia.domain.com.br/wikivia/index.php?title=P%C3%A1gina_principal&action=edit" utf-8)) oddmuse-wikis))


;; (defvar load-path (append '("~/.emacs.d/site-lisp/net/cvs-packages/pmwiki-mode/") load-path))
;; (require 'pmwiki-mode)

;; (defvar pmwiki-main-wiki-base-uri "http://wikivia.domain.com.br/wikivia/")
;; (defvar pmwiki-main-homepage-uri
;;       (concat pmwiki-main-wiki-base-uri "index.php/P%C3%A1gina_principal"))

;; ;;;;
;; (autoload 'wikipedia-mode "wikipedia-mode.el"
;;   "Major mode for editing documents in Wikipedia markup." t)

;; (add-to-list 'auto-mode-alist
;;    '("\\.wiki\\'" . wikipedia-mode))

;; flyspell
(defvar text-mode-hook (quote (#[nil "\300\301!\207" [flyspell-mode 1] 2] flyspell-buffer text-mode-hook-identify)))

;; (add-to-list 'auto-mode-alist '("wikivia\\.domain\\.com\\.br" . wikipedia-mode)) 
;; (require 'wikipedia)
;; (defun wikipedia-test-locally () (interactive) (url-retrieve "http://wikivia.domain.com.br/wikivia/" 'wikipedia-extract-article-text '("br" "brazil"))) 
;; ;;}}}
;; (eval-when-compile (defvar load-path (append '("~/.emacs.d/site-lisp/common") load-path))) (eval-after-load "buff-menu" '(require 'buff-menu+ "~/.emacs.d/site-lisp/common/buff-menu+"))
;;}}} --[ NON-GNU-emacs ]
;;}}}
;;{{{ ---[ GAMES ]--------------------------------------------------------------

;; (eval-when-compile
;;   (defvar load-path (append '("~/.emacs.d/site-lisp/games") load-path)))
;; (require 'sudoku "~/.emacs.d/site-lisp/games/sudoku")

;; (autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; ; sokoban
;; (autoload 'sokoban "sokoban"
;;   "Start a new game of Sokoban." t)
;; (autoload 'sokoban-mode "sokoban"
;;   "Play Sokoban in current buffer." t)

;; (defvar sokoban-levels-dir "~/.emacs.d/site-lisp/games/sokoban/sokoban-levels/")

; cell problem
;(require 'rlx "~/.emacs.d/site-lisp/games/rlx")

;;}}} --[ games ]
;;{{{ ---[ TESTE ]-------------

; adiciona um icone na tool-bar
;; (define-key global-map [tool-bar shell]
;;   '(menu-item "Shell" shell
;; 	      :image (image :type xpm :file "brasil.xpm")))

;(define-key-after 'Words [drink]
;  '("Drink" . drink-command) 'eat)

; Here is how to insert an item called `Work' in the `Signals' menu
; of Shell mode, after the item `break':
;; (define-key-after
;;   (lookup-key shell-mode-map [menu-bar signals])
;;   [work] '("Work" . work-command) 'break)

;; (define-key-after
;;   (lookup-key emacs-lisp-mode-map [menu-bar indent-line])
;;   [work] '("Work" . work-command) 'break)

;; para ler sequencia do teclado, senao souber como mapear no emacs
;(read-key-sequence "prompt: " t)

;;}}} [ TESTE ]
;; (eval-when-compile
;;   (defvar load-path (append '("~/.emacs.d/site-lisp/net/cvs-packages/chess"  ) load-path)))
;; (defvar load-path (append '("~/.emacs.d/site-lisp/net/cvs-packages/chess/") load-path))
;; (autoload 'chess "chess" "Play a game of chess" t)
;; (defvar chess-default-engine           'chess-gnuchess)
;; (defvar chess-display-highlight-legal  t)
;; ;;(defvar chess-message-language         'portuguese)
;; (defvar chess-full-name                "amcorreia")
;; (defvar chess-images-separate-frame    nil)
;; (defvar chess-images-default-size      21)
(require 'tramp)
(defvar tramp-persistency-file-name t)
(tramp-set-completion-function "ssh"
			       '((tramp-parse-sconfig "/etc/ssh_config")
				 (tramp-parse-sconfig "~/.ssh/config")))



(sit-for 2)
(message "done!")

;;; Local Variables: ***
;;; compile-command: "emacs --no-init-file --no-site-file -batch -f batch-byte-compile ~/.emacs.d/emacs-el/init.el" ***
;;; End: ***
