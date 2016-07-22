(require 'package)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/rhtml/")
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(smex
		      textmate
                      ruby-compilation
                      php-mode
                      iedit
                      flymake-easy
                      flx-ido
                      yaml-mode
                      zenburn-theme
                      expand-region
                      yasnippet-bundle
                      sws-mode
                      jade-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)


;; general

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory "~/.emacs.d/oddmuse"
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

;; Set tabs to 4 spaces
(setq-default indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(setq standard-indent 4)
(setq tab-width 4)
(setq sgml-basic-offset 2)

;;smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;golang
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'php-mode-hook (lambda() (setq indent-tabs-mode nil
                                    tab-width 4
                                    c-basic-offset 4)))

;;php
(add-hook 'php-mode-hook (lambda ()
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))

(require 'yaml-mode)
(require 'sws-mode)
(require 'jade-mode)
(require 'rhtml-mode)
(require 'ace-jump-mode)

(define-key global-map (kbd "C-,") 'ace-jump-mode)
(define-key global-map (kbd "C-.") 'ace-jump-line-mode)

;; Textmate like fuzzy file locate and symbol lookup
;; Map to Super-t and Super-T. For the sake of Mac
;; terminal/iterm ssh user also mapped to Meta-t and
;; Meta-T
(textmate-mode)
(global-set-key (kbd "M-t") 'textmate-goto-file)
(global-set-key (kbd "M-T") 'textmate-goto-symbol)


;; ido on
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Turn off ido flex complete if the complete list is
;; exceed 2000. Emacs will freeze up otherwise.
(defvar af-ido-flex-fuzzy-limit (* 2000 5))
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                     af-ido-flex-fuzzy-limit)))
    ad-do-it))



;; Provid IntelliJ C-W style incremental selection base on sexp.
(global-set-key (kbd "M-+") 'er/expand-region)

;; Robust version of duplicate a line
(require 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'duplicate-line)

;; vi's o command
(require 'open-next-line)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)


;; load ruby-test-run
(require 'ruby-test-run)
(global-set-key (kbd "C-x r f") 'ruby-test-run-file)
(global-set-key (kbd "C-x r s") 'ruby-test-run-single)
(global-set-key (kbd "C-x r l") 'ruby-test-run-last)


;; load rails-toolbelt
(require 'rails-toolbelt)

(require 'flymake-json)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(font-use-system-font t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; yas/snippets
;; (setq yas/root-directory "~/.emacs.d/snippets")
;; (yas/load-directory yas/root-directory)

;; Setting rbenv path
(setenv "GOPATH" (concat (getenv "HOME") "/go"))
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/go/bin"
                      (cons (concat (getenv "HOME") "/.rbenv/shims")
                            (cons "/usr/local/bin"
                                  (cons (concat (getenv "HOME") "/.rbenv/bin")
                                        (cons (concat (getenv "HOME") "/go/bin")
                                              exec-path))))))


;; disable annoyed ring bell
(setq ring-bell-function 'ignore)
(setq js-indent-level 4)
(setq css-indent-offset 4)

;; auto scroll compilation window
(setq compilation-scroll-output 1)


;; style I want to use in c++ mode
(c-add-style "my-cpp-style"
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-cpp-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; increase font size
(set-face-attribute 'default nil :height 130 :family "hack")
(setq-default line-spacing 2)

(load-theme 'zenburn)
(turn-off-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'html-mode-hook (lambda () (flyspell-mode -1)))

(setq last-kbd-macro
   [M-right M-right ?\] left ?\' M-left ?\' left ?\[ left backspace backspace])

(server-start)
