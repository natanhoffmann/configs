;;=======================================================================
;; Package Management: package.el & straight.el
;;=======================================================================
;; Initialize package.el and add MELPA, GNU, and Org repositories
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el with use-package by default
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;=======================================================================
;; Visual Enhancements & UI Configuration
;;=======================================================================
;; Set default theme
(load-theme 'manoj-dark t)

;; Enable global line and column number modes
(global-display-line-numbers-mode t)
(setq-default
 column-number-mode t
 size-indication-mode t)

;; Set mode line format, including percentage position
(defun my/update-percent-position ()
  "Update the percentage position in the mode line."
  (let ((size (float (buffer-size)))
        (point (float (point))))
    (format " %d%%" (if (= size 0) 0 (floor (* 100 point) size)))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                (line-number-mode "L%l ")
                (column-number-mode "C%c ")
                (:eval (my/update-percent-position))
                "   "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;;=======================================================================
;; Editing Behavior & Keybindings
;;=======================================================================
;; Bind Home and End keys to move to the beginning and end of lines
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; Function and keybinding for matching parentheses
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)

;; Custom function to split buffer into two files and keybinding
(defun my/two-file-split-buffer ()
  "Custom function to split the current buffer into two windows with different buffers."
  (interactive)
  (split-window-below)
  (call-interactively 'transpose-frame)
  (let ((new-buffer (car (last (buffer-list)))))
    (set-window-buffer (next-window) new-buffer)
    (other-window 1)
    (switch-to-buffer new-buffer t))
  (other-window 1))

(global-set-key (kbd "C-c m") 'my/two-file-split-buffer)

;;=======================================================================
;; Backup & Auto-Save Configuration
;;=======================================================================
;; Store backups and auto-save files in temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;=======================================================================
;; Package-Specific Configurations
;;=======================================================================
;; Use Which Key Mode for better discoverability of keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Centered Cursor Mode for programming, text, and org modes
(unless (boundp 'mouse-wheel-up-event)
  (defvar mouse-wheel-up-event 'mouse-4))
(unless (boundp 'mouse-wheel-down-event)
  (defvar mouse-wheel-down-event 'mouse-5))

(require 'centered-cursor-mode)
(add-hook 'prog-mode-hook 'centered-cursor-mode)
(add-hook 'text-mode-hook 'centered-cursor-mode)
(add-hook 'org-mode-hook 'centered-cursor-mode)

;; ;; Load cmake-mode
;; (require 'cmake-mode)

;; ;; Automatically enable CMake mode for CMake-related files
;; (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
;; (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;=======================================================================
;; Miscellaneous Settings
;;=======================================================================
;; Recognize .bashrc and similar files as Bash configuration files
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.natanh\\'" . shell-script-mode))

;; Add shfmt to the exec-path
(add-to-list 'exec-path "/home/natanh/misc/.local/opt/shfmt-v3.7.0/bin/shfmt")

;; Add lisp directory to load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Enable debugging on error
(setq debug-on-error t)
