;;=======================================================================
;; 1. Package Management: package.el & straight.el
;;=======================================================================
(require 'package)

;; Initialize package archives
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
;; 2. User Interface & Visual Enhancements
;;=======================================================================
;; Set default theme
(load-theme 'manoj-dark t)

;; Enable global line and column number modes
(global-display-line-numbers-mode t)
(setq-default
 column-number-mode t
 size-indication-mode t)

;; Mode line customizations
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
;; 3. Core Functionality
;;=======================================================================
;; Keybindings
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; Matching parentheses function and keybinding
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "%" 'match-paren)

;; Custom buffer split function and keybinding
(defun my/two-file-split-buffer ()
  "Split the current buffer into two windows with different buffers."
  (interactive)
  (split-window-below)
  (call-interactively 'transpose-frame)
  (let ((new-buffer (car (last (buffer-list)))))
    (set-window-buffer (next-window) new-buffer)
    (other-window 1)
    (switch-to-buffer new-buffer t))
  (other-window 1))

(global-set-key (kbd "C-c m") 'my/two-file-split-buffer)

;; Enable EditorConfig mode for consistency across projects
(editorconfig-mode 1)


;;=======================================================================
;; 4. Language-Specific Configuration
;;=======================================================================
;; C/C++ Indentation Preferences
(defun my-c-cpp-style ()
  "Set indentation preferences for C/C++ style files."
  (setq indent-tabs-mode nil)    ;; Use spaces instead of tabs
  (setq tab-width 4)             ;; Set tab width to 4 spaces
  (setq c-basic-offset 4))       ;; Set C/C++ indentation to 4 spaces

(add-hook 'c-mode-hook 'my-c-cpp-style)
(add-hook 'c++-mode-hook 'my-c-cpp-style)

;; Associate `.hpp` and `.h` with C++ mode
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Formatters for Different Modes
(setq format-all-formatters
      '((c-mode . "clang-format")
        (c++-mode . "clang-format")
        (python-mode . "black")
        (fortran-mode . "fprettify")
        (sh-mode . "shfmt")))

(defun my/indent-and-format ()
  "Indent the current line and then format the buffer."
  (interactive)
  (indent-for-tab-command)  ;; Perform the default TAB action (indentation)
  (format-all-buffer))      ;; Then format the buffer

(global-set-key (kbd "TAB") 'my/indent-and-format)


;;=======================================================================
;; 5. External Tools & Integrations
;;=======================================================================
;; Use Which Key for discoverable keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Enable Centered Cursor Mode for programming and text modes
(require 'centered-cursor-mode)
(add-hook 'prog-mode-hook 'centered-cursor-mode)
(add-hook 'text-mode-hook 'centered-cursor-mode)
(add-hook 'org-mode-hook 'centered-cursor-mode)

;; Load cmake-mode and automatically associate CMake files
(require 'cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; Add `shfmt` to exec-path
(add-to-list 'exec-path "/home/natanh/misc/.local/opt/shfmt-v3.7.0/bin/shfmt")


;;=======================================================================
;; 6. Miscellaneous Settings
;;=======================================================================
;; Add lisp directory to load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Store backups and auto-save files in temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable debugging on error
(setq debug-on-error t)

