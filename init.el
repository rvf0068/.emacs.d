;; taking ideas from https://github.com/jkitchin/scimax
;;
(defconst my-emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where my-emacs is installed.")

(defvar my-emacs-personal-dir (expand-file-name "personal" my-emacs-dir)
  "Personal directory for personal code.")

(setq user-emacs-directory my-emacs-dir)

(setq package-user-dir (expand-file-name "elpa"  my-emacs-dir))

;; From https://github.com/DarwinAwardWinner/dotemacs/blob/master/init.el
;;
;; Initialize the emacs packaging system
(require 'package)
(package-initialize)

;; Ensure latest org-mode is installed from
;; http://orgmode.org/elpa.html
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun package-ensure (pkg &optional dont-select)
  "Ensure PKG is installed, refreshing if necessary."
  (unless (package-installed-p pkg)
    (unless (memq pkg (mapcar #'car package-archive-contents))
      (package-refresh-contents))
    (package-install pkg dont-select)))

(condition-case nil
    (package-ensure 'org-plus-contrib)
  (error (display-warning "Could not install latest org-mode. Falling back to bundled version.")))

(condition-case nil
    (package-ensure 'use-package)
  (error (display-warning "Could not install use-package.")))

(save-window-excursion 
  (org-babel-load-file 
   (expand-file-name "config.org" my-emacs-dir)))
