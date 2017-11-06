;; taking ideas from https://github.com/jkitchin/scimax
;;
(defconst my-emacs-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where my-emacs is installed.")

(defvar my-emacs-personal-dir (expand-file-name "personal" my-emacs-dir)
  "Personal directory for personal code.")

(setq user-emacs-directory my-emacs-dir)

(setq package-user-dir (expand-file-name "elpa"  my-emacs-dir))

;; Initialize the emacs packaging system
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-refresh-contents)

(package-install 'org-plus-contrib)

(package-install 'use-package)

(org-babel-load-file  (expand-file-name "config.org" my-emacs-dir))
