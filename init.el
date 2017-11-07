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

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (and (package-installed-p 'use-package)
	     (package-installed-p 'org-plus-contrib))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'org-plus-contrib))

;; From http://www.emacswiki.org/emacs/LoadPath#AddSubDirectories
;; to add a directory and its subdirectories
(let ((default-directory (expand-file-name "site-lisp" my-emacs-personal-dir)))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(org-babel-load-file  (expand-file-name "config.org" my-emacs-dir))
