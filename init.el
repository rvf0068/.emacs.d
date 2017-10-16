(defconst scimax-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where the scimax is installed.")

(defvar scimax-user-dir (expand-file-name "user" scimax-dir)
  "User directory for personal code.")

(setq user-emacs-directory scimax-user-dir)

(setq package-user-dir (expand-file-name "elpa" scimax-dir))


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives 
'("melpa" . "https://melpa.org/packages/")) 
(package-initialize) 
;; Bootstrap `use-package' 
(unless (package-installed-p
'use-package) (package-refresh-contents) (package-install
'use-package))

(require 'ido)
(ido-mode t)

(visual-line-mode 1)
