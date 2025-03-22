;; Reset package system
(require 'package)
(setq package-check-signature nil)  ; Disable signature verification
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Make sure we have up-to-date package lists
(package-refresh-contents)

;; Install essential packages
(dolist (pkg '(use-package company ivy))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Tell user we've finished
(message "Basic installation complete. Now customize your init.el to load packages incrementally.")
