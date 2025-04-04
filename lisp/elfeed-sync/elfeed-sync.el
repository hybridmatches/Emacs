;;; elfeed-sync.el --- Multi-device syncing for Elfeed -*- lexical-binding: t -*-

;; Author: Jure Smolar
;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (elfeed "3.0.0") (timeout "1.0"))
;; Keywords: news

;;; Commentary:
;; This package provides optimized multi-device syncing for Elfeed using
;; throttling and debouncing techniques to improve performance.

;;; Code:

(require 'elfeed)
(require 'timeout)  ;; Our throttle/debounce implementation

;;; Core Variables
(defvar my/elfeed-db-modified nil
  "When non-nil, indicates the Elfeed database has unsaved modifications.")

(defvar my/elfeed-save-delay 15
  "Seconds to wait after modification before saving database.")

(defvar my/elfeed-load-throttle 5
  "Minimum seconds between database loads.")

(defvar my/elfeed-debug nil
  "When non-nil, enable verbose debugging messages for Elfeed sync.")

;;; Core Functions - Regular non-debounced versions
(defun my/elfeed-mark-db-modified (&rest _)
  "Mark database as modified and schedule a save."
  (when my/elfeed-debug
    (message "Elfeed: Database modification detected!"))
  (setq my/elfeed-db-modified t)
  (when my/elfeed-debug
    (message "Elfeed: Modified flag set to %s" my/elfeed-db-modified)))

(defun my/elfeed-save-if-modified ()
  "Save the database if it has unsaved changes.
This is the non-debounced direct version."
  (when my/elfeed-debug
    (message "Elfeed: Save triggered. Modified: %s" my/elfeed-db-modified))
  
  (when my/elfeed-db-modified
    (message "Elfeed: Saving database changes...")
    (elfeed-db-save)
    (setq my/elfeed-db-modified nil)
    (message "Elfeed: Database saved.")))

(defun my/elfeed-load-db ()
  "Load the database from disk if no unsaved changes exist.
This is the non-throttled direct version."
  (when my/elfeed-debug
    (message "Elfeed: Load DB called. Modified: %s" my/elfeed-db-modified))
  
  (unless my/elfeed-db-modified
    (message "Elfeed: Loading database from disk...")
    (elfeed-db-load)
    (when my/elfeed-debug
      (message "Elfeed: Database loaded, updating search buffer..."))
    
    (when-let ((buffer (get-buffer "*elfeed-search*")))
      (with-current-buffer buffer
        (elfeed-search-update t))) ; Force update
    
    (message "Elfeed: Database loaded.")))

;; No timer cancellation function needed - we'll just use direct saves when needed

;;; Setup hooks for buffer activation
(defun my/elfeed-setup-local-activation-hooks ()
  "Set up buffer-local hooks for database reloading on activation."
  (when my/elfeed-debug
    (message "Elfeed: Setting up local activation hooks in buffer: %s" 
             (current-buffer)))
  
  ;; Using throttled version of load-db for these hooks
  (add-hook 'focus-in-hook #'my/elfeed-load-db-throttled nil t)
  (add-hook 'tab-bar-tab-post-select-functions 
            (lambda (&rest _) 
              (when my/elfeed-debug
                (message "Elfeed: Tab selection triggered db load"))
              (my/elfeed-load-db-throttled))
            nil t)
  
  (when my/elfeed-debug
    (message "Elfeed: Local activation hooks installed.")))

;;; Setup and Hooks
(defun my/elfeed-setup-sync ()
  "Setup database synchronization by attaching to core DB functions."
  (when my/elfeed-debug
    (message "Elfeed: Setting up sync system..."))
  
  ;; Monitor actual database modification functions
  (advice-add 'elfeed-tag :after #'my/elfeed-mark-db-modified)
  (advice-add 'elfeed-untag :after #'my/elfeed-mark-db-modified)
  (advice-add 'elfeed-db-add :after #'my/elfeed-mark-db-modified)
  
  ;; Create and set up debounced save function
  (defalias 'my/elfeed-save-if-modified-debounced 'my/elfeed-save-if-modified
    "Debounced version of `my/elfeed-save-if-modified'.")
  (debounce! 'my/elfeed-save-if-modified-debounced my/elfeed-save-delay)
  
  ;; Create and set up throttled load function
  (defalias 'my/elfeed-load-db-throttled 'my/elfeed-load-db
    "Throttled version of `my/elfeed-load-db'.")
  (throttle! 'my/elfeed-load-db-throttled my/elfeed-load-throttle)
  
  ;; Load on entering/focusing Elfeed - use throttled version
  (advice-add 'elfeed :before #'my/elfeed-load-db-throttled)
  
  ;; Add the local hooks to elfeed modes
  (add-hook 'elfeed-search-mode-hook #'my/elfeed-setup-local-activation-hooks)
  (add-hook 'elfeed-show-mode-hook #'my/elfeed-setup-local-activation-hooks)
  
  ;; Ensure save on exit uses direct function
  (advice-add 'elfeed-search-quit-window :before #'my/elfeed-save-if-modified)
  
  ;; Save on Emacs exit too
  (add-hook 'kill-emacs-hook #'my/elfeed-save-if-modified)
  
  ;; Hook database modifications to the debounced save
  (advice-add 'my/elfeed-mark-db-modified :after 
              (lambda (&rest _) 
                (my/elfeed-save-if-modified-debounced)))
  
  (when my/elfeed-debug
    (message "Elfeed: Setup complete!")))

;; Command to manually trigger a save
(defun my/elfeed-manual-save ()
  "Manually save the Elfeed database, regardless of the modified flag."
  (interactive)
  (message "Elfeed: Manual save requested")
  (elfeed-db-save)
  (setq my/elfeed-db-modified nil)
  (message "Elfeed: Database manually saved."))

;; Command to force reload the database
(defun my/elfeed-force-reload ()
  "Force reload the Elfeed database from disk."
  (interactive)
  (if my/elfeed-db-modified
      (when (yes-or-no-p "Unsaved changes exist. Load anyway and lose changes? ")
        (setq my/elfeed-db-modified nil)
        (my/elfeed-load-db))
    (my/elfeed-load-db)))

;; Command to show sync status
(defun my/elfeed-sync-status ()
  "Display the current status of the Elfeed sync system."
  (interactive)
  (message "Elfeed Status: Modified: %s" my/elfeed-db-modified))

;; Command to toggle debug mode
(defun my/elfeed-toggle-debug ()
  "Toggle verbose debug messages for Elfeed sync."
  (interactive)
  (setq my/elfeed-debug (not my/elfeed-debug))
  (message "Elfeed: Debug mode %s" 
           (if my/elfeed-debug "enabled" "disabled")))

;; Initialize the system
(my/elfeed-setup-sync)

(provide 'elfeed-sync)
;;; elfeed-sync.el ends here
