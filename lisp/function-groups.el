;;; function-groups.el --- Group functions with shared hooks -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Jure Smolar <jure.smolar@gmail.com>
;; Keywords: extensions, lisp, tools
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a mechanism that can be considered the opposite of hooks.
;; While hooks let you add functions to run at specific points, function groups
;; let you specify a list of functions for which your function should run.
;;
;; Basic usage:
;;
;; 1. Define a function group:
;;    (define-function-group buffer-modification)
;;
;; 2. Advise functions to run the group's hook:
;;    (group-advise-functions buffer-modification :after save-buffer)
;;
;; 3. Add your functions to the hook:
;;    (add-hook 'buffer-modification-functions-hook 'my-function)
;;
;; The package provides a complete set of interactive commands to manage
;; function groups through the customize interface and command line.

;;; Code:

(defgroup function-groups nil
  "Group functions with shared hooks."
  :group 'extensions
  :prefix "function-group-")

(defcustom function-groups-list nil
  "List of defined function groups."
  :type '(repeat symbol)
  :group 'function-groups)

;;;###autoload
(defmacro define-function-group (group-name)
  "Define a hook that will run for functions in GROUP-NAME."
  `(progn
     (defvar ,(intern (format "%s-functions-hook" group-name))
       nil
       ,(format "Hook run for functions in the %s group." group-name))
     
     (defun ,(intern (format "%s-run-hook" group-name)) (&rest _)
       ,(format "Run the %s-functions-hook." group-name)
       (run-hooks ',(intern (format "%s-functions-hook" group-name))))
     
     (add-to-list 'function-groups-list ',group-name)))

;;;###autoload
(defmacro group-advise-functions (group-name when &rest functions)
  "Advise FUNCTIONS to run the hook for GROUP-NAME.
Said hook is named GROUP-NAME-functions-hook.
WHEN should be either :before or :after.
FUNCTIONS can be a single function, a list of functions, or a variable
that contains a list of functions."
  (let ((hook-runner (intern (format "%s-run-hook" group-name))))
    ;; Handle different types of arguments for FUNCTIONS
    (cond
     ;; If first arg is a list literal (quoted or not)
     ((and (= (length functions) 1) (listp (car functions)))
      `(progn
         ,@(mapcar (lambda (func)
                     `(advice-add ',func ,when #',hook-runner))
                   (car functions))))
     
     ;; If first arg is a variable name that might contain a list
     ((and (= (length functions) 1) (symbolp (car functions)))
      `(let ((funcs-to-advise ,(car functions)))
         (if (listp funcs-to-advise)
             ;; It's a variable containing a list
             (dolist (func funcs-to-advise)
               (advice-add func ,when #',hook-runner))
           ;; It's a single function
           (advice-add ',(car functions) ,when #',hook-runner))))
     
     ;; Multiple function names passed directly
     (t
      `(progn
         ,@(mapcar (lambda (func)
                     `(advice-add ',func ,when #',hook-runner))
                   functions))))))

;;;###autoload
(defmacro group-unadvise-functions (group-name when &rest functions)
  "Remove GROUP-NAME advice from FUNCTIONS.
WHEN should be either :before or :after.
FUNCTIONS can be a single function, a list of functions, or a variable
that contains a list of functions."
  (let ((hook-runner (intern (format "%s-run-hook" group-name))))
    ;; Handle different types of arguments for FUNCTIONS
    (cond
     ;; If first arg is a list literal (quoted or not)
     ((and (= (length functions) 1) (listp (car functions)))
      `(progn
         ,@(mapcar (lambda (func)
                     `(advice-remove ',func #',hook-runner))
                   (car functions))))
     
     ;; If first arg is a variable name that might contain a list
     ((and (= (length functions) 1) (symbolp (car functions)))
      `(let ((funcs-to-unadvise ,(car functions)))
         (if (listp funcs-to-unadvise)
             ;; It's a variable containing a list
             (dolist (func funcs-to-unadvise)
               (advice-remove func #',hook-runner))
           ;; It's a single function
           (advice-remove ',(car functions) #',hook-runner))))
     
     ;; Multiple function names passed directly
     (t
      `(progn
         ,@(mapcar (lambda (func)
                     `(advice-remove ',func #',hook-runner))
                   functions))))))

;;;###autoload
(defun group-list-advised-functions (group-name)
  "Return list of functions advised with GROUP-NAME hook."
  (let ((hook-runner (intern (format "%s-run-hook" group-name)))
        (advised-funcs nil))
    (mapatoms
     (lambda (symbol)
       (when (and (fboundp symbol)
                  (advice-member-p hook-runner symbol))
         (push symbol advised-funcs))))
    advised-funcs))

;;;###autoload
(defun group-function-advised-p (group-name function)
  "Check if FUNCTION has GROUP-NAME advice."
  (let ((hook-runner (intern (format "%s-run-hook" group-name))))
    (advice-member-p hook-runner function)))

;;;###autoload
(defun function-group-create (group-name)
  "Create a new function group named GROUP-NAME."
  (interactive "SEnter group name: ")
  (when (symbolp group-name)
    (eval `(define-function-group ,group-name))
    (message "Created function group: %s" group-name)))

;;;###autoload
(defun function-group-add-function (group-name when function)
  "Add FUNCTION to GROUP-NAME with WHEN advice."
  (interactive
   (let* ((group (intern (completing-read "Group: " function-groups-list)))
          (when-type (intern (completing-read "When (:before/:after): "
                                             '(":before" ":after"))))
          (func (intern (completing-read "Function: "
                                        obarray #'fboundp t))))
     (list group when-type func)))
  (eval `(group-advise-functions ,group-name ,when ,function))
  (message "Added %s to %s with %s advice" function group-name when))

;;;###autoload
(defun function-group-remove-function (group-name when function)
  "Remove FUNCTION from GROUP-NAME with WHEN advice."
  (interactive
   (let* ((group (intern (completing-read "Group: " function-groups-list)))
          (when-type (intern (completing-read "When (:before/:after): "
                                             '(":before" ":after"))))
          (advised-funcs (group-list-advised-functions group))
          (func (intern (completing-read "Function: " advised-funcs nil t))))
     (list group when-type func)))
  (eval `(group-unadvise-functions ,group-name ,when ,function))
  (message "Removed %s from %s with %s advice" function group-name when))

;;;###autoload
(defun function-group-list-functions (group-name when)
  "List all functions in GROUP-NAME with WHEN advice."
  (interactive
   (let* ((group (intern (completing-read "Group: " function-groups-list)))
          (when-type (intern (completing-read "When (:before/:after): "
                                             '(":before" ":after")))))
     (list group when-type)))
  (let ((functions (group-list-advised-functions group-name))
        (buffer (get-buffer-create "*Function Group Functions*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))  ;; Allow modification of read-only buffer
        (erase-buffer)
        (insert (format "Functions in group %s with %s advice:\n\n"
                        group-name when))
        (dolist (func functions)
          (insert (format "  - %s\n" func)))
        (special-mode)
        (goto-char (point-min))))
    (display-buffer buffer)))

;;;###autoload
(defun function-group-list-all-groups ()
  "List all defined function groups."
  (interactive)
  (let ((buffer (get-buffer-create "*Function Groups*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))  ;; Allow modification of read-only buffer
        (erase-buffer)
        (insert "Defined function groups:\n\n")
        (dolist (group function-groups-list)
          (insert (format "  - %s\n" group)))
        (special-mode)
        (goto-char (point-min))))
    (display-buffer buffer)))

;;;###autoload
(defun function-group-add-hook-function (group-name hook-function)
  "Add HOOK-FUNCTION to GROUP-NAME's hook."
  (interactive
   (let* ((group (intern (completing-read "Group: " function-groups-list)))
          (func (intern (completing-read "Hook function: " obarray #'fboundp t))))
     (list group func)))
  (let ((hook-var (intern (format "%s-functions-hook" group-name))))
    (add-hook hook-var hook-function)
    (message "Added %s to %s hook" hook-function hook-var)))

;;;###autoload
(defun function-group-remove-hook-function (group-name hook-function)
  "Remove HOOK-FUNCTION from GROUP-NAME's hook."
  (interactive
   (let* ((group (intern (completing-read "Group: " function-groups-list)))
          (hook-var (intern (format "%s-functions-hook" group)))
          (current-functions (symbol-value hook-var))
          (func (intern (completing-read "Hook function: "
                                        current-functions nil t))))
     (list group func)))
  (let ((hook-var (intern (format "%s-functions-hook" group-name))))
    (remove-hook hook-var hook-function)
    (message "Removed %s from %s hook" hook-function hook-var)))

(provide 'function-groups)
;;; function-groups.el ends here
