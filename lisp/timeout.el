;;; timeout.el --- Throttle and debounce functions for Emacs -*- lexical-binding: t -*-

;; Author: Based on Karthik Chikmagalur's article
;; URL: https://karthinks.com/software/cool-your-heels-emacs/
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;;; Commentary:
;; This package provides functions to throttle and debounce Emacs functions.
;; Throttling limits a function to run at most once within a specified time period.
;; Debouncing delays function execution until after a period of inactivity.

;;; Code:

(defun throttle--advice (&optional timeout)
  "Return advice that throttles a function for TIMEOUT seconds."
  (let ((throttle-timer nil)
        (timeout (or timeout 1.0))
        (result))
    (lambda (oldfun &rest args)
      "Throttle this function."
      (if (and throttle-timer (timerp throttle-timer))
          result
        (prog1 (setq result (apply oldfun args))
          (setq throttle-timer
                (run-with-timer
                 timeout nil
                 (lambda ()
                   (cancel-timer throttle-timer)
                   (setq throttle-timer nil)))))))))

(defun throttle! (func &optional timeout)
  "Throttle FUNC to run at most once every TIMEOUT seconds.
If TIMEOUT is 0, remove throttling."
  (if (and timeout (= timeout 0))
      (advice-remove func 'throttle)
    (advice-add func :around (throttle--advice timeout)
                '((name . throttle)
                  (depth . -99)))))

(defun debounce--advice (&optional delay default)
  "Return advice that debounces a function for DELAY seconds."
  (let ((debounce-timer nil)
        (delay (or delay 0.5)))
    (lambda (oldfun &rest args)
      "Debounce this function."
      (if (timerp debounce-timer)
          (timer-set-idle-time debounce-timer delay)
        (prog1 default
          (setq debounce-timer
                (run-with-idle-timer
                 delay nil
                 (lambda (buf)
                   (setq debounce-timer nil)
                   (with-current-buffer buf
                     (apply oldfun args)))
                 (current-buffer))))))))

(defun debounce! (func &optional delay)
  "Debounce FUNC to run after DELAY seconds of inactivity.
If DELAY is 0, remove debouncing."
  (if (and delay (= delay 0))
      (advice-remove func 'debounce)
    (advice-add func :around (debounce--advice delay)
                '((name . debounce)
                  (depth . -98)))))

(provide 'timeout)
;;; timeout.el ends here
