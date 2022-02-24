;;; dired-hist.el --- Traverse dired history. -*- lexical-binding: t -*-
;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.10
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; URL: https://github.com/karthink/dired-hist

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; dired-hist is a minor mode for Emacs that keeps track of visited
;; dired buffers and lets you go back and forwards across them. This is
;; similar to the facility provided in other Emacs major modes, such as
;; Info and EWW.
;;
;; Commands:
;;
;; `dired-hist-mode'       : Turn on dired history tracking
;; `dired-hist-go-back'    : Go back in dired history
;; `dired-hist-go-forward' : Go forward in dired history
;;
;; Usage:
;;
;; Bind the two history commands to suitable keybindings in dired-mode and call
;; `dired-hist-mode':
;;
;;   (define-key dired-mode-map "l" #'dired-hist-go-back)
;;   (define-key dired-mode-map "r" #'dired-hist-go-forward)
;;   (with-eval-after-load 'dired (dired-hist-mode 1))
;;    
;; Customization:
;;
;; There are no customization options at this time.
;;
;;; Code:
(require 'dired)

(defvar dired-hist-stack nil
  "A stack of previously visited dired buffers.")

(defvar dired-hist-forward-stack nil
  "Forward history of previously visited dired buffers.")

(defun dired-hist-go-back ()
  "Go backward in the visited dired buffer history."
  (interactive)
  (when (and dired-hist-stack
             (equal (cdr-safe (car dired-hist-stack))
                    default-directory))
    (pop dired-hist-stack))
  (when dired-hist-stack
    (push (cons (point-marker) default-directory)
          dired-hist-forward-stack)
    (dired-hist--visit (car dired-hist-stack))))

(defun dired-hist-go-forward ()
  "Go forward in the visited dired buffer history."
  (interactive)
  (when dired-hist-forward-stack
    (dired-hist--update)
    (dired-hist--visit (pop dired-hist-forward-stack))))

(defun dired-hist--visit (item)
  (let* ((last-buffer (marker-buffer (car item)))
         (alive-p (buffer-live-p last-buffer))
         (win (and alive-p
                   (get-buffer-window last-buffer))))
    (cond
     (win (select-window win))
     (alive-p (switch-to-buffer last-buffer))
     (t (dired (cdr item))))))

(defun dired-hist--update ()
  (unless (equal default-directory (cdr-safe (car-safe dired-hist-stack)))
    (push (cons (point-marker) default-directory) dired-hist-stack)))

;;;###autoload
(define-minor-mode dired-hist-mode
  "Keep track of visited dired buffers and switch between them."
  :global t
  :lighter nil
  (if dired-hist-mode
      (add-hook 'dired-mode-hook #'dired-hist--update)
    (remove-hook 'dired-mode-hook #'dired-hist--update)
    (setq dired-hist-stack nil
          dired-hist-forward-stack nil)))

(provide 'dired-hist)
;;; dired-hist.el ends here
