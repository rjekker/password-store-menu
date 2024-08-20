;;; password-store-menu.el --- A better, more complete UI for password-store -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Reindert-Jan Ekker <info@rjekker.nl>

;; Author: Reindert-Jan Ekker <info@rjekker.nl>
;; Maintainer: Reindert-Jan Ekker <info@rjekker.nl>
;; Version: 1.0.0
;; URL: https://github.com/rjekker/password-store-menu
;; Package-Requires: ((emacs "29.1") (password-store "2.3.2"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: convenience data files

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds a transient user interface for working with
;; pass ("the standard Unix password manager"); extending and improving
;; the user interface already implemented by password-store.el

;; For complete documentation, see https://github.com/rjekker/password-store-menu

;;; Code:

(require 'password-store)
(require 'transient)
(require 'vc)
(require 'epa)


(defcustom password-store-menu-edit-auto-commit t
  "Automatically commit edited password files to version control."
  :group 'password-store
  :type 'boolean)


(defcustom password-store-menu-key "C-c p"
  "Key to bind to the `password-store-menu' command.

This is used by the `password-store-menu-enable' command."
  :group 'password-store
  :type 'key)


(defconst password-store-menu--insert-buffer-name "*password-store-insert*")

;;; Public functions

;;;###autoload
(defun password-store-menu-view (entry)
  "Show the contents of the selected password file ENTRY."
  (interactive (list (password-store--completing-read)))
  (view-file (password-store--entry-to-file entry)))

;;;###autoload
(defun password-store-menu-browse-and-copy (entry)
  "Browse ENTRY using `password-store-url', and copy the secret to the kill ring."
  (interactive (list (password-store--completing-read)))
  (password-store-copy entry)
  (password-store-url entry))

;;;###autoload
(defun password-store-menu-dired ()
  "Open the password store directory in Dired."
  (interactive)
  (dired (password-store-dir)))

;;;###autoload
(defun password-store-menu-visit (entry)
  "Visit file for ENTRY."
  (interactive (list (password-store--completing-read)))
  (with-current-buffer
      (find-file (password-store--entry-to-file entry))
    (password-store-menu-edit-mode)))

;;;###autoload
(defun password-store-menu-pull ()
  "Pull password store files from version control."
  (interactive)
  (let ((default-directory (password-store-dir)))
    (vc-pull)))

;;;###autoload
(defun password-store-menu-push ()
  "Push password store files to version control."
  (interactive)
  (let ((default-directory (password-store-dir)))
    (vc-push)))

;;;###autoload
(defun password-store-menu-diff ()
  "Show vc diff for password store."
  (interactive)
  (vc-dir (password-store-dir)))

;;;###autoload
(defun password-store-menu-qr (entry)
  "Show QR for given password ENTRY."
  (interactive (list (password-store--completing-read t)))
  (password-store-menu--run-show-qr entry))

(defun password-store-menu--run-show-qr (entry &rest args)
  "Show QR code for ENTRY in a buffer.

This runs \"pass show --qrcode\" and adds all other ARGS."
  (let ((arguments (append (list entry "show" "--qrcode") args))
        (buf (generate-new-buffer "*password-store-qrcode*")))
    (make-process
     :name "password-store-gpg"
     :command (cons password-store-executable (delq nil arguments))
     :connection-type 'pipe
     :buffer buf
     :noquery t
     :sentinel (lambda (_process _event)
                 (with-current-buffer buf
                   (view-mode t))
                 (pop-to-buffer buf)))))


;;; Inserting new entries
;;;###autoload
(defun password-store-menu-insert (entry password)
  "Insert a new ENTRY containing PASSWORD.

This wraps `password-store-insert' with some code to read a new entry."
  (interactive (let ((entry (password-store-menu--completing-read-new-entry)))
                 (list entry
                       (if entry
                           (read-passwd "Password: " t)
                         nil))))
  (when entry (password-store-insert entry password)))


;;;###autoload
(defun password-store-menu-insert-multiline (entry)
  "Insert a multi-line password ENTRY."
  (interactive (list (password-store-menu--completing-read-new-entry)))
  (when entry
    (ignore-errors
      (kill-buffer password-store-menu--insert-buffer-name))
    (let ((buffer (get-buffer-create password-store-menu--insert-buffer-name)))
      (message "%s ""Please insert text for new pass entry, then press `C-c C-c' to save, or `C-c C-k' to cancel.")
      (with-current-buffer buffer
        (password-store-menu-insert-mode)
        (setq-local password-store-menu-new-entry entry))
      (pop-to-buffer buffer)
      "")))


(define-derived-mode password-store-menu-insert-mode text-mode "pass-insert"
  "Major mode for editing new password-store entries."
  (setq buffer-offer-save nil))


(defvar-keymap password-store-menu-insert-mode-map
  :parent text-mode-map
  "C-c C-c" #'password-store-menu--insert-save
  "C-c C-k" #'password-store-menu--kill-insert-buffer)


(defun password-store-menu--kill-insert-buffer (&optional force)
  "Kill buffer containing new pass entry.

Ask for confirmation unless FORCE is t."
  (interactive)
  (when (or force
            (yes-or-no-p "Cancel new pass entry?"))
    (kill-buffer password-store-menu--insert-buffer-name)))


(defun password-store-menu--insert-save ()
  "Save buffer to new password entry; kill the buffer."
  (interactive)
  (with-current-buffer (get-buffer password-store-menu--insert-buffer-name)
    (when (boundp 'password-store-menu-new-entry)
      (password-store-menu-insert password-store-menu-new-entry (buffer-string))))
  (password-store-menu--kill-insert-buffer t))


(defun password-store-menu--commit-on-save ()
  "Function to be called when saving changed password entries."
  (interactive)
  (when password-store-menu-edit-auto-commit
    (when-let ((backend (vc-responsible-backend (password-store-dir) t)))
      (let ((entry (password-store--file-to-entry (buffer-file-name))))
        (when (not (vc-registered (buffer-file-name)))
          (vc-register))
        (vc-call-backend backend 'checkin (list buffer-file-name)
                         (format "Edit password for %s using Emacs" entry) nil)))))

;;; Editing entries
(define-derived-mode password-store-menu-edit-mode text-mode "pass-edit"
  "Major mode for editing password-store entries, which auto-commits changes."
  (add-hook 'after-save-hook #'password-store-menu--commit-on-save nil t))


(defun password-store-menu--maybe-edit-mode ()
  "Start pass-edit mode, but only when we are in the password store."
  (when (file-in-directory-p (buffer-file-name) (password-store-dir))
    (password-store-menu-edit-mode)))


(defun password-store-menu--completing-read-new-entry ()
  "Prompt for name of new pass entry, ask confirmation if it exists."
  (let*
      ((entry (password-store--completing-read))
       (exists (file-exists-p (password-store--entry-to-file entry))))
    (when (or (not exists)
              (yes-or-no-p (format "Overwrite entry %s?" entry)))
      entry)))

;;; The actual transient UI
(transient-define-suffix password-store-menu--generate-run-transient
  (entry &optional password-length)
  "Generate a new password for ENTRY with PASSWORD-LENGTH.

Default PASSWORD-LENGTH is `password-store-password-length'."
  (interactive (list (password-store--completing-read)
                     (and current-prefix-arg
                          (abs (prefix-numeric-value current-prefix-arg)))))
  (let ((transient-length-arg nil)
        (args nil)
        (length nil))
    (dolist
        ;; filter length out of the argument list
        (arg (transient-args transient-current-command))
      (if (string-prefix-p "--" arg)
          (push arg args)
        (setq transient-length-arg arg)))
    ;; for the value of length, prefix argument takes precedence over transient arg
    (setq length (or password-length transient-length-arg password-store-password-length))
    (apply #'password-store--run-async `("generate" ,@args ,entry ,length))))

(defun password-store-menu--read-length (prompt initial-input history)
  "Read a number for the password length, or return default if input empty.

Arguments PROMPT, INITIAL-INPUT and HISTORY are passed to
transient--read-number."
  (let ((input (transient--read-number-N prompt initial-input history nil)))
    (if (string-equal input "")
        (int-to-string password-store-password-length)
      input)))

(transient-define-infix password-store-menu-generate-length ()
  "Password length: should always be set."
  :argument ""
  :key "l"
  :prompt "Password length: "
  :multi-value nil
  :always-read t
  :description "Length"
  :class 'transient-option
  :reader #'password-store-menu--read-length)

(transient-define-prefix password-store-menu-generate-transient ()
  "Generate new password using transient."
  :value `(nil nil nil ,(int-to-string password-store-password-length))
  :incompatible '(("--in-place" "--force"))
  [
   ("i" "In place" "--in-place")
   ("f" "Force overwrite" "--force")
   ("n" "No symbols" "--no-symbols")
   (password-store-menu-generate-length)
   ("g" "Generate" password-store-menu--generate-run-transient)])


;;;###autoload
(transient-define-prefix password-store-menu ()
  "Entry point for password store actions."
  ["Password Entry"
   ["Use"
    ("b" "Browse" password-store-url)
    ("c" "Copy Secret" password-store-copy)
    ("f" "Copy Field" password-store-copy-field)
    ("o" "Browse and copy" password-store-menu-browse-and-copy)
    ("v" "View" password-store-menu-view)
    ("q" "QR code" password-store-menu-qr)]
   ["Change"
    ("D" "Delete" password-store-remove)
    ("e" "Edit (visit file)" password-store-menu-visit)
    ("E" "Edit (pass command)" password-store-edit)
    ("i" "Insert password" password-store-menu-insert)
    ("I" "Insert multiline" password-store-menu-insert-multiline)
    ("g" "generate" password-store-menu-generate-transient :transient transient--do-exit)
    ("r" "Rename" password-store-rename)]
   ["VC" :if (lambda () (vc-responsible-backend (password-store-dir) t))
    ("V=" "Diff" password-store-menu-diff)
    ("Vp" "Pull" password-store-menu-pull)
    ("VP" "Push" password-store-menu-push)]
   ["Store"
    ("d" "Dired" password-store-menu-dired)]]
  [("!" "Clear secret from kill ring" password-store-clear)])

;;;###autoload
(defun password-store-menu-enable ()
  "Run this to setup `auto-mode-alist' and keybinding for `password-store-menu'."
  (interactive)
  (add-to-list 'auto-mode-alist (cons epa-file-name-regexp 'password-store-menu--maybe-edit-mode))
  (define-key global-map (kbd password-store-menu-key) #'password-store-menu))

(provide 'password-store-menu)
;;; password-store-menu.el ends here
