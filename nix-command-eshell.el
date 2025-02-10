;;; nix-command-eshell.el --- eshell support for nix-command -*- lexical-binding: t -*-

;; Author: Patryk Wychowaniec <pwychowaniec@pm.me>
;; Homepage: https://github.com/NixOS/nix-mode
;; Keywords: nix, tools, eshell

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Provides an eshell-aware `nix' command[1].
;;
;; Using this module causes `nix develop' and `nix shell' to extend the active
;; eshell session instead of spawning a separate `bash' process.
;;
;; This module also advices `eshell/exit' so that it exits the Nix environment
;; first and only the second call to `exit' actually closes the `eshell' buffer,
;; simulating a nested shell. Of course, if you didn't invoke `nix develop'
;; before, just one `exit' remains sufficient.
;;
;; Finally, it is recommended that you extend your eshell prompt to check
;; `nix-command-eshell-active-p' so that you know whether you're inside a Nix
;; environment or not, like:
;;
;;     (defun +eshell/prompt ()
;;       (concat
;;        (eshell/pwd)
;;        (if (nix-command-eshell-active-p) " | nix" "")
;;        " "))
;;
;; NOTE: If any of the functionalities provided here malfunctions, remember that
;;       you can always use `*' to avoid going through `eshell/nix' - that is,
;;       `*nix develop' will revert back to the original behavior and start a
;;       regular `bash' shell.
;;
;; [1] https://nixos.wiki/wiki/Nix_command

;;; Code:

(require 'eshell)
(require 'esh-mode)

(push "nix" eshell-complex-commands)

;;;###autoload
(defun eshell/nix (&rest args)
  "eshell-aware wrapper for the `nix' command."
  (let ((cmd (car args)))
    (if (and (or (string= "develop" cmd) (string= "shell" cmd))
             (not (member "-c" args)))
        (nix-command-eshell args)
      (throw 'eshell-replace-command
             (eshell-parse-command
              (concat (char-to-string eshell-explicit-command-char) "nix") args)))))

(defun nix-command-eshell-active-p ()
  "Return t if we're inside a `nix develop' or a `nix shell' subshell - useful
for customizing the prompt."
  (boundp 'nix-command-eshell--prev-env))

(defun nix-command-eshell (args)
  (make-local-variable 'process-environment)
  (nix-command-eshell--leave)
  (nix-command-eshell--spawn args))

(defun nix-command-eshell--spawn (args)
  (throw 'eshell-external
         (let* ((temp-file
                 (make-temp-file "nix"))
                (command
                 (append '("nix") args (list "--command" "sh" "-c" (format "export > %s" temp-file))))
                (proc
                 (make-process
                  :name "nix"
                  :buffer (current-buffer)
                  :command command
                  :filter 'eshell-interactive-process-filter
                  :sentinel 'nix-command-eshell--sentinel)))
           (process-put proc 'temp-file temp-file)
           (eshell-record-process-object proc)
           (eshell-record-process-properties proc)
           proc)))

(defun nix-command-eshell--sentinel (proc status)
  (let ((cmd (car (eshell-commands-for-process proc)))
        (buffer (process-buffer proc))
        (temp-file (process-get proc 'temp-file)))
    (when (and cmd (buffer-live-p buffer) (eq 0 (process-exit-status proc)))
      (with-current-buffer buffer
        (nix-command-eshell--enter (nix-command-eshell--parse temp-file))))
    (when (not (process-live-p proc))
      (delete-file temp-file)))
  (eshell-sentinel proc status))

(defun nix-command-eshell--parse (path)
  (let ((env '())
        (env-regex
         (rx "export "
             (group (one-or-more (or alpha ?_)))
             "=\""
             (group (zero-or-more (not "\""))))))
    (with-temp-buffer
      (insert-file-contents path)
      (while (search-forward-regexp env-regex nil t 1)
        (let ((env-name (match-string 1))
              (env-value (match-string 2)))
          (setq env (setenv-internal env env-name env-value nil)))))
    env))

(defun nix-command-eshell--enter (env)
  (setq-local nix-command-eshell--prev-env process-environment
              process-environment env)
  (nix-command-eshell--refresh))

(defun nix-command-eshell--leave ()
  (when (boundp 'nix-command-eshell--prev-env)
    (setq-local process-environment nix-command-eshell--prev-env)
    (makunbound 'nix-command-eshell--prev-env)
    (nix-command-eshell--refresh)))

(defun nix-command-eshell--leave-a (fn &rest args)
  (if (boundp 'nix-command-eshell--prev-env)
      (progn (nix-command-eshell--leave) ())
    (apply fn args)))

(advice-add 'eshell/exit :around 'nix-command-eshell--leave-a)

;; Most envvars get overwritten by doing `(setq-local process-environment ...)',
;; but some envvars are virtual and require special treatment to be refreshed.
(defun nix-command-eshell--refresh ()
  (eshell-set-path (getenv "PATH")))

(provide 'nix-command-eshell)
