;;; emacs-gpt.el --- ChatGPT client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Yusuke Watanabe

;; Author: Yusuke Watanabe <ywata1989@gmail.com>
;; Version: 1.0
;; Keywords: gpt, language
;; URL: https://github.com/ywatanabe/emacs-gpt
;; License: MIT
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package defines a set of functions and variables for running
;; instruction-following language models like ChatGPT and GPT-4.

;;; Code:

(defvar gpt-script-path (expand-file-name "emacs-gpt.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(defvar gpt-openai-engine "gpt-4"
  "The OpenAI engine to use.")

(defvar gpt-openai-max-tokens "2000"
  "The max_tokens value used with OpenAI engine.")

(defvar gpt-openai-temperature "0"
  "The temperature value used with OpenAI completion engine.")

(defvar gpt-openai-key "NOT SET"
  "The OpenAI API key to use.")

(defvar gpt-openai-use-chat-api t
  "If non-nil, use the chat completion API.  Otherwise, use the prompt completion API.")


(defun gpt-run-buffer (buffer task-type) ; task-type should be optional
  "Run GPT command with BUFFER text as input and append output stream to output-buffer."
  (with-current-buffer buffer
    (goto-char (point-max))
    (font-lock-fontify-buffer)
    (let* ((prompt-file (gpt-create-prompt-file buffer))
           (process (gpt-start-process prompt-file buffer task-type))
           (timer (gpt-start-timer process)))
      (gpt-set-process-sentinel process timer prompt-file)
      (message "GPT: Running...")
      (font-lock-fontify-buffer))))

(define-derived-mode gpt-mode text-mode "GPT"
  "Major mode for GPT.

\\{gpt-mode-map}")



;; if initial-input is f, return Fix
;; (defun gpt-select-task-type (&optional initial-input)
;;   "Prompt the user to select a task type for the GPT model after a short delay.
;; If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
;;   (interactive)
;;   (unless (minibufferp)
;;     (let ((task-type (read-string "Enter task type (f)ix / (r)efactor /(s)ciwrite / (c)orrect / (d)ocstring or custom: " initial-input)))
;;       (if (not (string-empty-p task-type))
;;           task-type
;;         (message "No task type selected; defaulting to custom.")
;;         "custom"))))

(defun gpt-select-task-type (&optional initial-input)
  "Prompt the user to select a task type for the GPT model after a short delay.
If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
  (interactive)
  (unless (minibufferp)
    (let ((task-type (read-string "Enter task type (f)ix / (r)efactor /(s)ciwrite / (c)orrect / (d)ocstring or custom: " initial-input)))
      (cond
       ((string= task-type "f") "Fix")
       ((string= task-type "r") "Refactor")
       ((string= task-type "s") "Sciwrite")
       ((string= task-type "c") "Correct")
       ((string= task-type "d") "Docstring")                     
       ((not (string-empty-p task-type)) task-type)
       (t
        (message "No task type selected; defaulting to custom.")
        "custom")))))




;; (add-hook 'gpt-mode-hook (lambda () (ivy-mode -1)))
;; (add-hook 'gpt-mode-hook (lambda () (auto-composition-mode -1)))
;; (add-hook 'gpt-mode-hook (lambda () (column-number-mode -1)))
;; (add-hook 'gpt-mode-hook (lambda () (counsel-mode -1)))
;; (add-hook 'gpt-mode-hook (lambda () (cua-mode -1)))

(defun gpt-on-region (beg end)
  "Run GPT command on region between BEG and END."
  (interactive "r")
  ;; (ivy-mode -1)
  ;; (auto-composition-mode -1)
  ;; (consel-mode - 1)
  ;; (cua-mode -1)
  (let* ((region-text (buffer-substring-no-properties beg end))
         (task-type (gpt-select-task-type))
         (buffer (get-buffer-create "*GPT*"))
         (text (concat task-type "\n\n" region-text)))
    (with-current-buffer buffer
      (unless (eq major-mode 'gpt-mode)
        (gpt-mode))
      (erase-buffer)
      (insert text)
      (gpt-truncate-history))
    ;; If the task type is nil or empty, don't run the buffer
    (unless (string-empty-p task-type)
      (gpt-run-buffer buffer task-type)
      (display-buffer buffer))))


(defun gpt-start-process (prompt-file output-buffer &optional task-type)
  "Start the GPT process with the given PROMPT-FILE and OUTPUT-BUFFER.
Use `gpt-script-path' as the executable and pass the other arguments as a list."
  (let* (
         (task-type (or task-type "")) ; Set default value of task-type to ""         
         (history-file (expand-file-name "history.json" (file-name-directory gpt-script-path)))
         (process (start-process "gpt-process" output-buffer
                                 "python"
                                 gpt-script-path
                                 gpt-openai-key
                                 gpt-openai-engine
                                 gpt-openai-max-tokens
                                 gpt-openai-temperature
                                 (if gpt-openai-use-chat-api "chat" "prompt")
                                 prompt-file
                                 history-file
                                 task-type
                                 )))
    process))

(defun gpt-start-timer (process)
  "Start a timer that checks if PROCESS is running every second."
  (run-with-timer 0 1 'gpt-check-process process))

(defun gpt-check-process (process)
  "Check if PROCESS is running and message the user if it has finished."
  (unless (process-live-p process)
    (message "GPT: Finished running command.")))

(defun gpt-set-process-sentinel (process timer prompt-file)
  "Set PROCESS sentinel to delete TIMER and PROMPT-FILE when process finishes."
  (set-process-sentinel process (lambda (_process _event)
                                  (cancel-timer timer)
                                  (delete-file prompt-file)
                                  (let ((buffer (process-buffer process)))
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                      (when (re-search-backward "^============================================================\n\nGPT" nil t)
                                        (beginning-of-line))
                                      (let ((point (point)))
                                        (when-let ((window (get-buffer-window buffer 0)))
                                          (set-window-point window point)
                                          (with-selected-window window
                                            (recenter 0)))))))))

(defun gpt-create-prompt-file (buffer)
  "Create a prompt file with the text in BUFFER and return its file name."
  (let ((file (make-temp-file "gpt-prompt-" nil ".txt")))
    (with-temp-file file
      (insert-buffer-substring buffer))
    file))

;; (defun gpt-on-region (beg end)
;;   "Run GPT command on region between BEG and END."
;;   (interactive "r")
;;   (let* ((region-text (buffer-substring-no-properties beg end))
;;          (task-type (gpt-select-task-type))
;;          (buffer (get-buffer-create "*GPT*"))
;;          (text (concat task-type "\n\n" region-text))
;;          )
;;     (with-current-buffer buffer
;;       (unless (eq major-mode 'gpt-mode)
;;         (gpt-mode))
;;       (erase-buffer)
;;       (insert text)
;;       (gpt-truncate-history)
;;       (gpt-run-buffer buffer task-type))

;;     (display-buffer buffer)
;;     ;; (copy-last-gpt-output-to-kill-ring)              
;;     ))




(defun copy-last-gpt-output-to-kill-ring ()
  "Copy the last GPT output from the *GPT* buffer to the kill ring."
  (interactive)
  (with-current-buffer "*GPT*"
    (goto-char (point-max))
    (let ((delimiter "============================================================\n"))
      (if (search-backward delimiter nil t)
          (if (search-backward delimiter nil t)              
              (progn
                (forward-line 1)
                (let ((start (point))
                      (end (point-max)))
                  ;; Calculate the position to remove the last three lines
                  (let* ((lines (split-string (buffer-substring-no-properties start end) "\n"))
                         (lines-to-keep (- (length lines) 3)))
                    (setq output (mapconcat 'identity (seq-subseq lines 0 lines-to-keep) "\n")))
                  (kill-new output)
                  (message "Output copied to kill ring."))))))))

(defun gpt-truncate-history ()
  "Truncate the GPT conversation history to the latest 5 entries."
  (interactive)
  (let* ((history-file (expand-file-name "history.json" (file-name-directory gpt-script-path))))
    (when (file-exists-p history-file)
      (call-process "python" nil nil nil "-c"
                    (concat "import json; "
                            "with open('" history-file "', 'r') as file: "
                            "history = json.load(file); "
                            "history = history[-5:]; "
                            "with open('" history-file "', 'w') as file: "
                            "json.dump(history, file)")))))

(defun gpt-clear-history ()
  "Clear the GPT conversation history file (history.json)."
  (interactive)
  (let* ((history-file (expand-file-name "history.json" (file-name-directory gpt-script-path)))
         (gpt-buffer (get-buffer "*GPT*")))
    (when gpt-buffer
      (kill-buffer gpt-buffer))
    (when (file-exists-p history-file)
      (delete-file history-file))
    (message "GPT conversation history file cleared.")))

(defun gpt-quit ()
  "Quit the GPT buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hide-gpt-buffer ()
  (interactive)
  (when (get-buffer "*GPT*")
    (delete-window (get-buffer-window "*GPT*"))))

;; (define-key gpt-mode-map (kbd "C-S-k") 'hide-gpt-buffer)
(define-key gpt-mode-map (kbd "q") nil)


(provide 'emacs-gpt)

;;; emacs-gpt.el ends here

