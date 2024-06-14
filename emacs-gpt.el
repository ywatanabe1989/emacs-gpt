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
(defvar gpt-process nil "Process object for the GPT process.")

(define-derived-mode gpt-mode text-mode "GPT"
  "Major mode for GPT interactions.")

(defun gpt-create-buffer-file (buffer)
  "Create a prompt file with the text in BUFFER and return its file name."
  (let ((file (make-temp-file "gpt-prompt-" nil ".txt")))
    (with-temp-file file
      (insert-buffer-substring buffer))
    file))

(defvar gpt-buffer-file
  (gpt-create-buffer-file (get-buffer-create "*GPT*"))
  "Buffer for running GPT outputs.")

(defvar gpt-home-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The home directory of emacs-gpt.el.")

(defvar gpt-python-script-path (concat gpt-home-dir "emacs-gpt.py")
  "Path to the Python script used by emacs-gpt.el.")

(defvar gpt-python-bin-path (concat gpt-home-dir "env/bin/python")
  "Path to the Python binary used by emacs-gpt.el.")

(defvar gpt-templates-dir (concat gpt-home-dir "templates/")
  "The path to the Python script used byemacs-gpt.el.")

(defvar gpt-history-path (concat gpt-home-dir "history.json")
  "Path to the history file used by emacs-gpt.el.")

(defvar gpt-openai-engine "gpt-4"  
  "The OpenAI engine to use.")

(defvar gpt-openai-max-tokens "2000"
  "Maximum number of tokens used with OpenAI engine.")

(defvar gpt-openai-n-history "5"
  "Number of history entries to keep.")

(defvar gpt-openai-temperature "0"
  "Temperature setting used with OpenAI engine.")

(defvar gpt-openai-key (getenv "OPENAI_API_KEY")
  "The OpenAI API key to use.")


(defun fetch-capital-templates (dir)
  "Return list of templates file names that start with capital letters in `dir`."
  (when (file-exists-p dir)
    (let ((files (directory-files dir nil "^[A-Z].*\\.md$")))
      (mapcar (lambda (f) (substring f 0 (string-match "\\.md" f))) files))))

(defun generate-shortcuts (templates)
  "Generate shortcuts for a list of TEMPLATES."
  (let ((shortcuts (make-hash-table :test 'equal)))
    (dolist (template templates)
      (let* ((name (car template))
             (first-char (downcase (substring name 0 1))))
        (if (gethash first-char shortcuts)
            ;; If the first character is already taken, find the next available character
            (let ((suffix 1)
                  (new-char first-char))
              (while (gethash new-char shortcuts)
                (setq new-char (concat first-char (number-to-string suffix)))
                (setq suffix (1+ suffix)))
              (puthash new-char name shortcuts))
          (puthash first-char name shortcuts))))
    shortcuts))

(defun gpt-select-task-type (&optional initial-input)
  "Prompt the user to select a task type for the GPT model.
If INITIAL-INPUT is non-nil, return it without prompting."
  (interactive)
  (if initial-input
      initial-input
    (unless (minibufferp)
      (let* ((capital-templates (fetch-capital-templates gpt-templates-dir))
             (default-templates '("Fix" "Implement" "Refactor" "SciWrite" "Correct" "Docstring" "Email" "Remember"))
             (all-templates (append default-templates capital-templates))
             (templates-with-shortcuts (mapcar (lambda (template) (cons template template)) all-templates))
             (shortcuts (generate-shortcuts templates-with-shortcuts))
             (shortcut-list (hash-table-keys shortcuts))
             (prompt (concat "Enter task type (" (mapconcat (lambda (key) (concat key " for " (gethash key shortcuts))) shortcut-list ", ") "): "))
             (input (completing-read prompt (append (hash-table-values shortcuts) shortcut-list) nil t initial-input))
             (task-type (or (gethash input shortcuts) input)))
        (if (member task-type all-templates)
            task-type
          (if (not (string-empty-p task-type))
              task-type
            "custom"))))))

(defun gpt-construct-command (prompt &optional task-type)
  "Construct the complete command string for starting the GPT Python process."
  (interactive "sEnter prompt: ")
  (let ((task-type (gpt-select-task-type task-type)))
    ;; (message "Debug: Task type in construct-command is %s" task-type) ;; Debug message
    (let ((command (format "%s %s --api_key %s --engine %s --max_tokens %s --temperature %s --history_file %s --n_history %s --task_type %s --prompt %s"
                           (shell-quote-argument gpt-python-bin-path)
                           (shell-quote-argument gpt-python-script-path)
                           (shell-quote-argument gpt-openai-key)
                           (shell-quote-argument gpt-openai-engine)
                           (shell-quote-argument gpt-openai-max-tokens)
                           (shell-quote-argument gpt-openai-temperature)
                           (shell-quote-argument gpt-history-path)
                           (shell-quote-argument gpt-openai-n-history)
                           (shell-quote-argument task-type)
                           (shell-quote-argument prompt))))
      command)))

(defun gpt-process-sentinel (process msg)
  "Custom sentinel for the GPT process. Ignores the finished message."
  (unless (string-match-p "finished\\|exited" msg) ; Also consider 'exited' messages
    (message "GPT Process: %s" msg)))

(defun gpt-start-python-process (prompt &optional task-type)
  "Start the GPT process with the given PROMPT using a shell command."
  (interactive "sEnter prompt: ")
  (let* ((command (gpt-construct-command prompt task-type))
         (process (start-process-shell-command "gpt-process" "*GPT*" command)))
    (setq gpt-process process)
    (set-process-sentinel process #'gpt-process-sentinel)
    ;; Debugging the command can be toggled on for additional insights during development or troubleshooting
    ;; (message "Debug: Starting GPT process with command: %s" command)
    process))

(defun gpt-stop-python-process ()
  "Stop the GPT process if it is running."
  (interactive)
  (when (and gpt-process (process-live-p gpt-process))
    (interrupt-process gpt-process)
    (message "GPT process interrupted.")))

(defvar gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'gpt-stop-python-process)
    map)
  "Keymap for `gpt-mode'.")

(define-minor-mode gpt-mode
  "Minor mode for GPT process management."
  :lighter " GPT"
  :keymap gpt-mode-map)

(defun gpt-enable-mode ()
  "Enable `gpt-mode'."
  (gpt-mode 1))

(add-hook 'gpt-mode-hook 'gpt-enable-mode)

;; (gpt-modehookglobal-set-key (kbd "C-g") 'gpt-stop-python-process)

;; ;; (defun gpt-process-sentinel (process msg)
;; ;;   "Custom sentinel for the GPT process. Ignores the finished message."
;; ;;   (unless (string-match-p "finished\\|exited" msg) ; Also consider 'exited' messages
;; ;;     (message "GPT Process: %s" msg)))

;; ;; (defun gpt-start-python-process (prompt) ;
;; ;;   "Start the GPT process with the given PROMPT using a shell command."
;; ;;   (interactive "sEnter prompt: ")
;; ;;   (let* ((command (gpt-construct-command prompt))
;; ;;          (process (start-process-shell-command "gpt-process" "*GPT*" command)))
;; ;;     (set-process-sentinel process #'gpt-process-sentinel)
;; ;;     ;; Debugging the command can be toggled on for additional insights during development or troubleshooting
;; ;;     ;; (message "Debug: Starting GPT process with command: %s" command)
;; ;;     process))

;; (defun gpt-start-python-process (prompt)
;;   "Start the GPT process with the given PROMPT using a shell command."
;;   (start-process-shell-command "gpt-process" "*GPT*" (gpt-construct-command prompt))
;; )





;; (defun fetch-capital-templates (dir)
;;   "Return list of templates file names that start with capital letters in `dir`."
;;   (when (file-exists-p dir)
;;     (let ((files (directory-files dir nil "^[A-Z].*\\.md$")))
;;       (mapcar (lambda (f) (substring f 0 (string-match "\\.md" f))) files))))

;; (defun gpt-select-task-type (&optional initial-input)
;;   "Prompt the user to select a task type for the GPT model.
;; If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
;;   (interactive)
;;   (unless (minibufferp)
;;     (let* ((capital-templates (fetch-capital-templates gpt-templates-dir))
;;            (default-templates '("Fix" "Implement" "Refactor" "SciWrite" "Correct" "Docstring" "Email" "Remember"))
;;            (all-templates (append default-templates capital-templates))
;;            (templates-with-shortcuts (mapcar (lambda (template) (cons template template)) all-templates))
;;            (shortcuts (generate-shortcuts templates-with-shortcuts))
;;            (shortcut-list (hash-table-keys shortcuts))
;;            (prompt (concat "Enter task type (" (mapconcat (lambda (key) (concat key " for " (gethash key shortcuts))) shortcut-list ", ") "): "))
;;            (input (completing-read prompt (append (hash-table-values shortcuts) shortcut-list) nil t initial-input))
;;            (task-type (or (gethash input shortcuts) input)))
;;       ;; (message "Debug: Selected task type is %s" task-type)  ;; Debug message
;;       (if (member task-type all-templates)
;;           (progn
;;             (message "%s" task-type)
;;             ;; (message "You have selected: %s" task-type)
;;             task-type)
;;         (if (not (string-empty-p task-type))
;;             task-type
;;           ;; (message "No task type selected; defaulting to custom.")
;;           "custom")))))


;; (defun gpt-construct-command (prompt)
;;   "Construct the complete command string for starting the GPT Python process."
;;   (interactive)
;;     (let* (
;;           (task-type (gpt-select-task-type))
;;           (command (format "%s %s --api_key %s --engine %s --max_tokens %s --temperature %s --history_file %s --n_history %s --task_type %s --prompt %s"
;;                            (shell-quote-argument gpt-python-bin-path)
;;                            (shell-quote-argument gpt-python-script-path)
;;                            (shell-quote-argument gpt-openai-key)
;;                            (shell-quote-argument gpt-openai-engine)
;;                            (shell-quote-argument gpt-openai-max-tokens)
;;                            (shell-quote-argument gpt-openai-temperature)
;;                            (shell-quote-argument gpt-history-path)
;;                            (shell-quote-argument gpt-openai-n-history)
;;                            (shell-quote-argument task-type)
;;                            (shell-quote-argument prompt)))))
;;       command)

;; (gpt-construct-command "hello")

;; (defun generate-shortcuts (templates)
;;   "Generate shortcuts for a list of TEMPLATES."
;;   (let ((shortcuts (make-hash-table :test 'equal)))
;;     (dolist (template templates)
;;       (let* ((name (car template))
;;              (first-char (downcase (substring name 0 1))))
;;         (if (gethash first-char shortcuts)
;;             ;; If the first character is already taken, find the next available character
;;             (let ((suffix 1)
;;                   (new-char first-char))
;;               (while (gethash new-char shortcuts)
;;                 (setq new-char (concat first-char (number-to-string suffix)))
;;                 (setq suffix (1+ suffix)))
;;               (puthash new-char name shortcuts))
;;           (puthash first-char name shortcuts))))
;;     shortcuts))

;; (defun gpt-process-sentinel (process msg)
;;   "Custom sentinel for the GPT process. Ignores the finished message."
;;   (unless (string-match-p "finished\\|exited" msg) ; Also consider 'exited' messages
;;     (message "GPT Process: %s" msg)))

;; (defun gpt-start-python-process (prompt)
;;   "Start the GPT process with the given TASK-TYPE and PROMPT using a shell command."
;;   (interactive "sEnter prompt: ")
;;   (let* ((command (gpt-construct-command prompt))
;;          (process (start-process-shell-command "gpt-process" "*GPT*" command)))
;;     (set-process-sentinel process #'gpt-process-sentinel)
;;     ;; Debugging the command can be toggled on for additional insights during development or troubleshooting
;;     ;; (message "Debug: Starting GPT process with command: %s" command)
;;     process))

(defun gpt-run (prompt &optional task-type)
  (interactive)
  "Run GPT command with BUFFER text as input and append output stream to output-buffer."
  (with-current-buffer (get-buffer-create "*GPT*")
    (goto-char (point-max))
    (font-lock-fontify-buffer)
    (message "GPT: Running...")
    (gpt-start-python-process prompt task-type)))


(defun gpt-on-region ()
  "Run GPT command on selected region, or prompt for input if no region is selected."
  (interactive)
  (let* ((region-text (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (read-string "Enter text: ")))
         (buffer (get-buffer-create "*GPT*")))

    ;; Debug: Output the region text to check
    (message "Text: %s" region-text)

    ;; Prepare the buffer for output
    (with-current-buffer buffer
      ;; Ensure the buffer is using `gpt-mode` if defined
      (unless (eq major-mode 'gpt-mode)
        (gpt-mode))  ; Assuming `gpt-mode` is defined to handle specific formatting or functionality

      (erase-buffer)
      (insert region-text))

    ;; Run the GPT function, assuming it's defined to handle the task
    ;; Make sure `gpt-run` is properly defined to take task type and text or adjust accordingly
    (gpt-run region-text "custom")

    ;; Display the buffer where the output is directed
    (display-buffer buffer)))

(provide 'emacs-gpt)


;; (defun fetch-capital-templates (dir)
;;   "Return list of templates file names that start with capital letters in `dir`."
;;   (when (file-exists-p dir)
;;     (let ((files (directory-files dir nil "^[A-Z].*\\.md$")))
;;       (mapcar (lambda (f) (substring f 0 (string-match "\\.md" f))) files))))

;; (defun generate-shortcuts (templates)
;;   "Generate shortcuts for a list of TEMPLATES."
;;   (let ((shortcuts (make-hash-table :test 'equal)))
;;     (dolist (template templates)
;;       (let* ((name (car template))
;;              (first-char (downcase (substring name 0 1))))
;;         (if (gethash first-char shortcuts)
;;             ;; If the first character is already taken, find the next available character
;;             (let ((suffix 1)
;;                   (new-char first-char))
;;               (while (gethash new-char shortcuts)
;;                 (setq new-char (concat first-char (number-to-string suffix)))
;;                 (setq suffix (1+ suffix)))
;;               (puthash new-char name shortcuts))
;;           (puthash first-char name shortcuts))))
;;     shortcuts))

;; (defun gpt-select-task-type (&optional initial-input)
;;   "Prompt the user to select a task type for the GPT model.
;; If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
;;   (interactive)
;;   (unless (minibufferp)
;;     (let* ((capital-templates (fetch-capital-templates gpt-templates-dir))
;;            (default-templates '("Fix" "Implement" "Refactor" "SciWrite" "Correct" "Docstring" "Email" "Remember"))
;;            (all-templates (append default-templates capital-templates))
;;            (templates-with-shortcuts (mapcar (lambda (template) (cons template template)) all-templates))
;;            (shortcuts (generate-shortcuts templates-with-shortcuts))
;;            (shortcut-list (hash-table-keys shortcuts))
;;            (prompt (concat "Enter task type (" (mapconcat (lambda (key) (concat key " for " (gethash key shortcuts))) shortcut-list ", ") "): "))
;;            (input (completing-read prompt (append (hash-table-values shortcuts) shortcut-list) nil t initial-input))
;;            (task-type (or (gethash input shortcuts) input)))
;;       (if (member task-type all-templates)
;;           (progn
;;             (message "%s" task-type)
;;             task-type)
;;         (if (not (string-empty-p task-type))
;;             task-type
;;           (message "No task type selected; defaulting to custom.")
;;           "custom")))))

;; (defun gpt-select-task-type (&optional initial-input)
;;   "Prompt the user to select a task type for the GPT model.
;; If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
;;   (interactive)
;;   (unless (minibufferp)
;;     (let* ((capital-templates (fetch-capital-templates gpt-templates-dir))
;;            (default-templates '("Fix" "Implement" "Refactor" "SciWrite" "Correct" "Docstring" "Email" "Remember"))
;;            (all-templates (append default-templates capital-templates))
;;            (templates-with-shortcuts (mapcar (lambda (template) (cons template template)) all-templates))
;;            (shortcuts (generate-shortcuts templates-with-shortcuts))
;;            (shortcut-list (hash-table-keys shortcuts))
;;            (prompt (concat "Enter task type (" (mapconcat (lambda (key) (concat key " for " (gethash key shortcuts))) shortcut-list ", ") "): "))
;;            (input (completing-read prompt (append (hash-table-values shortcuts) shortcut-list) nil t initial-input))
;;            (task-type (or (gethash input shortcuts) input)))
;;       (if (member task-type all-templates)
;;           (progn
;;             (message "You have selected: %s" task-type)
;;             task-type)
;;         (if (not (string-empty-p task-type))
;;             task-type
;;           (message "No task type selected; defaulting to custom.")
;;           "custom")))))



;; ;; (defun gpt-select-task-type (&optional initial-input)
;; ;;   "Prompt the user to select a task type for the GPT model after a short delay.
;; ;; If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
;; ;;   (interactive)
;; ;;   (unless (minibufferp)
;; ;;     (let ((task-type (read-string "Enter task type (f)ix / (i)mplement / (r)efactor /(s)ciwrite / (c)orrect / (d)ocstring / (e)mail or custom: " initial-input)))
;; ;;       (cond
;; ;;        ((string= task-type "r") "Remember")
;; ;;        ((string= task-type "f") "Fix")
;; ;;        ;; ((string= task-type "r") "Refactor")
;; ;;        ((string= task-type "i") "Implement")                                          
;; ;;        ((string= task-type "s") "SciWrite")
;; ;;        ((string= task-type "c") "Correct")
;; ;;        ((string= task-type "d") "Docstring")
;; ;;        ((string= task-type "e") "Email")                                   
;; ;;        ((not (string-empty-p task-type)) task-type)
;; ;;        (t
;; ;;         (message "No task type selected; defaulting to custom.")
;; ;;         "custom")))))

;; ;; (defun fetch-capital-templates (dir)
;; ;;   "Return list of templates file names that start with capital letters in `dir`."
;; ;;   (when (file-exists-p dir)
;; ;;     (let ((files (directory-files dir nil "^[A-Z].*\\.md$")))
;; ;;       (mapcar (lambda (f) (substring f 0 (string-match "\\.md" f))) files))))

;; ;; (defun gpt-select-task-type (&optional initial-input)
;; ;;   "Prompt the user to select a task type for the GPT model after a short delay.
;; ;; If INITIAL-INPUT is non-nil, it's used as the initial input for the prompt."
;; ;;   (interactive)
;; ;;   (unless (minibufferp)
;; ;;     (let* ((capital-templates (fetch-capital-templates gpt-templates-dir))
;; ;;            (task-type
;; ;;             (completing-read "Enter one of the provided task types or custom: " 
;; ;;                                        capital-templates nil true initial-input)
;; ;;             ))
;; ;;       (if (member task-type capital-templates)
;; ;;           (message "You have selected: %s" task-type)
;; ;;         (message "Invalid task type; defaulting to custom.")
;; ;;         "custom"))))

;; ;; (gpt-select-task-type)

;; ;; (fetch-capital-templates gpt-templates-dir)



;; ;; ;; (defun gpt-on-region (beg end)
;; ;; ;;   "Run GPT command on region between BEG and END, or pass nothing if no region is selected."
;; ;; ;;   (interactive
;; ;; ;;    (if (use-region-p)
;; ;; ;;        (list (region-beginning) (region-end)) ; If region is selected, use it
;; ;; ;;      (list nil nil))) ; If not, pass nil for both BEG and END
;; ;; ;;   (let* ((region-text (if (and beg end) ; Check if BEG and END are non-nil
;; ;; ;;                           (buffer-substring-no-properties beg end)
;; ;; ;;                         "")) ; If nil, set region-text to an empty string
;; ;; ;;          ;; (sleep-for 1.0)         
;; ;; ;;          (task-type (gpt-select-task-type))
;; ;; ;;          (gpt-buffer)
;; ;; ;;          (text (concat task-type "\n\n" region-text)))
;; ;; ;;     (with-current-buffer buffer
;; ;; ;;       (unless (eq major-mode 'gpt-mode)
;; ;; ;;         (gpt-mode))
;; ;; ;;       (erase-buffer)
;; ;; ;;       (insert text)
;; ;; ;;       ;; (backward-delete-char)
;; ;; ;;       (gpt-truncate-history)
;; ;; ;;       )
;; ;; ;;     ;; If the task type is nil or empty, don't run the buffer
;; ;; ;;     (unless (string-empty-p task-type)
;; ;; ;;       (gpt-run-buffer task-type)
;; ;; ;;       (display-buffer gbt-buffer))))

;; ;; ;; (defun gpt-start-python-process (prompt-file output-buffer &optional task-type)
;; ;; ;;   "Start the GPT process with the given PROMPT-FILE and OUTPUT-BUFFER.
;; ;; ;; Use `gpt-script-path' as the executable and pass the other arguments as a list."
;; ;; ;;   (let* (
;; ;; ;;          (task-type (or task-type "")) ; Set default value of task-type to ""
;; ;; ;;          (process (start-process "gpt-process" output-buffer
;; ;; ;;                                  gpt-python-bin-path
;; ;; ;;                                  gpt-python-script-path
;; ;; ;;                                  (format "--api_key %s" gpt-openai-key)
;; ;; ;;                                  (format "--engine %s" gpt-openai-engine)
;; ;; ;;                                  (format "--max_tokens %s" gpt-openai-max-tokens)
;; ;; ;;                                  (format "--temperature %s" gpt-openai-temperature)
;; ;; ;;                                  (format "--prompt_file %s" prompt-file)  ; Ensure this is correct
;; ;; ;;                                  (format "--history_file %s" gpt-history-path)
;; ;; ;;                                  (format "--n_history %s" gpt-openai-n-history)
;; ;; ;;                                  (format "--task_type %s" task-type)
;; ;; ;;                                  )))
;; ;; ;;     process))

;; ;; ;; ;; (defun gpt-start-python-process (prompt-file output-buffer &optional task-type)
;; ;; ;; ;;   "Start the GPT process with the given PROMPT-FILE and OUTPUT-BUFFER.
;; ;; ;; ;; Use `gpt-script-path' as the executable and pass the other arguments as a list."
;; ;; ;; ;;   (let* (
;; ;; ;; ;;          (task-type (or task-type "")) ; Set default value of task-type to ""
;; ;; ;; ;;          (process (start-process "gpt-process" output-buffer
;; ;; ;; ;;                                  gpt-python-bin-path
;; ;; ;; ;;                                  gpt-python-script-path
;; ;; ;; ;;                                  (format "--api-key %s" gpt-openai-key)
;; ;; ;; ;;                                  (format "--engine %s" gpt-openai-engine)
;; ;; ;; ;;                                  (format "--max-tokens %s" gpt-openai-max-tokens)
;; ;; ;; ;;                                  (format "--temperature %s" gpt-openai-temperature)
;; ;; ;; ;;                                  (format "--prompt_file %s" (gpt-create-prompt-file output-buffer))
;; ;; ;; ;;                                  (format "--history_file %s" gpt-history-path)
;; ;; ;; ;;                                  (format "--task_type %s" task-type)                                 
;; ;; ;; ;;                                  )))
;; ;; ;; ;;     process))

;; ;; ;; ;; (defun gpt-start-python-process (prompt-file output-buffer &optional task-type)
;; ;; ;; ;;   "Start the GPT process with the given PROMPT-FILE and OUTPUT-BUFFER.
;; ;; ;; ;; Use `gpt-script-path' as the executable and pass the other arguments as a list."
;; ;; ;; ;;   (let* (
;; ;; ;; ;;          (task-type (or task-type "")) ; Set default value of task-type to ""         
;; ;; ;; ;;          (process (start-process "gpt-process" output-buffer
;; ;; ;; ;;                                  ;; "python"
;; ;; ;; ;;                                  gpt-python-bin-path
;; ;; ;; ;;                                  gpt-python-script-path
;; ;; ;; ;;                                  (concat "--api-key " gpt-openai-key)
;; ;; ;; ;;                                  (concat "--engine " gpt-openai-engine)
;; ;; ;; ;;                                  (concat "--max-tokens " gpt-openai-max-tokens)
;; ;; ;; ;;                                  (concat "--temperature " gpt-openai-temperature)
;; ;; ;; ;;                                  (concat "--prompt_file " (gpt-create-prompt-file output-buffer))
;; ;; ;; ;;                                  (concat "--history_file " gpt-history-path)
;; ;; ;; ;;                                  (concat "--task_type " task-type)                                 
;; ;; ;; ;;                                  )))
;; ;; ;; ;;     process))

;; ;; ;; (defun gpt-start-timer (process)
;; ;; ;;   "Start a timer that checks if PROCESS is running every second."
;; ;; ;;   (run-with-timer 0 1 'gpt-check-process process))

;; ;; ;; (defun gpt-check-process (process)
;; ;; ;;   "Check if PROCESS is running and message the user if it has finished."
;; ;; ;;   (unless (process-live-p process)
;; ;; ;;     (message "GPT: Finished running command.")))

;; ;; ;; (defun gpt-set-process-sentinel (process timer prompt-file)
;; ;; ;;   "Set PROCESS sentinel to delete TIMER and PROMPT-FILE when process finishes."
;; ;; ;;   (set-process-sentinel process (lambda (_process _event)
;; ;; ;;                                   (cancel-timer timer)
;; ;; ;;                                   (delete-file prompt-file)
;; ;; ;;                                   (goto-char (point-max))                                  
;; ;; ;;                                   (let ((buffer (process-buffer process)))
;; ;; ;;                                     (with-current-buffer buffer
;; ;; ;;                                       (goto-char (point-max))
;; ;; ;;                                       (when (re-search-backward "^============================================================\n\nGPT" nil t)
;; ;; ;;                                         (beginning-of-line))
;; ;; ;;                                       (let ((point (point)))
;; ;; ;;                                         (when-let ((window (get-buffer-window buffer 0)))
;; ;; ;;                                           (set-window-point window point)
;; ;; ;;                                           (with-selected-window window
;; ;; ;;                                             (recenter 0)))))))))

;; ;; ;; (defun gpt-create-prompt-file (buffer)
;; ;; ;;   (interactive)
;; ;; ;;   "Create a prompt file with the text in BUFFER and return its file name."
;; ;; ;;   (let ((file (make-temp-file "gpt-prompt-" nil ".txt")))
;; ;; ;;     (with-temp-file file
;; ;; ;;       (insert-buffer-substring buffer))
;; ;; ;;     file))

;; ;; ;; ;; (gpt-create-prompt-file (current-buffer))


;; ;; ;; (defun copy-last-gpt-output-to-kill-ring ()
;; ;; ;;   "Copy the last GPT output from the *GPT* buffer to the kill ring."
;; ;; ;;   (interactive)
;; ;; ;;   (with-current-buffer "*GPT*"
;; ;; ;;     (goto-char (point-max))
;; ;; ;;     (let ((delimiter "============================================================\n\nGPT\n\n"))
;; ;; ;;       ;; Search for the second last occurrence of the delimiter
;; ;; ;;       (when (search-backward delimiter nil t 2)
;; ;; ;;         (forward-char (length delimiter))  ; Move past the delimiter to the start of the last GPT output
;; ;; ;;         (let ((start (point))
;; ;; ;;               end output)
;; ;; ;;           ;; Find the end of the GPT output before the next delimiter
;; ;; ;;           (when (search-forward delimiter nil t)
;; ;; ;;             (setq end (match-beginning 0))
;; ;; ;;             (goto-char start)  ; Go back to the start of the output
;; ;; ;;             ;; Get the content excluding the last three lines
;; ;; ;;             (let* ((content (buffer-substring-no-properties start end))
;; ;; ;;                    (lines (split-string content "\n" t))  ; Split into lines, omitting empty trailing lines
;; ;; ;;                    (trimmed-lines (butlast lines 3)))  ; Remove the last three lines
;; ;; ;;               (setq output (string-join trimmed-lines "\n"))  ; Join the lines back with newline characters
;; ;; ;;               (kill-new output)
;; ;; ;;               (message "Output copied to kill ring."))))))))



;; ;; ;; (defun copy-all-code-blocks-from-last-gpt-output ()
;; ;; ;;   "Find the last GPT output in the *GPT* buffer, then find all code blocks within that section, reverse their order, and copy to the kill-ring."
;; ;; ;;   (interactive)
;; ;; ;;   (message "bbcopy-all-code-blocks-from-last-gpt-output starts")    
;; ;; ;;   (with-current-buffer "*GPT*"
;; ;; ;;     (goto-char (point-max))  ; Start from the end of the buffer
;; ;; ;;     (let ((delimiter "============================================================\n\nGPT\n\n")
;; ;; ;;           (code-block-delimiter-start "^```[a-z]*")  ; [REVISED]
;; ;; ;;           (code-block-delimiter-end "^```$")  ; [REVISED]
;; ;; ;;           blocks start end)
;; ;; ;;       ;; Find the last occurrence of the GPT output delimiter
;; ;; ;;       (when (search-backward delimiter nil t)
;; ;; ;;         (goto-char (match-end 0))  ; Move point to the end of the delimiter
;; ;; ;;         ;; Collect all code blocks from this point forward
;; ;; ;;         (while (re-search-forward code-block-delimiter-start nil t)
;; ;; ;;           (forward-line 1)
;; ;; ;;           (setq start (point))
;; ;; ;;           (if (re-search-forward code-block-delimiter-end nil t)
;; ;; ;;               (progn
;; ;; ;;                 (setq end (match-beginning 0))
;; ;; ;;                 (push (buffer-substring-no-properties start end) blocks))))
;; ;; ;;         ;; Reverse the order of code blocks
;; ;; ;;         (setq blocks (nreverse blocks))
;; ;; ;;         ;; Combine all blocks into a single string and copy to kill ring
;; ;; ;;         (let ((output (mapconcat 'identity blocks "\n\n")))
;; ;; ;;           (kill-new output)
;; ;; ;;           (message "Code blocks were copied (n = %d)." (length blocks)))))))



;; ;; ;; (defun gpt-truncate-history ()
;; ;; ;;   "Truncate the GPT conversation history to the latest 5 entries."
;; ;; ;;   (interactive)
;; ;; ;;   (let* ((history-file (expand-file-name "history.json" (file-name-directory gpt-python-script-path))))
;; ;; ;;     (when (file-exists-p history-file)
;; ;; ;;       (call-process "python" nil nil nil "-c"
;; ;; ;;                     (concat "import json; "
;; ;; ;;                             "with open('" history-file "', 'r') as file: "
;; ;; ;;                             "history = json.load(file); "
;; ;; ;;                             "history = history[-5:]; "
;; ;; ;;                             "with open('" history-file "', 'w') as file: "
;; ;; ;;                             "json.dump(history, file)")))))

;; ;; ;; (defun gpt-clear-history ()
;; ;; ;;   "Clear the GPT conversation history file (history.json)."
;; ;; ;;   (interactive)
;; ;; ;;   (let* ((history-file (expand-file-name "history.json" (file-name-directory gpt-python-script-path)))
;; ;; ;;          (gpt-buffer (get-buffer "*GPT*")))
;; ;; ;;     (when gpt-buffer
;; ;; ;;       (kill-buffer gpt-buffer))
;; ;; ;;     (when (file-exists-p history-file)
;; ;; ;;       (delete-file history-file))
;; ;; ;;     (message "GPT conversation history file cleared.")))

;; ;; ;; (defun gpt-quit ()
;; ;; ;;   "Quit the GPT buffer."
;; ;; ;;   (interactive)
;; ;; ;;   (kill-buffer (current-buffer)))

;; ;; ;; (defun hide-gpt-buffer ()
;; ;; ;;   (interactive)
;; ;; ;;   (when (get-buffer "*GPT*")
;; ;; ;;     (delete-window (get-buffer-window "*GPT*"))))

;; ;; ;; ;; (define-key gpt-mode-map (kbd "C-S-k") 'hide-gpt-buffer)
;; ;; ;; (define-key gpt-mode-map (kbd "q") nil)

;; ;; ;; ;; (defun call-ai-model-stream (prompt)
;; ;; ;; ;;   (interactive "sPrompt: ")
;; ;; ;; ;;   (url-retrieve
;; ;; ;; ;;    "http://localhost:5000/predict"
;; ;; ;; ;;    (lambda (status)
;; ;; ;; ;;      (switch-to-buffer (current-buffer))
;; ;; ;; ;;      (goto-char (point-min))
;; ;; ;; ;;      (when (search-forward-regexp "^$" nil t)
;; ;; ;; ;;        (delete-region (point-min) (point)))
;; ;; ;; ;;      (set-buffer-multibyte t)
;; ;; ;; ;;      (auto-revert-mode 1))
;; ;; ;; ;;    `(("Content-Type" . "application/json")
;; ;; ;; ;;      ("Accept" . "text/plain"))
;; ;; ;; ;;    (json-encode `(("prompt" . ,prompt)))
;; ;; ;; ;;    'POST))


;; ;; ;; (provide 'emacs-gpt)

;; ;; ;; ;;; emacs-gpt.el ends here


;; ;; ;; (defun create-or-switch-to-buffer (buffer-name)
;; ;; ;;   "Create a new buffer with the given BUFFER-NAME or switch to it if it already exists."
;; ;; ;;   (interactive "sBuffer name: ")
;; ;; ;;   (let ((buffer (get-buffer-create buffer-name)))
;; ;; ;;     (switch-to-buffer buffer)
;; ;; ;;     (auto-revert-mode 1)
;; ;; ;;     (set-visited-file-name "/tmp/buffering_file.txt" t t)  ; Associate with a file
;; ;; ;;     (message "Buffer is set up as a file: %s" (buffer-file-name))))


;; ;; ;; ;; (defun save-buffer-to-file ()
;; ;; ;; ;;   "Save the current buffer to its associated file."
;; ;; ;; ;;   (interactive)
;; ;; ;; ;;   (when (buffer-file-name)
;; ;; ;; ;;     (save-buffer)
;; ;; ;; ;;     (message "Buffer saved to file: %s" (buffer-file-name))))

;; ;; ;; ;; (defun create-or-switch-to-example-buffer ()
;; ;; ;; ;;   "Create or switch to a buffer named *Example* and enable auto-revert-mode."
;; ;; ;; ;;   (interactive)  ; Make the function callable via M-x or a key binding
;; ;; ;; ;;   (let ((buf (get-buffer-create "*Example*")))  ; Create the buffer or get it if it already exists
;; ;; ;; ;;     (switch-to-buffer buf)  ; Switch to the buffer
;; ;; ;; ;;     (auto-revert-mode 1)  ; Enable auto-revert-mode in the buffer
;; ;; ;; ;;     (message "Switched to buffer %s with auto-revert-mode enabled" (buffer-name buf))))

;; ;; ;; ;; (create-or-switch-to-buffer "*Example*")

