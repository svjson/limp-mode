;;; limp-mode.el --- Lisple Interaction Mode for Programming -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sven Johansson

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; limp-mode provides a minor mode for REPL interaction from a Lisple buffer
;; and a REPL mode.

;;; Code:

(require 'zprint-format)

(defvar limp-socket-connection nil
  "Holds the active socket connection to the external process.")

(defvar limp-mode-reconnect-interval 5
  "Interval in seconds for attempting to reconnect if the socket is closed.")

(defvar limp-repl-buffer-name "*Lisple REPL*"
  "Name of the buffer used for REPL interaction.")

(defvar limp-repl-stand-alone-server-port 8100
  "Default port to connect to a Lisple Stand-alone Server at.")

(defvar limp-repl-ns "user"
  "The current(and default) Lisple namespace.")

(defvar limp-interactive-command nil
  "Determines if the current command handled is interactive.")

(defvar limp-connected-server nil
  "Server-info for the connected server.")

(defvar limp-message-buffer ""
  "Socket buffer for buffered reading over the socket.")

(defvar limp-repl-format-fn 'limp-mode-zprint-format
  "(Optional) Function called for formatting Lisple code and data.")

(defvar limp-repl-command-history '() "History of commands entered in the REPL.")
(defvar limp-repl-history-max-size 100 "Maximum number of commands to store in history.")
(defvar limp-repl-history-index -1 "Current position in the REPL command history.")

(defun limp-mode-connect ()
  "Establish a socket connection to a Lisple server."
  (unless (process-live-p limp-socket-connection)
    (setq limp-socket-connection
          (condition-case nil
              (open-network-stream "limp-socket-connection" nil "localhost" 8100)
            (error (message "Failed to connect. Retrying in %d seconds..."
                            limp-mode-reconnect-interval)
                   (run-at-time limp-mode-reconnect-interval nil 'limp-mode-connect))))
    (when (process-live-p limp-socket-connection)
      (limp-mode-send-message (list (cons :MSG
                                          (list (cons :CMD
                                                      (list (cons :ID "server-info"))))))))))

(defun limp-mode-disconnect ()
  "Disconnect the socket if it's active."
  (when (process-live-p limp-socket-connection)
    (delete-process limp-socket-connection)
    (setq limp-socket-connection nil)))

(defun limp-mode-reconnect ()
  "Automatically reconnect if the connection is lost."
  (unless (process-live-p limp-socket-connection)
    (limp-mode-connect)))

(defun limp-encode-block-entry (entry)
  "Encodes a block ENTRY into the message wire format. Recursively, if required."
  (if (consp (cdr entry))
      (format "!%s\x1E%s/%s\x1E"
              (substring (symbol-name (car entry)) 1)
              (limp-encode-block (cdr entry))
              (substring (symbol-name (car entry)) 1))
    (format "@%s=%s\x1E"
            (substring (symbol-name (car entry)) 1)
            (cdr entry))))

(defun limp-encode-block (block)
  "Encode an alist structure BLOCK into the message wire format."
  (mapconcat #'limp-encode-block-entry block ""))

(defun limp-parse-block (input)
  "Parse an encoded message string INPUT back into a nested alist structure."
  (let ((index 0) stack)

    (cl-labels (;; Helper function to consume the next segment in input based on a regex.
                (consume (regex)
                  (when (string-match regex input index)
                    (let ((match (match-string 0 input)))
                      (setq index (match-end 0))
                      match)))
                ;; Recursively parse and build nested alists.
                (parse-entry ()
                  (cond
                   ;; Handle opening of a new block
                   ((consume (concat "!\\([A-Z]+\\)" (char-to-string ?\x1E)))
                    (let ((tag (intern (concat ":" (match-string 1 input)))))
                      ;; Create a new entry for this block
                      (let ((new-entry (cons tag nil)))
                        ;; Push the new entry onto the stack
                        (push new-entry stack)
                        (let ((content (parse-entry)))
                          ;; Associate content with the last entry in the stack
                          (if stack
                              (progn
                                (setcdr (car stack) (nreverse (cons content (cdr (car stack)))))
                                (parse-entry))
                            (list content))))))
                   ;; Handle key-value property pairs
                   ((consume (concat "@\\([A-Z]+\\)=\\([^" (char-to-string ?\x1E) "]+\\)" (char-to-string ?\x1E)))
                    (let ((key (intern (concat ":" (match-string 1 input))))
                          (value (match-string 2 input)))
                      (when stack
                        (push (cons key value) (cdr (car stack)))) ;; Push to the last entry's alist
                      (parse-entry)))
                   ;; Handle closing of a block
                   ((consume (concat "/\\([A-Z]+\\)" (char-to-string ?\x1E)))
                    (let ((tag (intern (concat ":" (match-string 1 input)))))
                      (if (and stack (eq tag (caar stack))) ;; Check if it matches the last opened block
                          (let ((closed-entry (pop stack))) ;; Remove the matching tag from the stack
                            closed-entry) ;; Return the closed entry
                        (progn            ;(message input)
                          (error "Mismatched closing block: %s" (symbol-name tag))))))
                   )))

      ;; Start parsing from the root and return result as an alist
      (parse-entry))))

(defun limp-mode-evaluate-prompt (code)
  "Send CODE to the external process and display the result in the minibuffer."
  (interactive "sCode to evaluate: ")
  (limp-mode-reconnect)                 ; Ensure connection
  (when (process-live-p limp-socket-connection)
    (setq limp-interactive-command t)
    (process-send-string limp-socket-connection (concat (limp-encode-block (limp-lisple-server-eval-sexp-command code)) "\n"))
    (set-process-filter limp-socket-connection #'limp-buffered-read-socket)))


(defun limp-buffered-read-socket (_process data)
  "`process-filter' that handles incoming DATA from the _PROCESS.

Stores the incoming data to the internal buffer and passes it to
`limp-process-message' once the buffer contains a full message."
  (setq limp-message-buffer (concat limp-message-buffer data))

  (while (string-match (concat "!MSG\\(\\(?:.\\|\n\\)*?\\)/MSG" (char-to-string ?\x1E)) limp-message-buffer)
    (let ((complete-message (match-string 0 limp-message-buffer)))
      ;; Here you can process the complete message
      (limp-process-message complete-message)

      ;; Remove the processed message from the buffer
      (setq limp-message-buffer (substring limp-message-buffer (length complete-message))))))

(defun limp-get-last-sexp ()
  "Get the previous s-expression as a string."
  (save-excursion
    (backward-sexp)
    (let ((start (point)))
      (forward-sexp)
      (buffer-substring-no-properties start (point)))))

(defun limp-lisple-server-eval-sexp-command (code)
  "Constructs an \"eval-sexp\" alist block from CODE."
  (list (cons :MSG
              (list (cons :CMD
                          (list (cons :ID "eval-sexp")
                                (cons :NS limp-repl-ns)
                                (cons :BODY code)))))))

(defun limp-mode-zprint-format (input)
  "Format INPUT Lisple code using zprint."
  (message "Formatting...")
  (with-temp-buffer
    (insert input)
    (zprint-format-buffer)
    (buffer-string)))

(defun limp-mode-send-message (msg &optional interactivep)
  "Encodes and sends MSG to the REPL server.
Optionally flags the command as interactive if INTERACTIVEP is provided."
  (setq limp-interactive-command interactivep)
  (process-send-string limp-socket-connection
                       (concat
                        (limp-encode-block msg)
                        "\n"))
  (set-process-filter limp-socket-connection #'limp-buffered-read-socket))

(defun limp-mode-eval-last-sexp ()
  "Send the Lisp form behind the cursor to the REPL server for eval."
  (interactive)
  (let ((code (limp-get-last-sexp)))    ; Get the Lisp form at point
    (if code
        (progn
          (limp-mode-reconnect)         ; Ensure connection
          (when (process-live-p limp-socket-connection)
            ;; Highlight the evaluated expression
            (save-excursion
              (let* ((start (progn (backward-sexp) (point)))
                     (end (progn (forward-sexp) (point)))
                     (overlay (make-overlay start end))) ; Create overlay
                (overlay-put overlay 'face 'region)      ; Set highlight face
                (limp-mode-send-message (limp-lisple-server-eval-sexp-command code) t);

                ;; Remove overlay after a brief pause
                (run-at-time 0.3 nil (lambda (ov) (delete-overlay ov)) overlay)))))
      (message "No Lisp expression found at point.")))) ;; Message if no expression found

(defun limp-process-message (input)
  "Process and dispatch incoming INPUT message."
  (let* ((msg (limp-parse-block input))
         (id (cdr (assoc :ID
                         (assoc :RESP
                                (assoc :MSG msg))))))
    (cond
     ((string= id "err") (limp-mode-display-result msg t))
     ((string= id "switch-ns") (limp-mode-switch-ns msg))
     ((string= id "server-info") (limp-mode-process-server-info msg))
     ((string= id "eval-result") (limp-mode-display-result msg nil t))
     (t (message (concat "Unrecognized response id: " id))))))

(defun limp-mode-process-server-info (msg)
  "Process a response MSG of type \"server-info\"."
  (let* ((body (cdr (assoc :BODY (assoc :RESP (assoc :MSG msg))))))
    (message (concat "Connected to: " body))
    (setq limp-connected-server body)))

(defun limp-mode-switch-ns (msg)
  "Process a response MSG of type \"switch-ns\"."
  (let* ((ns (cdr (assoc :NS
                         (assoc :RESP
                                (assoc :MSG msg))))))
    (if limp-interactive-command
        (progn
          (message "not implemented")
          (limp-mode-display-result msg))
      (progn
        (setq limp-repl-ns ns)
        (limp-mode-display-result msg)))))

(defun limp-mode-display-result (msg &optional errorp formatp)
  "Display the body of a MSG.
Dispatches the output to either the mini-buffer or the REPL buffer
depending on `limp-interactive-command'. Optionally formats and/or
displays the message as an error depending on ERRORP and FORMATP."
  (let* ((body (cdr (assoc :BODY
                           (assoc :RESP
                                  (assoc :MSG msg))))))
    (if limp-interactive-command
        (progn
          (if errorp
              (error "%s" body)
            (message "%s" body))
          (setq limp-interactive-command nil))
      (when (get-buffer limp-repl-buffer-name)
        (with-current-buffer limp-repl-buffer-name
          (let ((output (if (and formatp limp-repl-format-fn)
                            (funcall limp-repl-format-fn body)
                          body)))
            (goto-char (point-max))
            (insert (propertize output
                                'font-lock-face
                                (if errorp
                                    'error
                                  'font-lock-syntax-table 'clojure-mode-syntax-table)))
            (insert-before-markers "\n")
            (limp-repl-prompt)))))))

(defun limp-mode-send-input (input)
  "Send REPL INPUT to the REPL server."
  (limp-mode-reconnect)
  (when (process-live-p limp-socket-connection)
    (process-send-string limp-socket-connection
                         (concat (limp-encode-block (limp-lisple-server-eval-sexp-command input)) "\n"))))

(defun limp-repl-banner ()
  "Create the Limp welcome banner."
  (format ";; Welcome to the Limp Lisple REPL
;; =====================================
;; Connected to %s REPL server at limp://%s:%S
"
          limp-connected-server
          "localhost"
          limp-repl-stand-alone-server-port))

(defun limp-repl-apply-input-face (_beg _end _len)
  "Apply a custom face to the user's input after the prompt."
  (save-excursion
    ;; (goto-char beg)
    ;; (let ((prompt-end (save-excursion
    ;;                     (goto-char (point-min))
    ;;                     (search-forward "=>" nil t)))) ; Adjust if prompt differs
    ;;   (when (and prompt-end (> beg prompt-end))
    ;;     (add-text-properties beg end '(font-lock-face font-lock-builtin-face))))
    (font-lock-ensure (line-beginning-position) (line-end-position))))

(defun limp-repl-prompt ()
  "Output the REPL prompt."
  (goto-char (point-max))
  (insert-before-markers
   (propertize
    (format "%s=>" limp-repl-ns)
    'font-lock-face 'font-lock-keyword-face))
  (goto-char (point-max))
  (insert-before-markers
   (propertize " " 'font-lock-face 'font-lock-builtin-face))
  (font-lock-ensure (line-beginning-position) (line-end-position)))

(defun limp-mode-start-repl ()
  "Start a REPL session in a separate buffer using the shared connection."
  (interactive)
  (let ((buffer (get-buffer-create limp-repl-buffer-name)))
    (with-current-buffer buffer
      (limp-repl-mode 1)
      (limp-mode-reconnect)
      (setq buffer-read-only nil)
      (font-lock-mode 1)
      (erase-buffer)
      (insert-before-markers (propertize (limp-repl-banner) 'font-lock-face 'font-lock-comment-face))
      (add-hook 'after-change-functions 'limp-repl-apply-input-face nil t)
      (limp-repl-prompt)
      (switch-to-buffer-other-window buffer))))

(defun limp-repl-add-to-history (command)
  "Add COMMAND to the REPL command history, maintaining a maximum size."
  (when (or (= (length limp-repl-command-history) 0)
            (and (> (length limp-repl-command-history) 0)
                 (not (string= command (car limp-repl-command-history)))))
    (setq limp-repl-command-history
          (append (list command) limp-repl-command-history))
    ;; Trim the list if it exceeds the maximum size
    (when (> (length limp-repl-command-history) limp-repl-history-max-size)
      (setq limp-repl-command-history (butlast limp-repl-command-history)))))

(defun limp-repl-replace-command (command)
  "Replace the command at the REPL prompt with COMMAND."
  (interactive)
  (goto-char (point-max))
  (when (re-search-backward (regexp-quote "=> ") nil t)
    (goto-char (match-end 0)))
  (delete-region (point) (point-max))
  (goto-char (point-max))
  (insert-before-markers (propertize command 'font-lock-face 'font-lock-builtin-face)))

(defun limp-repl-previous-command ()
  "Get the previous command in the REPL history."
  (interactive)
  (when (< limp-repl-history-index (1- (length limp-repl-command-history)))
    (setq limp-repl-history-index (1+ limp-repl-history-index)))
  (let ((command (nth limp-repl-history-index limp-repl-command-history)))
    (when command
      (limp-repl-replace-command command))))

(defun limp-repl-next-command ()
  "Get the next command in the REPL history."
  (interactive)
  (when (> limp-repl-history-index 0)
    (setq limp-repl-history-index (1- limp-repl-history-index)))
  (let ((command (nth limp-repl-history-index limp-repl-command-history)))
    (when command
      (limp-repl-replace-command command))))

(defun limp-repl-send-input ()
  "Send input from the REPL buffer to the external process."
  (interactive)
  (let* ((prompt-text "=> ")
         (prompt-pos (save-excursion
                       (re-search-backward (regexp-quote prompt-text) nil t)))
         (input (buffer-substring-no-properties
                 (if prompt-pos
                     (+ prompt-pos (length prompt-text))
                   (line-beginning-position))
                 (line-end-position))))
    (goto-char (point-max))
    (insert-before-markers "\n")
    (if (string-match-p "[^ \t\n\r]" input)
        (progn
          (setq limp-interactive-command nil)
          (limp-repl-add-to-history input)
          (setq limp-repl-history-index -1)
          (limp-mode-send-input input)
          (with-current-buffer limp-repl-buffer-name
            (setq buffer-read-only nil)))
      (limp-repl-prompt))))

;;;###autoload
(define-minor-mode limp-repl-mode
  "A REPL mode for Lisple."
  :init-value nil
  :lighter "LimpREPL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'limp-repl-send-input)
            (define-key map (kbd "M-p") 'limp-repl-previous-command)
            (define-key map (kbd "M-n") 'limp-repl-next-command)
            map))

;;;###autoload
(define-minor-mode limp-mode
  "A mode to maintain a socket connection and evaluate code remotely."
  :init-value nil
  :lighter "Limp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c e") 'limp-mode-evaluate-prompt)
            (define-key map (kbd "C-c C-e") 'limp-mode-eval-last-sexp)
            (define-key map (kbd "C-c r") 'limp-mode-start-repl)
            map)
  (if limp-mode
      (progn
        (limp-mode-connect)
        (add-hook 'kill-buffer-hook #'limp-mode-disconnect nil t))
    (limp-mode-disconnect)))

(provide 'limp-mode)

;;; limp-mode.el ends here
