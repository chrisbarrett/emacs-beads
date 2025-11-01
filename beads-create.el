;;; beads-create.el --- Buffer-based input for creating bd issues -*- lexical-binding: t; -*-

;;; Commentary:

;; This library provides a git-commit-like buffer interface for creating
;; beads (bd) issues. Users can type free-form text describing the issue,
;; and an AI agent interprets the text into structured issue data.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ring)
(require 'beads-process)
(require 'beads-agent)
(require 'beads-issue)

;;; Customization

(defvar beads-issue-buffer-name "*bd-new-issue*"
  "Name of the buffer used for creating bd issues.")

(defcustom beads-issue-message-ring-size 32
  "Maximum number of issue descriptions to save in the history ring."
  :type 'integer
  :group 'bd-issue)



(defvar beads-issue-message-ring nil
  "Ring of previous bd issue descriptions for history cycling.")

(defvar-local beads-issue-message-ring-index nil
  "Index into `beads-issue-message-ring' for the current buffer.")

(defconst beads-issue-template
  "

# Describe the issue you want to create. Write naturally - the agent will
# interpret your description to create the issue with an appropriate title,
# type, and priority.
#
# Lines starting with '#' will be ignored.
"
  "Template for creating new bd issues.")

(defvar-local beads-issue--worktree-path nil
  "Path to worktree for this bd issue buffer.")

(defvar-local beads-issue--previous-window-config nil
  "Window configuration to restore when issue creation completes.")


;;; Major mode

(defvar beads-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-issue-finish)
    (define-key map (kbd "C-c C-k") #'beads-issue-cancel)
    (define-key map (kbd "C-c C-p") #'beads-issue-prev-message)
    (define-key map (kbd "M-p") #'beads-issue-prev-message)
    (define-key map (kbd "C-c C-n") #'beads-issue-next-message)
    (define-key map (kbd "M-n") #'beads-issue-next-message)
    map)
  "Keymap for `beads-issue-mode'.")

(define-derived-mode beads-issue-mode text-mode "BD-Issue"
  "Major mode for creating beads (bd) issues.

Special commands:
\\{beads-issue-mode-map}"
  (setq-local comment-start "#")
  (setq-local comment-start-skip (rx (one-or-more "#") (zero-or-more space)))
  (setq-local fill-column 72)
  (setq-local font-lock-defaults '((("^#.*$" . font-lock-comment-face)) nil nil nil nil))

  (auto-fill-mode 1)

  (add-hook 'kill-buffer-query-functions #'beads-issue--kill-buffer-query nil t)

  (setq-local mode-line-misc-info
              `((beads-issue-mode
                 ,(substitute-command-keys
                   " [\\[beads-issue-finish] = create, \\[beads-issue-cancel] = cancel, \\[beads-issue-prev-message] = prev]"))))

  (beads-issue--prepare-message-ring))


;;; Entry point

;;;###autoload
(defun beads-issue-create (&optional worktree-path)
  "Create a new bd issue based on free text.

Opens a buffer with a template for describing the issue. Write
your description naturally and press \\[beads-issue-finish] to
create the issue or \\[beads-issue-cancel] to cancel.

If WORKTREE-PATH is provided, use that directory for creating
the issue. Otherwise, use the worktree associated with the
current tab, or fall back to `default-directory'."
  (interactive)
  (let* ((worktree (or worktree-path
                       (when beads-worktree-root-function
                         (funcall beads-worktree-root-function))
                       default-directory))
         (window-config (current-window-configuration))
         (buf (generate-new-buffer beads-issue-buffer-name)))

    ;; Set up the buffer
    (with-current-buffer buf
      (beads-issue-mode)
      (setq-local beads-issue--worktree-path worktree)
      (setq-local beads-issue--previous-window-config window-config)
      (insert beads-issue-template)
      (goto-char (point-min)))

    (pop-to-buffer buf)

    (message "%s" (substitute-command-keys
                   "Enter issue details. \\[beads-issue-finish] to create, \\[beads-issue-cancel] to cancel."))))

;;; Buffer content extraction

(defun beads-issue--get-buffer-text ()
  "Extract user's issue description from buffer.
Removes comment lines - those starting with '#' followed by whitespace
or end-of-line. This strips lines like '# comment' and bare '#' lines,
but preserves '#tag' or '#123' (no space after #).
Note: This will also strip markdown headers like '# Title',
which is intentional to match the git-commit style."
  (string-trim
   (string-join
    (seq-remove (lambda (line)
                  (string-match-p (rx line-start "#" (or space line-end)) line))
                (split-string (buffer-string) "\n"))
    "\n")))

;;; Accept/Cancel handlers

(defun beads-issue-finish ()
  "Send user input to the agent for analysis and trigger issue creation."
  (interactive)
  (let ((issue-text (beads-issue--get-buffer-text)))
    (when (string-empty-p issue-text)
      (user-error "Please describe the issue you want to create"))

    (beads-issue-save-message)

    (let ((worktree-path beads-issue--worktree-path)
          (window-config beads-issue--previous-window-config))

      (let ((kill-buffer-query-functions nil))
        (kill-buffer (current-buffer)))

      (when window-config
        (set-window-configuration window-config))

      (beads-issue--create-via-agent issue-text worktree-path))))

(defun beads-issue-cancel ()
  "Cancel bd issue creation without creating the issue.
Saves the current message to history before cancelling."
  (interactive)
  (beads-issue-save-message)
  (let ((window-config beads-issue--previous-window-config))

    (let ((kill-buffer-query-functions nil))
      (kill-buffer (current-buffer)))

    (when window-config
      (set-window-configuration window-config))

    (message "Issue creation cancelled. Message saved to history")))

(defun beads-issue--kill-buffer-query ()
  "Query function for `kill-buffer-query-functions'.
Prevents accidental buffer kill without using \\[beads-issue-finish] or \\[beads-issue-cancel]."
  (if (eq major-mode 'beads-issue-mode)
      (progn
        (message "%s" (substitute-command-keys
                       "Use \\[beads-issue-finish] to create issue or \\[beads-issue-cancel] to cancel"))
        nil) ; Prevent kill
    t)) ; Allow kill for other modes


;;; AI Agent integration
;;
;; Use an AI agent to process a natural language issue description into
;; something more structured for bd create.
;; The flow is: agent completion (via beads-agent) -> parse output ->
;; bd create command (via beads-process-call in callback).

(defun beads-issue--make-prompt (issue-text)
  (format "
<instructions>
    You receive free text input from a user and must interpret it into a LISP
    S-Expression structure for use in a program.

    The user will provide a description of an issue they would like to create.
    You must interpret it into an alist with a specific set of allowed keys and
    value types, as per the structure definition below.

    You return ONLY the `read'-able alist, without markdown code fencing or any
    other prose.
</instructions>

<structure>
    <input>
        ARBITRARY FREE TEXT ENTERED BY USER
    </input>
    <output>
        ((arguments . (\"title\"))                          ; Required: positional arguments (just the title)
         (flags . ((type . \"bug|feature|task|epic|chore\") ; Optional (default: task)
                   (priority . 2)                           ; Optional: 0=highest, 2=default
                   (description . \"string\")               ; Optional: issue description
                   (labels . (\"string\" ...))              ; Optional: keyword labels for querying
                   (deps . (\"string\" ...))                ; Optional: Dependencies in format 'type:id' or 'id'
                   (design . \"string\")                    ; Optional: design notes
                   (acceptance . \"string\")                ; Optional: acceptance criteria
                   (external-ref . \"string\"))))           ; Optional: external ticket ID
    </output>
</structure>

<input>
    Fix the login button - it's not responding to clicks on mobile
</input>
<output
    ((arguments . (\"Fix login button on mobile\"))
     (flags . ((type . \"bug\")
               (priority . 1)
               (description . \"Login button not responding to clicks on mobile devices\"))))
</output>

<input>
%s
</input>
<output>
```
" (string-join (seq-map (lambda (line)
                          (format "    %s" line))
                        (string-lines issue-text))
               "\n")))

(defun beads-issue--create-via-agent (issue-text worktree-path)
  "Create a bd issue via AI agent asynchronously.

ISSUE-TEXT is the free-form description from the user.
WORKTREE-PATH is the directory to run the command in."
  (let ((prompt (beads-issue--make-prompt issue-text)))
    (message "Analyzing input...")
    (beads-agent-completion prompt
                            (lambda (output)
                              (beads-issue--parse-and-create output worktree-path issue-text))
                            worktree-path)))

(defun beads-issue--parse-and-create (output worktree-path _input)
  "Parse alist from OUTPUT and create issue via bd command.

OUTPUT is the raw output from Claude (should be an alist).
WORKTREE-PATH is the directory to run bd in.
_INPUT is the original user input (for error reporting)."
  (let* ((cleaned-output (string-trim (string-replace "```" "" output)))
         (issue-alist (read cleaned-output))
         (default-directory worktree-path))
    (debug)
    (beads-process-call (list :bd "create"
                              :arguments (alist-get 'arguments issue-alist)
                              :flags (alist-get 'flags issue-alist)
                              :callback (lambda (result)
                                          (message "Issue created successfully: %s" result))))))



;;; Message History

(defun beads-issue--prepare-message-ring ()
  "Initialize the message ring for this buffer if needed."
  (unless beads-issue-message-ring
    (setq beads-issue-message-ring (make-ring beads-issue-message-ring-size)))
  (make-local-variable 'beads-issue-message-ring-index))

(defun beads-issue--buffer-message ()
  "Extract the user's issue description from the buffer.
Returns nil if the buffer contains only whitespace and/or comments."
  (let ((text (beads-issue--get-buffer-text)))
    (and (not (string-match-p "\\`[ \t\n\r]*\\'" text))
         text)))

(defun beads-issue--new-message-index (offset len)
  "Calculate new message ring index.
OFFSET is the relative movement (positive = backward, negative = forward).
LEN is the ring length."
  (if beads-issue-message-ring-index
      (mod (+ beads-issue-message-ring-index offset) len)
    (1- len)))

(defun beads-issue-prev-message (arg)
  "Cycle backward through message history, after saving current message.
With a numeric prefix ARG, go back ARG messages."
  (interactive "*p")
  (beads-issue--prepare-message-ring)
  (let ((len (ring-length beads-issue-message-ring)))
    (if (<= len 0)
        (progn (message "Empty issue description history") (ding))
      ;; Save the current non-empty message to the ring
      (when-let* ((message (beads-issue--buffer-message)))
        (unless (ring-member beads-issue-message-ring message)
          (ring-insert beads-issue-message-ring message)
          (cl-incf arg)
          (setq len (ring-length beads-issue-message-ring))))
      ;; Delete the current text but not comment lines
      (delete-region (point-min) (point-max))
      ;; Calculate new index and insert the message
      (setq beads-issue-message-ring-index (beads-issue--new-message-index arg len))
      (message "Issue description %d" (1+ beads-issue-message-ring-index))
      (insert (ring-ref beads-issue-message-ring beads-issue-message-ring-index))
      ;; Save position before comments for cursor placement
      (let ((end-of-message (point)))
        ;; Re-insert template comments at the end (template already starts with newlines)
        (goto-char (point-max))
        (insert beads-issue-template)
        ;; Position cursor on the newline before the template comments
        (goto-char (1+ end-of-message))))))

(defun beads-issue-next-message (arg)
  "Cycle forward through message history, after saving current message.
With a numeric prefix ARG, go forward ARG messages."
  (interactive "*p")
  (beads-issue-prev-message (- arg)))

(defun beads-issue-save-message ()
  "Save current issue description to the history ring."
  (interactive)
  (beads-issue--prepare-message-ring)
  (when-let* ((message (beads-issue--buffer-message)))
    (when-let* ((index (ring-member beads-issue-message-ring message)))
      (ring-remove beads-issue-message-ring index))
    (ring-insert beads-issue-message-ring message)))

(provide 'beads-create)
;;; beads-create.el ends here
