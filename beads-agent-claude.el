;;; beads-agent-claude.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'beads-agent)
(require 'beads-vars)

(defcustom beads-agent-claude-program "claude"
  "Name or path of the Claude CLI program."
  :type 'string
  :group 'beads)

(defcustom beads-agent-claude-model "haiku"
  "Claude model to use for completions."
  :type '(choice (const :tag "Haiku (fastest, cheapest)" "haiku")
          (const :tag "Sonnet (balanced)" "sonnet")
          (const :tag "Opus (most capable)" "opus")
          (string :tag "Other model name"))
  :group 'beads)

(cl-defmethod beads-agent-completion-impl ((_backend (eql claude)) prompt callback &optional worktree-path)
  "Request completion from Claude Code CLI.

PROMPT is the input string to send to Claude.
CALLBACK is a function called with Claude's response string.
WORKTREE-PATH is the optional working directory for the process."
  (unless (executable-find beads-agent-claude-program)
    (user-error "Claude program not found: %s" beads-agent-claude-program))

  (let ((default-directory (or worktree-path default-directory)))
    (beads-process-call
     (list :command beads-agent-claude-program
           :arguments (list "--model" beads-agent-claude-model)
           :stdin prompt
           :callback callback))))

(provide 'beads-agent-claude)

;;; beads-agent-claude.el ends here
