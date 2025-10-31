;;; beads-agent.el --- AI agent abstraction for beads -*- lexical-binding: t; -*-

;;; Commentary:

;; This library provides an abstraction layer over different AI agent backends
;; for processing natural language input into structured issue data.
;;
;; The main entry point is `beads-agent-completion', which dispatches to the
;; appropriate backend based on `beads-agent-backend'.

;;; Code:

(require 'cl-lib)

(require 'beads-vars)
(require 'beads-process)

(defun beads-agent-completion (prompt callback &optional worktree-path)
  "Request a completion from an AI agent.

Delegates to concrete implementation according to the value of
`beads-agent-backend'.

PROMPT is the input string to send to the agent.
CALLBACK is a function called with the agent's response string.
WORKTREE-PATH is the optional working directory for the agent process."
  (beads-agent-completion beads-agent-backend prompt callback worktree-path))

(cl-defgeneric beads-agent-completion-impl (backend prompt callback &optional worktree-path)
  "Request a completion from an AI agent.

BACKEND is the agent backend to use (e.g., \\='claude).
PROMPT is the input string to send to the agent.
CALLBACK is a function called with the agent's response string.
WORKTREE-PATH is the optional working directory for the agent process.")

(provide 'beads-agent)

;;; beads-agent.el ends here
