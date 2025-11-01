;;; beads-issue.el --- Issue data structures and parsing -*- lexical-binding: t; -*-

;;; Commentary:

;; This library provides data structures and parsing functions for beads (bd)
;; issues. It can parse JSON output from `bd list --json` and `bd show --json`
;; into Emacs Lisp data structures.

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Issue data structure

;; An issue is represented as an alist with keyword keys.
;; Keys use kebab-case (e.g., :issue-type, :created-at).
;;
;; The structure comes directly from bd's JSON output with underscores
;; converted to hyphens.

;;; Core parsing function

(defun beads-issue--kebab-keyword (keyword)
  "Convert KEYWORD from snake_case to kebab-case.
Examples: :issue_type -> :issue-type, :created_at -> :created-at"
  (intern (concat ":" (replace-regexp-in-string "_" "-" (substring (symbol-name keyword) 1)))))

(defun beads-issue--parse-json-object (json-obj)
  "Parse a single issue from JSON-OBJ (an alist with keyword keys).
Converts snake_case keywords to kebab-case keywords."
  (mapcar (lambda (pair)
            (cons (beads-issue--kebab-keyword (car pair))
                  (cdr pair)))
          json-obj))

;;; Public API

(defun beads-issue-parse-json-issue (json-string)
  "Parse a single issue from JSON-STRING.
Returns an alist with kebab-case keyword keys."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (json-null nil)
         (parsed (json-read-from-string json-string)))
    (beads-issue--parse-json-object parsed)))

(defun beads-issue-parse-json-issues (json-string)
  "Parse an array of issues from JSON-STRING.
Returns a list of alists with kebab-case keyword keys."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (json-null nil)
         (parsed (json-read-from-string json-string)))
    (mapcar #'beads-issue--parse-json-object parsed)))

(provide 'beads-issue)
;;; beads-issue.el ends here
