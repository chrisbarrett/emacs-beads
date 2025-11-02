;;; beads-issue-tests.el --- Tests for beads-issue -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'beads-issue)
(require 'buttercup)


;;; Test data

(defconst beads-issue-tests--minimal-json-string
  "{\"id\":\"bd-test\",\"title\":\"Test issue\",\"status\":\"open\",\"priority\":2,\"issue_type\":\"task\",\"created_at\":\"2025-11-01T10:00:00+13:00\",\"updated_at\":\"2025-11-01T10:00:00+13:00\"}"
  "Minimal valid issue as JSON string.")

(defconst beads-issue-tests--full-json-string
  "{\"id\":\"bd-full\",\"content_hash\":\"abc123\",\"title\":\"Full test issue\",\"description\":\"This is a description\",\"design\":\"This is the design\",\"acceptance_criteria\":\"This is acceptance criteria\",\"status\":\"in_progress\",\"priority\":1,\"issue_type\":\"feature\",\"created_at\":\"2025-11-01T10:00:00+13:00\",\"updated_at\":\"2025-11-01T11:00:00+13:00\",\"closed_at\":\"2025-11-01T12:00:00+13:00\"}"
  "Full issue with all fields as JSON string.")

(defconst beads-issue-tests--array-json-string
  "[{\"id\":\"bd-1\",\"title\":\"First\",\"status\":\"open\",\"priority\":2,\"issue_type\":\"task\",\"created_at\":\"2025-11-01T10:00:00+13:00\",\"updated_at\":\"2025-11-01T10:00:00+13:00\"},{\"id\":\"bd-2\",\"title\":\"Second\",\"status\":\"closed\",\"priority\":1,\"issue_type\":\"bug\",\"created_at\":\"2025-11-01T10:00:00+13:00\",\"updated_at\":\"2025-11-01T11:00:00+13:00\"}]"
  "Array of issues as JSON string.")

;;; Core parsing tests

(describe "beads-issue--kebab-keyword"
  (it "converts snake_case to kebab-case"
    (expect (beads-issue--kebab-keyword :issue_type) :to-equal :issue-type)
    (expect (beads-issue--kebab-keyword :created_at) :to-equal :created-at)
    (expect (beads-issue--kebab-keyword :content_hash) :to-equal :content-hash))

  (it "leaves kebab-case unchanged"
    (expect (beads-issue--kebab-keyword :id) :to-equal :id)
    (expect (beads-issue--kebab-keyword :title) :to-equal :title)))

(describe "beads-issue--parse-json-object"
  (it "converts all keys to kebab-case keywords"
    (let* ((input '((:id . "bd-1")
                    (:issue_type . "task")
                    (:created_at . "2025-11-01")))
           (result (beads-issue--parse-json-object input)))
      (expect (alist-get :id result) :to-be-truthy)
      (expect (alist-get :issue-type result) :to-be-truthy)
      (expect (alist-get :created-at result) :to-be-truthy)
      (expect (alist-get :issue_type result) :not :to-be-truthy)
      (expect (alist-get :created_at result) :not :to-be-truthy)))

  (it "preserves all values"
    (let* ((input '((:id . "bd-test")
                    (:title . "Test")
                    (:priority . 2)))
           (result (beads-issue--parse-json-object input)))
      (expect (alist-get :id result) :to-equal "bd-test")
      (expect (alist-get :title result) :to-equal "Test")
      (expect (alist-get :priority result) :to-equal 2))))

;;; Single issue parsing tests

(describe "beads-issue-parse-json-issue"
  (it "parses minimal valid JSON string"
    (let ((result (beads-issue-parse-json-issue beads-issue-tests--minimal-json-string)))
      (expect (alist-get :id result) :to-equal "bd-test")
      (expect (alist-get :title result) :to-equal "Test issue")
      (expect (alist-get :status result) :to-equal "open")
      (expect (alist-get :priority result) :to-equal 2)
      (expect (alist-get :issue-type result) :to-equal "task")
      (expect (alist-get :created-at result) :to-equal "2025-11-01T10:00:00+13:00")
      (expect (alist-get :updated-at result) :to-equal "2025-11-01T10:00:00+13:00")))

  (it "parses full JSON with all fields"
    (let ((result (beads-issue-parse-json-issue beads-issue-tests--full-json-string)))
      (expect (alist-get :id result) :to-equal "bd-full")
      (expect (alist-get :content-hash result) :to-equal "abc123")
      (expect (alist-get :title result) :to-equal "Full test issue")
      (expect (alist-get :description result) :to-equal "This is a description")
      (expect (alist-get :design result) :to-equal "This is the design")
      (expect (alist-get :acceptance-criteria result) :to-equal "This is acceptance criteria")
      (expect (alist-get :status result) :to-equal "in_progress")
      (expect (alist-get :priority result) :to-equal 1)
      (expect (alist-get :issue-type result) :to-equal "feature")
      (expect (alist-get :closed-at result) :to-equal "2025-11-01T12:00:00+13:00")))

  (it "converts snake_case keys to kebab-case"
    (let ((result (beads-issue-parse-json-issue beads-issue-tests--minimal-json-string)))
      ;; Should have kebab-case keys
      (expect (alist-get :issue-type result) :to-be-truthy)
      (expect (alist-get :created-at result) :to-be-truthy)
      (expect (alist-get :updated-at result) :to-be-truthy)
      ;; Should NOT have snake_case keys
      (expect (alist-get :issue_type result) :not :to-be-truthy)
      (expect (alist-get :created_at result) :not :to-be-truthy)
      (expect (alist-get :updated_at result) :not :to-be-truthy))))

;;; Array parsing tests

(describe "beads-issue-parse-json-issues"
  (it "parses valid JSON array"
    (let ((result (beads-issue-parse-json-issues beads-issue-tests--array-json-string)))
      (expect (length result) :to-equal 2)
      (expect (alist-get :id (nth 0 result)) :to-equal "bd-1")
      (expect (alist-get :title (nth 0 result)) :to-equal "First")
      (expect (alist-get :status (nth 0 result)) :to-equal "open")
      (expect (alist-get :id (nth 1 result)) :to-equal "bd-2")
      (expect (alist-get :title (nth 1 result)) :to-equal "Second")
      (expect (alist-get :status (nth 1 result)) :to-equal "closed")))

  (it "parses empty array"
    (let ((result (beads-issue-parse-json-issues "[]")))
      (expect result :to-equal nil)))

  (it "converts keys in all elements"
    (let ((result (beads-issue-parse-json-issues beads-issue-tests--array-json-string)))
      (dolist (issue result)
        (expect (alist-get :issue-type issue) :to-be-truthy)
        (expect (alist-get :created-at issue) :to-be-truthy)
        (expect (alist-get :issue_type issue) :not :to-be-truthy)))))

;;; Field access tests

(describe "beads-issue field access"
  (let ((issue (beads-issue-parse-json-issue beads-issue-tests--full-json-string)))

    (it "can access all fields with alist-get"
      (expect (alist-get :id issue) :to-equal "bd-full")
      (expect (alist-get :title issue) :to-equal "Full test issue")
      (expect (alist-get :status issue) :to-equal "in_progress")
      (expect (alist-get :priority issue) :to-equal 1)
      (expect (alist-get :issue-type issue) :to-equal "feature")
      (expect (alist-get :description issue) :to-equal "This is a description")
      (expect (alist-get :design issue) :to-equal "This is the design")
      (expect (alist-get :acceptance-criteria issue) :to-equal "This is acceptance criteria")
      (expect (alist-get :created-at issue) :to-equal "2025-11-01T10:00:00+13:00")
      (expect (alist-get :updated-at issue) :to-equal "2025-11-01T11:00:00+13:00")
      (expect (alist-get :closed-at issue) :to-equal "2025-11-01T12:00:00+13:00")))

  (describe "with minimal issue"
    (let ((issue (beads-issue-parse-json-issue beads-issue-tests--minimal-json-string)))

      (it "returns nil for missing optional fields"
        (expect (alist-get :description issue) :to-be nil)
        (expect (alist-get :design issue) :to-be nil)
        (expect (alist-get :acceptance-criteria issue) :to-be nil)
        (expect (alist-get :closed-at issue) :to-be nil)))))

;;; Real-world integration tests

(describe "beads-issue"
  (describe "parsing real bd list --json output"
    (it "handles the actual structure from bd"
      ;; This is actual output from bd list --json (first issue only)
      (let* ((real-json "{\"id\":\"bd-c80d\",\"content_hash\":\"54296e2d26d66ed6e1feeab35b4ae7a2f21a0cb11e1df0b1185cb08ff435eace\",\"title\":\"Move beads-issue.el contents to beads-create.el and extract issue parsing functions\",\"description\":\"Refactor beads-issue.el to better separate concerns: move the current buffer-based issue creation UI to beads-create.el, and extract/test the core issue parsing functions in beads-issue.el. This will make beads-issue.el a better place for shared issue data structures and parsing logic that can be reused by beads-show and other modules.\",\"design\":\"1. Rename beads-issue.el to beads-create.el (keeping all the buffer UI code there)\\n2. Create new beads-issue.el with:\\n   - Issue alist parsing/validation functions\\n   - JSON parsing utilities for bd list --json output\\n   - Shared issue data structure helpers\\n3. Update require statements in beads-create.el\\n4. Add buttercup tests for the parsing functions in beads-issue.el\",\"acceptance_criteria\":\"- beads-create.el contains all the buffer-based UI code\\n- beads-issue.el contains tested issue parsing/data functions\\n- All existing functionality still works\\n- Tests pass for issue parsing functions\",\"status\":\"in_progress\",\"priority\":1,\"issue_type\":\"chore\",\"created_at\":\"2025-11-01T17:45:21.485081+13:00\",\"updated_at\":\"2025-11-01T17:52:33.060131+13:00\"}")
             (result (beads-issue-parse-json-issue real-json)))
        (expect (alist-get :id result) :to-equal "bd-c80d")
        (expect (alist-get :status result) :to-equal "in_progress")
        (expect (alist-get :issue-type result) :to-equal "chore")
        (expect (alist-get :priority result) :to-equal 1)
        (expect (alist-get :description result) :to-match "Refactor beads-issue.el")
        (expect (alist-get :design result) :to-match "Rename beads-issue.el")
        (expect (alist-get :acceptance-criteria result) :to-match "beads-create.el contains")))))

(provide 'beads-issue-tests)

;;; beads-issue-tests.el ends here
