;;; beads-process-tests.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'beads-process)
(require 'ert)

;;; Tests for beads-process-command-to-process-args

(ert-deftest beads-process-command-to-process-args--simple ()
  "Test conversion with just command and arguments."
  (let ((beads-program "bd"))
    (should (equal (beads-process-command-to-process-args
                    '(:bd "create" :arguments ("my-title")))
                   '("bd" "create" "my-title")))))

(ert-deftest beads-process-command-to-process-args--flag-with-string ()
  "Test conversion with string flag."
  (let ((beads-program "bd"))
    (should (equal (beads-process-command-to-process-args
                    '(:bd "create"
                      :arguments ("my-title")
                      :flags ((type . "bug"))))
                   '("bd" "create" "my-title" "--type" "bug")))))

(ert-deftest beads-process-command-to-process-args--flag-with-number ()
  "Test conversion with number flag."
  (let ((beads-program "bd"))
    (should (equal (beads-process-command-to-process-args
                    '(:bd "create"
                      :arguments ("my-title")
                      :flags ((priority . 1))))
                   '("bd" "create" "my-title" "--priority" "1")))))

(ert-deftest beads-process-command-to-process-args--flag-with-list ()
  "Test conversion with list flag (should be comma-joined)."
  (let ((beads-program "bd"))
    (should (equal (beads-process-command-to-process-args
                    '(:bd "create"
                      :arguments ("my-title")
                      :flags ((labels . ("foo" "bar" "baz")))))
                   '("bd" "create" "my-title" "--labels" "foo,bar,baz")))))

(ert-deftest beads-process-command-to-process-args--flag-with-underscore ()
  "Test conversion with underscore in flag name (should become dash)."
  (let ((beads-program "bd"))
    (should (equal (beads-process-command-to-process-args
                    '(:bd "create"
                      :arguments ("my-title")
                      :flags ((external_ref . "GH-123"))))
                   '("bd" "create" "my-title" "--external-ref" "GH-123")))))

(ert-deftest beads-process-command-to-process-args--complex ()
  "Test conversion with multiple arguments and flags of different types."
  (let* ((beads-program "bd")
         (result (beads-process-command-to-process-args
                  '(:bd "create"
                    :arguments ("my-title")
                    :flags ((type . "bug")
                            (priority . 2)
                            (labels . ("urgent" "frontend"))
                            (external_ref . "JIRA-456"))))))
    (should (equal (nth 0 result) "bd"))
    (should (equal (nth 1 result) "create"))
    (should (equal (nth 2 result) "my-title"))
    (should (member "--type" result))
    (should (member "bug" result))
    (should (member "--priority" result))
    (should (member "2" result))
    (should (member "--labels" result))
    (should (member "urgent,frontend" result))
    (should (member "--external-ref" result))
    (should (member "JIRA-456" result))))

(ert-deftest beads-process-command-to-process-args--arbitrary-command ()
  "Test conversion with arbitrary :command instead of :bd."
  (should (equal (beads-process-command-to-process-args
                  '(:command "echo" :arguments ("hello" "world")))
                 '("echo" "hello" "world"))))

(ert-deftest beads-process-command-to-process-args--arbitrary-command-with-flags ()
  "Test conversion with arbitrary :command with flags."
  (should (equal (beads-process-command-to-process-args
                  '(:command "git"
                    :arguments ("commit")
                    :flags ((message . "test commit")
                            (author . "Test User"))))
                 '("git" "commit" "--message" "test commit" "--author" "Test User"))))

;;; Integration tests

(ert-deftest beads-process-call--single-command--no-args ()
  "Test single command with no arguments."
  (let* ((beads-program "echo")
         (beads-proc-buffer (generate-new-buffer " *test-bd-proc*"))
         (beads-process-commands-queue nil)
         callback-called
         callback-result)
    (unwind-protect
        (progn
          (beads-process-call (list :bd "1"
                                  :arguments nil
                                  :callback (lambda (str)
                                              (setq callback-called t
                                                    callback-result str))))
          ;; Wait for the callback to be called
          (while (not callback-called)
            (accept-process-output nil 0.1))

          (should callback-called)
          (should (equal "1" callback-result)))

      (ignore-errors
        (kill-buffer beads-proc-buffer)))))

(ert-deftest beads-process-call--single-command--with-args ()
  "Test single command with arguments."
  (let* ((beads-program "echo")
         (beads-proc-buffer (generate-new-buffer " *test-bd-proc*"))
         (beads-process-commands-queue nil)
         callback-called
         callback-result)
    (unwind-protect
        (progn
          (beads-process-call (list :bd "1"
                                  :arguments '("a" "b")
                                  :callback (lambda (str)
                                              (setq callback-called t
                                                    callback-result str))))

          ;; Wait for the callback to be called
          (while (not callback-called)
            (accept-process-output nil 0.1))

          (should callback-called)
          (should (equal "1 a b" callback-result)))

      (ignore-errors
        (kill-buffer beads-proc-buffer)))))

(ert-deftest beads-process-call--multiple-commands ()
  "Test multiple commands executed sequentially."
  (let* ((beads-program "echo")
         (beads-proc-buffer (generate-new-buffer " *test-bd-proc*"))
         (beads-process-commands-queue nil)
         first-callback-called
         first-callback-result
         second-callback-called
         second-callback-result)
    (unwind-protect
        (progn

          (beads-process-call (list :bd "1"
                                  :arguments nil
                                  :callback (lambda (str)
                                              (setq first-callback-called t
                                                    first-callback-result str)))
                            (list :bd "2"
                                  :arguments nil
                                  :callback (lambda (str)
                                              (setq second-callback-called t
                                                    second-callback-result str))))

          (while (not (and first-callback-called second-callback-called))
            (accept-process-output nil 0.1))

          (should first-callback-called)
          (should second-callback-called)
          (should (equal "1" first-callback-result))
          (should (equal "2" second-callback-result)))

      (ignore-errors
        (kill-buffer beads-proc-buffer)))))

(ert-deftest beads-process-call--command-without-callback ()
  "Test command execution without a callback."
  (let* ((beads-program "echo")
         (beads-proc-buffer (generate-new-buffer " *test-bd-proc*"))
         (beads-process-commands-queue nil)
         queue-emptied)

    (unwind-protect
        (progn
          (beads-process-call (list :bd "test"
                                  :arguments '("foo")))

          ;; Wait for queue to empty (command completed)
          (while (not queue-emptied)
            (setq queue-emptied (null beads-process-commands-queue))
            (accept-process-output nil 0.1))

          (should queue-emptied))

      (ignore-errors
        (kill-buffer beads-proc-buffer)))))

(ert-deftest beads-process-call--failed-command-stops-sequence ()
  "Test that a failed command stops subsequent commands from executing."
  (let* ((beads-program "false")  ; Command that always fails
         (beads-proc-buffer (generate-new-buffer " *test-bd-proc*"))
         (beads-process-commands-queue nil)
         (first-completed nil)
         (second-callback-called nil)
         (queue-emptied nil))
    (unwind-protect
        (progn
          (beads-process-call (list :bd ""
                                  :arguments nil
                                  :flags nil
                                  :callback (lambda (_str)
                                              (setq first-completed t)))
                            (list :bd ""
                                  :arguments nil
                                  :flags nil
                                  :callback (lambda (_str)
                                              (setq second-callback-called t))))

          ;; Wait for queue to empty (both commands processed, one way or another)
          (while (not queue-emptied)
            (setq queue-emptied (null beads-process-commands-queue))
            (accept-process-output nil 0.1))

          (should-not first-completed)
          (should-not second-callback-called))

      (ignore-errors
        (kill-buffer beads-proc-buffer)))))

(ert-deftest beads-process-call--with-flags ()
  "Test command with both arguments and flags."
  (let* ((beads-program "echo")
         (beads-proc-buffer (generate-new-buffer " *test-bd-proc*"))
         (beads-process-commands-queue nil)
         callback-called
         callback-result)
    (unwind-protect
        (progn
          (beads-process-call (list :bd "create"
                                  :arguments '("my-title")
                                  :flags '((type . "bug")
                                           (priority . 1)
                                           (labels . ("foo" "bar")))
                                  :callback (lambda (str)
                                              (setq callback-called t
                                                    callback-result str))))

          (while (not callback-called)
            (accept-process-output nil 0.1))

          (should callback-called)
          ;; Should output: create my-title --type bug --priority 1 --labels foo,bar
          (should (string-match-p (rx "create" (+ space) "my-title") callback-result))
          (should (string-match-p (rx "--type" (+ space) "bug") callback-result))
          (should (string-match-p (rx "--priority" (+ space) "1") callback-result))
          (should (string-match-p (rx "--labels" (+ space) "foo,bar") callback-result)))

      (ignore-errors
        (kill-buffer beads-proc-buffer)))))

(provide 'beads-process-tests)

;;; beads-process-tests.el ends here
