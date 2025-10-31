;;; beads-process-tests.el --- Tests for beads-process -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'beads-process)
(require 'buttercup)

(describe "beads-process-command-to-process-args"

  (describe "basic command conversion"
    (it "converts simple command with arguments"
      (let ((beads-program "bd"))
        (expect (beads-process-command-to-process-args
                 '(:bd "create" :arguments ("my-title")))
                :to-equal
                '("bd" "create" "my-title"))))

    (it "converts arbitrary command instead of bd"
      (expect (beads-process-command-to-process-args
               '(:command "echo" :arguments ("hello" "world")))
              :to-equal
              '("echo" "hello" "world"))))

  (describe "flag conversion"
    (it "converts string flag"
      (let ((beads-program "bd"))
        (expect (beads-process-command-to-process-args
                 '(:bd "create"
                   :arguments ("my-title")
                   :flags ((type . "bug"))))
                :to-equal
                '("bd" "create" "my-title" "--type" "bug"))))

    (it "converts number flag"
      (let ((beads-program "bd"))
        (expect (beads-process-command-to-process-args
                 '(:bd "create"
                   :arguments ("my-title")
                   :flags ((priority . 1))))
                :to-equal
                '("bd" "create" "my-title" "--priority" "1"))))

    (it "converts list flag to comma-joined string"
      (let ((beads-program "bd"))
        (expect (beads-process-command-to-process-args
                 '(:bd "create"
                   :arguments ("my-title")
                   :flags ((labels . ("foo" "bar" "baz")))))
                :to-equal
                '("bd" "create" "my-title" "--labels" "foo,bar,baz"))))

    (it "converts underscore in flag name to dash"
      (let ((beads-program "bd"))
        (expect (beads-process-command-to-process-args
                 '(:bd "create"
                   :arguments ("my-title")
                   :flags ((external_ref . "GH-123"))))
                :to-equal
                '("bd" "create" "my-title" "--external-ref" "GH-123"))))

    (it "converts arbitrary command with flags"
      (expect (beads-process-command-to-process-args
               '(:command "git"
                 :arguments ("commit")
                 :flags ((message . "test commit")
                         (author . "Test User"))))
              :to-equal
              '("git" "commit" "--message" "test commit" "--author" "Test User"))))

  (describe "complex conversions"
    (it "handles multiple arguments and flags of different types"
      (let* ((beads-program "bd")
             (result (beads-process-command-to-process-args
                      '(:bd "create"
                        :arguments ("my-title")
                        :flags ((type . "bug")
                                (priority . 2)
                                (labels . ("urgent" "frontend"))
                                (external_ref . "JIRA-456"))))))
        (expect (nth 0 result) :to-equal "bd")
        (expect (nth 1 result) :to-equal "create")
        (expect (nth 2 result) :to-equal "my-title")
        (expect result :to-contain "--type")
        (expect result :to-contain "bug")
        (expect result :to-contain "--priority")
        (expect result :to-contain "2")
        (expect result :to-contain "--labels")
        (expect result :to-contain "urgent,frontend")
        (expect result :to-contain "--external-ref")
        (expect result :to-contain "JIRA-456")))))

;;; Integration tests

(describe "beads-process-call"

  (describe "single command execution"
    (it "executes single command with no arguments"
      (with-temp-buffer
        (let* ((beads-program "echo")
               (beads-proc-buffer (current-buffer))
               (beads-process-commands-queue nil)
               callback-called
               callback-result)
          (beads-process-call (list :bd "1"
                                    :arguments nil
                                    :callback (lambda (str)
                                                (setq callback-called t
                                                      callback-result str))))
          ;; Wait for the callback to be called
          (while (not callback-called)
            (accept-process-output nil 0.1))

          (expect callback-called :to-be-truthy)
          (expect callback-result :to-equal "1"))))

    (it "executes single command with arguments"
      (with-temp-buffer
        (let* ((beads-program "echo")
               (beads-proc-buffer (current-buffer))
               (beads-process-commands-queue nil)
               callback-called
               callback-result)
          (beads-process-call (list :bd "1"
                                    :arguments '("a" "b")
                                    :callback (lambda (str)
                                                (setq callback-called t
                                                      callback-result str))))

          ;; Wait for the callback to be called
          (while (not callback-called)
            (accept-process-output nil 0.1))

          (expect callback-called :to-be-truthy)
          (expect callback-result :to-equal "1 a b"))))

    (it "executes command without callback"
      (with-temp-buffer
        (let* ((beads-program "echo")
               (beads-proc-buffer (current-buffer))
               (beads-process-commands-queue nil)
               queue-emptied)
          (beads-process-call (list :bd "test"
                                    :arguments '("foo")))

          ;; Wait for queue to empty (command completed)
          (while (not queue-emptied)
            (setq queue-emptied (null beads-process-commands-queue))
            (accept-process-output nil 0.1))

          (expect queue-emptied :to-be-truthy)))))

  (describe "multiple command execution"
    (it "executes multiple commands sequentially"
      (with-temp-buffer
        (let* ((beads-program "echo")
               (beads-proc-buffer (current-buffer))
               (beads-process-commands-queue nil)
               first-callback-called
               first-callback-result
               second-callback-called
               second-callback-result)
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

          (expect first-callback-called :to-be-truthy)
          (expect second-callback-called :to-be-truthy)
          (expect first-callback-result :to-equal "1")
          (expect second-callback-result :to-equal "2")))))

  (describe "error handling"
    (it "stops sequence when command fails"
      (with-temp-buffer
        (let* ((beads-program "false")  ; Command that always fails
               (beads-proc-buffer (current-buffer))
               (beads-process-commands-queue nil)
               (first-completed nil)
               (second-callback-called nil)
               (queue-emptied nil))
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

          (expect first-completed :not :to-be-truthy)
          (expect second-callback-called :not :to-be-truthy)))))

  (describe "command with flags"
    (it "executes command with both arguments and flags"
      (with-temp-buffer
        (let* ((beads-program "echo")
               (beads-proc-buffer (current-buffer))
               (beads-process-commands-queue nil)
               callback-called
               callback-result)
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

          (expect callback-called :to-be-truthy)
          ;; Should output: create my-title --type bug --priority 1 --labels foo,bar
          (expect callback-result :to-match (rx "create" (+ space) "my-title"))
          (expect callback-result :to-match (rx "--type" (+ space) "bug"))
          (expect callback-result :to-match (rx "--priority" (+ space) "1"))
          (expect callback-result :to-match (rx "--labels" (+ space) "foo,bar")))))))

(provide 'beads-process-tests)

;;; beads-process-tests.el ends here
