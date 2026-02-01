;;; eglot-signature-test.el --- Tests for eglot-signature -*- lexical-binding: t -*-

;;; Code:
(require 'eglot)
(require 'eglot-signature)
(require 'ert)

;; Test: eglot-signature--request basic invoked request
(ert-deftest test-eglot-signature--request-basic-invoked ()
  "Test basic invoked signature help request with mock server."
  (let ((mock-server (make-hash-table :test 'equal))
        (request-params nil)
        (did-change-called nil))

    (with-temp-buffer
      (let ((test-buffer (current-buffer))
            (point-before-call (point)))

        (cl-letf* (((symbol-function 'eglot-managed-p)
                    (lambda () t))
                   ((symbol-function 'eglot-current-server)
                    (lambda () mock-server))
                   ((symbol-function 'eglot--signal-textDocument/didChange)
                    (lambda ()
                      (setq did-change-called t)))
                   ((symbol-function 'eglot--TextDocumentPositionParams)
                    (lambda ()
                      '(:textDocument (:uri "test://file.el") :position (:line 0 :character 0))))
                   ((symbol-function 'jsonrpc-async-request)
                    (lambda (server method params &rest _args)
                      (setq request-params (list server method params)))))

          (eglot-signature--request :invoked)

          ;; Verify didChange was called
          (should did-change-called)

          ;; Verify request parameters
          (should (equal (car request-params) mock-server))
          (should (equal (cadr request-params) :textDocument/signatureHelp))

          ;; Verify context structure
          (let ((params (caddr request-params))
                (context (plist-get (caddr request-params) :context)))
            (should context)
            (should (eq (plist-get context :triggerKind) 1))  ; :invoked = 1
            (should (eq (plist-get context :isRetrigger) :json-false)))

          ;; Verify side effects
          (should (eq eglot-signature--active-buffer test-buffer))
          (should (eq eglot-signature--active-point point-before-call)))))))

(ert-deftest test-eglot-signature--request-no-server ()
  "Test that request does nothing when no active eglot server."
  (let ((jsonrpc-called nil)
        (did-change-called nil)
        (active-buffer-before eglot-signature--active-buffer)
        (active-point-before eglot-signature--active-point))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-managed-p)
                  (lambda () nil))
                 ((symbol-function 'eglot-current-server)
                  (lambda () nil))
                 ((symbol-function 'eglot--signal-textDocument/didChange)
                  (lambda ()
                    (setq did-change-called t)))
                 ((symbol-function 'jsonrpc-async-request)
                  (lambda (&rest _args)
                    (setq jsonrpc-called t))))
        (eglot-signature--request :invoked)
        (should (not jsonrpc-called))
        (should (not did-change-called))
        (should (eq eglot-signature--active-buffer active-buffer-before))
        (should (eq eglot-signature--active-point active-point-before))))))

(ert-deftest test-eglot-signature--request-retrigger ()
  "Test retrigger scenario with active signature."
  (let ((mock-server (make-hash-table :test 'equal))
        (request-params nil)
        (active-signature-mock '(:signatures [(:label "test")])))
    (with-temp-buffer
      (let ((test-buffer (current-buffer))
            (point-before-call (point)))
        (setq eglot-signature--active-signature active-signature-mock)
        (cl-letf* (((symbol-function 'eglot-managed-p)
                    (lambda () t))
                   ((symbol-function 'eglot-current-server)
                    (lambda () mock-server))
                   ((symbol-function 'eglot-signature--sig-active-p)
                    (lambda () t))
                   ((symbol-function 'eglot--signal-textDocument/didChange)
                    (lambda () nil))
                   ((symbol-function 'eglot--TextDocumentPositionParams)
                    (lambda ()
                      '(:textDocument (:uri "test://file.el") :position (:line 0 :character 0))))
                   ((symbol-function 'jsonrpc-async-request)
                    (lambda (server method params &rest _args)
                      (setq request-params (list server method params)))))
          (eglot-signature--request :trigger-character)
          (should (equal (car request-params) mock-server))
          (should (equal (cadr request-params) :textDocument/signatureHelp))
          (let ((context (plist-get (caddr request-params) :context)))
            (should context)
            (should (eq (plist-get context :triggerKind) 2))
            (should (plist-get context :isRetrigger))
            (should (equal (plist-get context :activeSignatureHelp) active-signature-mock)))
          (should (eq eglot-signature--active-buffer test-buffer))
          (should (eq eglot-signature--active-point point-before-call)))))))

(ert-deftest test-eglot-signature--request-content-change ()
  "Test content-change trigger kind."
  (let ((mock-server (make-hash-table :test 'equal))
        (request-params nil))
    (with-temp-buffer
      (let ((test-buffer (current-buffer))
            (point-before-call (point)))
        (cl-letf* (((symbol-function 'eglot-managed-p)
                    (lambda () t))
                   ((symbol-function 'eglot-current-server)
                    (lambda () mock-server))
                   ((symbol-function 'eglot--signal-textDocument/didChange)
                    (lambda () nil))
                   ((symbol-function 'eglot--TextDocumentPositionParams)
                    (lambda ()
                      '(:textDocument (:uri "test://file.el") :position (:line 0 :character 0))))
                   ((symbol-function 'jsonrpc-async-request)
                    (lambda (server method params &rest _args)
                      (setq request-params (list server method params)))))
          (eglot-signature--request :content-change)
          (should (equal (car request-params) mock-server))
          (should (equal (cadr request-params) :textDocument/signatureHelp))
          (let ((context (plist-get (caddr request-params) :context)))
            (should context)
            (should (eq (plist-get context :triggerKind) 3))
            (should (eq (plist-get context :isRetrigger) :json-false)))
          (should (eq eglot-signature--active-buffer test-buffer))
          (should (eq eglot-signature--active-point point-before-call)))))))

;; Test: eglot-signature--debounce-request no-op when mode is nil
(ert-deftest test-eglot-signature--debounce-request-mode-nil ()
  "Test no-op when eglot-signature-mode is nil."
  (let ((eglot-signature-mode nil)
        (eglot-signature--debounce-timer nil)
        (run-with-idle-timer-called nil)
        (timer-set-idle-time-called nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'run-with-idle-timer)
                  (lambda (&rest _args)
                    (setq run-with-idle-timer-called t)))
                 ((symbol-function 'timer-set-idle-time)
                  (lambda (&rest _args)
                    (setq timer-set-idle-time-called t))))
        (eglot-signature--debounce-request :invoked)

        ;; Should not create or modify any timer
        (should (not run-with-idle-timer-called))
        (should (not timer-set-idle-time-called))
        (should (eq eglot-signature--debounce-timer nil))))))

(ert-deftest test-eglot-signature--debounce-request-reset-timer ()
  "Test reset timer idle time when debounce timer already exists."
  (let* ((mock-timer (timer-create))
         (eglot-signature-mode t)
         (eglot-signature-debounce-delay 0.1)
         (eglot-signature--debounce-timer mock-timer)
         (run-with-idle-timer-called nil)
         (timer-set-idle-time-called nil)
         (timer-set-idle-time-args nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf* (((symbol-function 'run-with-idle-timer)
                      (lambda (&rest _args)
                        (setq run-with-idle-timer-called t)))
                     ((symbol-function 'timer-set-idle-time)
                      (lambda (timer delay &rest _args)
                        (setq timer-set-idle-time-called t)
                        (setq timer-set-idle-time-args (list timer delay)))))
            (eglot-signature--debounce-request :invoked)

            ;; Should reset existing timer, not create new one
            (should timer-set-idle-time-called)
            (should (not run-with-idle-timer-called))

            ;; Verify correct timer and delay were used
            (should (eq (car timer-set-idle-time-args) mock-timer))
            (should (= (cadr timer-set-idle-time-args) 0.1))))
      ;; Cleanup
      (ignore-errors (cancel-timer mock-timer)))))

(ert-deftest test-eglot-signature--debounce-request-create-timer ()
  "Test create new debounce timer when timer does not exist."
  (let ((eglot-signature-mode t)
        (eglot-signature-debounce-delay 0.1)
        (eglot-signature--debounce-timer nil)
        (run-with-idle-timer-called nil)
        (run-with-idle-timer-args nil)
        (timer-set-idle-time-called nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'run-with-idle-timer)
                  (lambda (secs repeat function &rest args)
                    (setq run-with-idle-timer-called t)
                    (setq run-with-idle-timer-args (list secs repeat function args))
                    'new-timer))  ; Return mock timer
                 ((symbol-function 'timer-set-idle-time)
                  (lambda (&rest _args)
                    (setq timer-set-idle-time-called t))))
        (eglot-signature--debounce-request :trigger-character ?x)

        ;; Should create new timer, not reset existing one
        (should run-with-idle-timer-called)
        (should (not timer-set-idle-time-called))

        ;; Verify correct delay and function were used
        (should (= (car run-with-idle-timer-args) 0.1))  ; secs
        (should (not (cadr run-with-idle-timer-args)))   ; repeat (nil)
        (should (functionp (cl-caddr run-with-idle-timer-args)))  ; function

        ;; Verify timer was set
        (should eglot-signature--debounce-timer)))))

(ert-deftest test-eglot-signature--debounce-request-callback-calls-request ()
  "Test timer callback calls request when buffer is live and conditions met."
  (let ((eglot-signature-mode t)
        (eglot-signature-debounce-delay 0.1)
        (request-called nil)
        (request-args nil)
        (callback-function nil))
    (with-temp-buffer
      (let ((test-buffer (current-buffer)))
        ;; First, capture the callback function by mocking run-with-idle-timer
        (cl-letf* (((symbol-function 'run-with-idle-timer)
                    (lambda (secs repeat function &rest args)
                      (setq callback-function function)
                      'new-timer))
                   ((symbol-function 'eglot-signature--request)
                    (lambda (trigger-kind &optional trigger-char)
                      (setq request-called t)
                      (setq request-args (list trigger-kind trigger-char)))))
          (eglot-signature--debounce-request :trigger-character ?x)

          ;; Verify callback was captured
          (should (functionp callback-function))

          ;; Simulate timer callback execution with live buffer
          (setq eglot-signature--debounce-timer 'some-timer)  ; Simulate timer exists
          (funcall callback-function test-buffer)

          ;; Verify timer was cleared
          (should (eq eglot-signature--debounce-timer nil))

          ;; Verify request was called with correct arguments
          (should request-called)
          (should (eq (car request-args) :trigger-character))
          (should (eq (cadr request-args) ?x)))))))

(ert-deftest test-eglot-signature--debounce-request-callback-dead-buffer ()
  "Test timer callback does not call request when buffer is dead."
  (let ((eglot-signature-mode t)
        (eglot-signature-debounce-delay 0.1)
        (request-called nil)
        (callback-function nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'run-with-idle-timer)
                  (lambda (secs repeat function &rest args)
                    (setq callback-function function)
                    'new-timer))
                 ((symbol-function 'buffer-live-p)
                  (lambda (buf) nil))  ; Buffer is not live
                 ((symbol-function 'eglot-signature--request)
                  (lambda (&rest _args)
                    (setq request-called t))))
        (eglot-signature--debounce-request :content-change)

        ;; Verify callback was captured
        (should (functionp callback-function))

        ;; Simulate timer callback execution with dead buffer
        (setq eglot-signature--debounce-timer 'some-timer)
        (funcall callback-function (current-buffer))

        ;; Verify timer was cleared (even when buffer is dead)
        (should (eq eglot-signature--debounce-timer nil))

        ;; Verify request was NOT called (because buffer is dead)
        (should (not request-called))))))

;; Test: eglot-signature--valid-response-p
(ert-deftest test-eglot-signature--valid-response-p ()
  "Test validation of signature help responses."
  (let ((test-cases '(;; (name resp expect-result)
                      ("valid response with non-empty signatures vector"
                       (:signatures [(:label "foo(x, y)") (:label "bar(a, b, c)")])
                       t)
                      ("nil response" nil nil)
                      ("response missing :signatures key"
                       (:activeParameter 0)
                       nil)
                      ("empty signatures vector"
                       (:signatures [])
                       nil)
                      ("signatures as list instead of vector"
                       (:signatures ((:label "foo(x, y)") (:label "bar(a, b, c)")))
                       nil))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(_name ,resp ,expect-result) test-case))
        (should (eq (eglot-signature--valid-response-p resp) expect-result))))))

;; Test: eglot-signature--active
(ert-deftest test-eglot-signature--active-basic-activation ()
  "Test basic activation: prepare-buffer called, render called, hooks added, mode enabled."
  (let ((prepare-buffer-called nil)
        (render-called nil)
        (sig-buffer-mock 'mock-sig-buffer)
        (mock-sig-help '(:signatures [(:label "test(x, y)")])))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-signature--prepare-buffer)
                  (lambda (sig-help)
                    (setq prepare-buffer-called t)
                    (should (equal sig-help mock-sig-help))
                    sig-buffer-mock))
                 ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda (buf)
                    (setq render-called t)
                    (should (eq buf sig-buffer-mock))))
                 (eglot-signature-popup-mode nil))
        (eglot-signature--active mock-sig-help)

        ;; Verify prepare-buffer and render were called
        (should prepare-buffer-called)
        (should render-called)

        ;; Verify hooks were added
        (should (memq 'eglot-signature--window-change window-buffer-change-functions))
        (should (memq 'eglot-signature--window-change window-selection-change-functions))

        ;; Verify popup mode is enabled
        (should eglot-signature-popup-mode)))))

(ert-deftest test-eglot-signature--active-popup-mode-already-enabled ()
  "Test function works correctly when popup mode is already enabled."
  (let ((sig-buffer-mock 'mock-sig-buffer)
        (mock-sig-help '(:signatures [(:label "test(x, y)")])))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-signature--prepare-buffer)
                  (lambda (_sig-help) sig-buffer-mock))
                 ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda (_buf) nil))
                 (eglot-signature-popup-mode t))  ; Already enabled
        (eglot-signature--active mock-sig-help)

        ;; Verify hooks were still added
        (should (memq 'eglot-signature--window-change window-buffer-change-functions))
        (should (memq 'eglot-signature--window-change window-selection-change-functions))

        ;; Verify popup mode remains enabled
        (should eglot-signature-popup-mode)))))

;; Test: eglot-signature--quit cancel debounce timer
(ert-deftest test-eglot-signature--quit-cancel-timer ()
  "Test cancel debounce timer when active."
  (let* ((mock-timer (timer-create))
         (cancel-timer-called nil)
         (canceled-timer nil)
         (eglot-signature--debounce-timer mock-timer))
    (unwind-protect
        (progn
          (cl-letf* (((symbol-function 'cancel-timer)
                      (lambda (timer)
                        (setq cancel-timer-called t)
                        (setq canceled-timer timer)))
                     (eglot-signature--active-frame nil)
                     (eglot-signature--active-buffer nil))
            (eglot-signature--quit)

            ;; Verify cancel-timer was called
            (should cancel-timer-called)

            ;; Verify correct timer was canceled
            (should (eq canceled-timer mock-timer))))
      ;; Cleanup: cancel the timer in case test didn't clean it up
      (ignore-errors (cancel-timer mock-timer)))))

(ert-deftest test-eglot-signature--quit-hide-frame ()
  "Test hide frame when frame is live."
  (let* ((mock-frame 'mock-frame)
         (make-invisible-called nil)
         (invisible-frame nil)
         (eglot-signature--active-frame mock-frame))
    (cl-letf* (((symbol-function 'make-frame-invisible)
                (lambda (frame)
                  (setq make-invisible-called t)
                  (setq invisible-frame frame)))
               ((symbol-function 'frame-live-p)
                (lambda (_frame) t))
               (eglot-signature--debounce-timer nil)
               (eglot-signature--active-buffer nil))
      (eglot-signature--quit)

      ;; Verify make-frame-invisible was called
      (should make-invisible-called)

      ;; Verify correct frame was made invisible
      (should (eq invisible-frame mock-frame)))))

(ert-deftest test-eglot-signature--quit-remove-hooks ()
  "Test remove hooks from active buffer when buffer is live."
  (with-temp-buffer
    (let ((test-buffer (current-buffer)))
      (setq eglot-signature--active-buffer test-buffer)
      ;; Add hooks to buffer
      (add-hook 'window-buffer-change-functions #'eglot-signature--window-change nil t)
      (add-hook 'window-selection-change-functions #'eglot-signature--window-change nil t)

      ;; Verify hooks are added
      (should (memq 'eglot-signature--window-change window-buffer-change-functions))
      (should (memq 'eglot-signature--window-change window-selection-change-functions))

      (cl-letf* ((eglot-signature--debounce-timer nil)
                 (eglot-signature--active-frame nil))
        (eglot-signature--quit)

        ;; Verify hooks are actually removed from buffer
        (should (not (memq 'eglot-signature--window-change window-buffer-change-functions)))
        (should (not (memq 'eglot-signature--window-change window-selection-change-functions)))))))

(ert-deftest test-eglot-signature--quit-no-op-scenarios ()
  "Test no-op when timer/frame/buffer are nil or invalid (combined test cases)."
  (let ((test-cases '(;; (name timer frame buffer frame-live-p buffer-live-p expect-error-p)
                      ("all nil"
                       nil nil nil nil nil nil)
                      ("timer not a timer"
                       'not-a-timer nil nil nil nil nil)
                      ("frame not live"
                       nil 'mock-frame nil nil nil nil)
                      ("buffer not live"
                       nil nil 'dead-buffer nil nil nil))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,name ,timer ,frame ,buffer ,frame-live-p ,_buffer-live-p ,_expect-error-p) test-case)
                   (eglot-signature--debounce-timer timer)
                   (eglot-signature--active-frame frame)
                   (eglot-signature--active-buffer buffer))
        (cl-letf* (((symbol-function 'frame-live-p)
                    (lambda (_frame) frame-live-p))
                   ((symbol-function 'buffer-live-p)
                    (lambda (_buf) nil)))  ; Buffer not live for all test cases
          ;; Should not error
          (should (eq (eglot-signature--quit) nil)))))))

;; Test: eglot-signature--sig-active-p
(ert-deftest test-eglot-signature--sig-active-p ()
  "Test checking if signature help is currently active."
  (let ((test-cases '(;; (name active-sig frame live-p visible-p expect-result)
                      ("active-sig is nil returns nil"
                       nil mock-frame nil nil nil)
                      ("frame is nil returns nil"
                       mock-sig nil nil nil nil)
                      ("frame is live and visible returns t"
                       mock-sig mock-frame t t t)
                      ("frame is not live returns nil"
                       mock-sig mock-frame nil nil nil)
                      ("frame is live but not visible returns nil"
                       mock-sig mock-frame t nil nil)))
        (mock-sig '(:signatures [(:label "test")]))
        (mock-frame 'mock-frame))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,active-sig ,frame ,live-p ,visible-p ,expect-result) test-case))
        (let ((eglot-signature--active-signature active-sig)
              (eglot-signature--active-frame frame))
          (cl-letf* (((symbol-function 'frame-live-p)
                      (lambda (_frame) live-p))
                     ((symbol-function 'frame-visible-p)
                      (lambda (_frame) visible-p)))
            (should (eq (eglot-signature--sig-active-p) expect-result))))))))

;; Test: eglot-signature--valid-win-buf-p
(ert-deftest test-eglot-signature--valid-win-buf-p ()
  "Test validation of window/buffer for signature help (combined test cases)."
  (let ((test-cases '(;; (name active-buf active-frame win-buf win-frame expect-result)
                      ("window buffer equals active-buffer returns t"
                       mock-buffer mock-frame mock-buffer mock-other-frame t)
                      ("window frame equals active-frame returns t"
                       mock-buffer mock-frame mock-other-buffer mock-frame t)
                      ("both conditions true returns t"
                       mock-buffer mock-frame mock-buffer mock-frame t)
                      ("neither condition met returns nil"
                       mock-buffer mock-frame mock-other-buffer mock-other-frame nil)
                      ("active-buffer is nil, frame doesn't match returns nil"
                       nil mock-frame mock-other-buffer mock-other-frame nil)
                      ("active-buffer is nil, but frame matches returns t"
                       nil mock-frame mock-other-buffer mock-frame t)
                      ("active-frame is nil, buffer doesn't match returns nil"
                       mock-buffer nil mock-other-buffer mock-other-frame nil)
                      ("both active-buffer and active-frame are nil returns nil"
                       nil nil mock-other-buffer mock-other-frame nil)))
        (mock-window 'mock-window)
        (mock-buffer 'mock-buffer)
        (mock-frame 'mock-frame)
        (mock-other-buffer 'mock-other-buffer)
        (mock-other-frame 'mock-other-frame))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,active-buf ,active-frame ,win-buf ,win-frame ,expect-result) test-case))
        (let ((eglot-signature--active-buffer active-buf)
              (eglot-signature--active-frame active-frame))
          (cl-letf* (((symbol-function 'selected-window)
                      (lambda () mock-window))
                     ((symbol-function 'window-buffer)
                      (lambda (_win) win-buf))
                     ((symbol-function 'window-frame)
                      (lambda (_win) win-frame)))
            (should (eq (eglot-signature--valid-win-buf-p) expect-result))))))))

;; Test: eglot-signature--valid-place-p
(ert-deftest test-eglot-signature--valid-place-p ()
  "Test checking if current position is valid for signature help (combined test cases)."
  (let ((test-cases '(;; (name valid-win-buf-p active-point current-point expect-result)
                      ("both valid-win-buf-p and point match returns t"
                       t 100 100 t)
                      ("valid-win-buf-p returns nil returns nil"
                       nil 100 100 nil)
                      ("point doesn't match active-point returns nil"
                       t 100 150 nil)
                      ("active-point is nil returns nil"
                       t nil 100 nil)))
        (mock-window 'mock-window)
        (mock-buffer 'mock-buffer)
        (mock-frame 'mock-frame))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,valid-win-buf-p ,active-point ,current-point ,expect-result) test-case))
        (with-temp-buffer
          ;; Insert enough content to support goto-char
          (insert (make-string (or current-point 1) ?x))
          (let ((eglot-signature--active-buffer mock-buffer)
                (eglot-signature--active-frame mock-frame)
                (eglot-signature--active-point active-point))
            (goto-char current-point)
            (cl-letf* (((symbol-function 'selected-window)
                        (lambda () mock-window))
                       ((symbol-function 'window-buffer)
                        (lambda (_win) mock-buffer))
                       ((symbol-function 'window-frame)
                        (lambda (_win) mock-frame))
                       ((symbol-function 'eglot-signature--valid-win-buf-p)
                        (lambda () valid-win-buf-p)))
              (should (eq (eglot-signature--valid-place-p) expect-result)))))))))

;; Test: eglot-signature--window-change
(ert-deftest test-eglot-signature--window-change ()
  "Test window change behavior (combined test cases)."
  (let* ((test-cases '(;; (name sig-active-p valid-win-buf-p is-doc-buf expect-quit)
                       ;; Quit is called when all three conditions met
                       ("quits when active, invalid win/buf, not doc buffer"
                        t nil nil t)
                       ;; No-op when not active
                       ("no-op when signature not active"
                        nil nil nil nil)
                       ("no-op when signature not active, valid window/buffer"
                        nil t nil nil)
                       ("no-op when signature not active, switching to doc buffer"
                        nil nil t nil)
                       ;; No-op when active but valid window/buffer
                       ("no-op when active, valid window/buffer"
                        t t nil nil)
                       ;; No-op when switching to doc buffer
                       ("no-op when active, invalid win/buf, but doc buffer"
                        t nil t nil)))
         (mock-doc-buffer 'mock-doc-buffer)
         (mock-other-buffer 'mock-other-buffer)
         (eglot-signature--doc-buffer mock-doc-buffer))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,sig-active-p ,valid-win-buf-p ,is-doc-buf ,expect-quit) test-case))
        (let ((quit-called nil)
              (mock-window-buffer (if is-doc-buf mock-doc-buffer mock-other-buffer)))
          (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                      (lambda () sig-active-p))
                     ((symbol-function 'eglot-signature--valid-win-buf-p)
                      (lambda () valid-win-buf-p))
                     ((symbol-function 'selected-window)
                      (lambda () 'mock-window))
                     ((symbol-function 'window-buffer)
                      (lambda (_win) mock-window-buffer))
                     ((symbol-function 'eglot-signature--quit)
                      (lambda ()
                        (setq quit-called t))))
            (eglot-signature--window-change nil)

            (should (eq quit-called expect-quit))))))))

;; Test: eglot-signature--doc-empty-p
(ert-deftest test-eglot-signature--doc-empty-p ()
  "Test checking if doc content is empty (combined test cases)."
  (let ((test-cases '(;; (name doc expect-result)
                      ("nil doc returns t"
                       nil t)
                      ("empty string doc returns t"
                       "" t)
                      ("non-empty string doc returns nil"
                       "This is documentation" nil)
                      ("plist with nil :value returns t"
                       (:value nil) t)
                      ("plist with empty string :value returns t"
                       (:value "") t)
                      ("plist with non-empty :value returns nil"
                       (:value "This is documentation") nil))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,doc ,expect-result) test-case))
        (should (eq (eglot-signature--doc-empty-p doc) expect-result))))))

;; Test: eglot-signature--doc-highlight-label
(ert-deftest test-eglot-signature--doc-highlight-label ()
  "Test highlighting active parameter in signature label (combined test cases)."
  (let ((test-cases '(;; (name label active-param expect-text)
                      ("label is not string returns empty string"
                       123 nil "")
                      ("label is nil returns empty string"
                       nil nil "")
                      ("active-param is nil returns original label"
                       "foo(x, y)" nil "foo(x, y)")
                      ("active-param without :label returns original label"
                       "foo(x, y)" (:value "doc") "foo(x, y)")
                      ("empty label returns empty string"
                       "" nil ""))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,label ,active-param ,expect-text) test-case))
        (should (string= (eglot-signature--doc-highlight-label label active-param) expect-text))))))

(ert-deftest test-eglot-signature--doc-highlight-label-vector-offset ()
  "Test vector offset [start, end] highlights correct range."
  (let* ((label "foo(x, y, z)")
         (active-param '(:label [4 9]))  ; "x, y" positions
         (result (eglot-signature--doc-highlight-label label active-param)))
    ;; Result should be a string with text property applied
    (should (stringp result))
    (should (string= result "foo(x, y, z)"))
    ;; Check that the face property is applied to the correct range
    (let ((props (get-text-property 6 'face result)))  ; Position 6 is 'x' (1+5)
      (should (eq props 'eglot-signature-active-parameter)))))

(ert-deftest test-eglot-signature--doc-highlight-label-string-label ()
  "Test string label finds and highlights first occurrence."
  (let* ((label "foo(x, y, z)")
         (active-param '(:label "x"))  ; Find and highlight "x"
         (result (eglot-signature--doc-highlight-label label active-param)))
    ;; Result should be a string with text property applied
    (should (stringp result))
    (should (string= result "foo(x, y, z)"))
    ;; Check that the face property is applied to the found position
    (let ((found-pos (cl-loop for i from 1 to (length result)
                              when (eq (get-text-property i 'face result)
                                       'eglot-signature-active-parameter)
                              return i)))
      (should found-pos))))

(ert-deftest test-eglot-signature--doc-highlight-label-string-not-found ()
  "Test string label not found returns original label."
  (let* ((label "foo(x, y, z)")
         (active-param '(:label "notfound"))  ; String not in label
         (result (eglot-signature--doc-highlight-label label active-param)))
    ;; Result should be the original label without any highlighting
    (should (stringp result))
    (should (string= result "foo(x, y, z)"))
    ;; Check that no face property is applied anywhere
    (let ((found-pos (cl-loop for i from 1 to (length result)
                              when (eq (get-text-property i 'face result)
                                       'eglot-signature-active-parameter)
                              return i)))
      (should (not found-pos)))))

;; Test: eglot-signature--prepare-buffer
(ert-deftest test-eglot-signature--prepare-buffer-basic-single-signature ()
  "Test basic signature with label only (single signature)."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)")]))
         (eglot-signature--doc-buffer nil)
         (eglot-signature-show-param-doc nil)
         (eglot-signature-show-doc nil))
    (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                (lambda (label _active-param)
                  (propertize label 'face 'eglot-signature-doc-default)))
               ((symbol-function 'eglot--format-markup)
                (lambda (_doc) "formatted doc")))
      (let ((result-buffer (eglot-signature--prepare-buffer mock-sig-help)))
        ;; Should return a buffer
        (should (bufferp result-buffer))
        ;; Buffer should be cached
        (should (eq result-buffer eglot-signature--doc-buffer))
        ;; Check buffer content contains the label
        (with-current-buffer result-buffer
          (should (string= (buffer-string) "foo(x, y)")))))))

(ert-deftest test-eglot-signature--prepare-buffer-multiple-signatures-with-counter ()
  "Test multiple signatures with counter."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)") (:label "foo(x, y, z)")]))
         (eglot-signature--doc-buffer nil)
         (eglot-signature-show-param-doc nil)
         (eglot-signature-show-doc nil))
    (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                (lambda (label _active-param)
                  (propertize label 'face 'eglot-signature-doc-default)))
               ((symbol-function 'eglot--format-markup)
                (lambda (_doc) "formatted doc")))
      (let ((result-buffer (eglot-signature--prepare-buffer mock-sig-help)))
        ;; Should return a buffer
        (should (bufferp result-buffer))
        ;; Check buffer content starts with counter [1/2]
        (with-current-buffer result-buffer
          (should (string-prefix-p "[1/2] " (buffer-string)))
          (should (string-suffix-p "foo(x, y)" (buffer-string))))))))

(ert-deftest test-eglot-signature--prepare-buffer-reuse-existing-buffer ()
  "Test buffer reuse when already exists."
  (let* ((mock-sig-help '(:signatures [(:label "bar(a, b)")]))
         (existing-buffer (get-buffer-create " *eglot-signature-doc*"))
         (eglot-signature--doc-buffer existing-buffer)
         (eglot-signature-show-param-doc nil)
         (eglot-signature-show-doc nil))
    (unwind-protect
        (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                    (lambda (label _active-param)
                      (propertize label 'face 'eglot-signature-doc-default)))
                   ((symbol-function 'eglot--format-markup)
                    (lambda (_doc) "formatted doc")))
          (let ((result-buffer (eglot-signature--prepare-buffer mock-sig-help)))
            ;; Should return the existing buffer (reused)
            (should (eq result-buffer existing-buffer))
            ;; Buffer content should be updated
            (with-current-buffer result-buffer
              (should (string= (buffer-string) "bar(a, b)")))))
      ;; Cleanup
      (kill-buffer existing-buffer))))

(ert-deftest test-eglot-signature--prepare-buffer-with-parameter-documentation ()
  "Test with parameter documentation."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)"
                                          :parameters [(:documentation "Param x doc")]
                                          :activeParameter 0)]))
         (eglot-signature--doc-buffer nil)
         (eglot-signature-show-param-doc t)
         (eglot-signature-show-doc nil))
    (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                (lambda (label _active-param)
                  (propertize label 'face 'eglot-signature-doc-default)))
               ((symbol-function 'eglot--format-markup)
                (lambda (_doc) "formatted param doc")))
      (let ((result-buffer (eglot-signature--prepare-buffer mock-sig-help)))
        (should (bufferp result-buffer))
        ;; Check buffer contains both label and param documentation
        (with-current-buffer result-buffer
          (should (string-match "foo(x, y)" (buffer-string)))
          (should (string-match "formatted param doc" (buffer-string))))))))

(ert-deftest test-eglot-signature--prepare-buffer-with-signature-documentation ()
  "Test with signature documentation."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)"
                                          :documentation "Signature doc")]))
         (eglot-signature--doc-buffer nil)
         (eglot-signature-show-param-doc nil)
         (eglot-signature-show-doc t))
    (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                (lambda (label _active-param)
                  (propertize label 'face 'eglot-signature-doc-default)))
               ((symbol-function 'eglot--format-markup)
                (lambda (_doc) "formatted sig doc")))
      (let ((result-buffer (eglot-signature--prepare-buffer mock-sig-help)))
        (should (bufferp result-buffer))
        ;; Check buffer contains both label and signature documentation
        (with-current-buffer result-buffer
          (should (string-match "foo(x, y)" (buffer-string)))
          (should (string-match "formatted sig doc" (buffer-string))))))))

(ert-deftest test-eglot-signature--prepare-buffer-with-both-documentation ()
  "Test with both parameter and signature documentation."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)"
                                          :parameters [(:documentation "Param x doc")]
                                          :documentation "Signature doc"
                                          :activeParameter 0)]))
         (eglot-signature--doc-buffer nil)
         (eglot-signature-show-param-doc t)
         (eglot-signature-show-doc t))
    (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                (lambda (label _active-param)
                  (propertize label 'face 'eglot-signature-doc-default)))
               ((symbol-function 'eglot--format-markup)
                (lambda (doc)
                  (if (string= doc "Param x doc") "formatted param doc"
                    "formatted sig doc"))))
      (let ((result-buffer (eglot-signature--prepare-buffer mock-sig-help)))
        (should (bufferp result-buffer))
        ;; Check buffer contains label, param doc, and sig doc
        (with-current-buffer result-buffer
          (should (string-match "foo(x, y)" (buffer-string)))
          (should (string-match "formatted param doc" (buffer-string)))
          (should (string-match "formatted sig doc" (buffer-string))))))))

(ert-deftest test-eglot-signature--prepare-buffer-edge-cases ()
  "Test nil signatures list and active signature index out of bounds (combined test cases)."
  (let ((test-cases '(;; (name sig-help expect-empty-p)
                      ("nil signatures list"
                       (:signatures nil)
                       t)
                      ("active signature index out of bounds"
                       (:signatures [(:label "foo(x, y)")] :activeSignature 5)
                       t))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,sig-help ,expect-empty-p) test-case))
        (let ((eglot-signature--doc-buffer nil)
              (eglot-signature-show-param-doc nil)
              (eglot-signature-show-doc nil))
          (cl-letf* (((symbol-function 'eglot-signature--doc-highlight-label)
                      (lambda (label _active-param)
                        (propertize label 'face 'eglot-signature-doc-default)))
                     ((symbol-function 'eglot--format-markup)
                      (lambda (_doc) "formatted doc")))
            (let ((result-buffer (eglot-signature--prepare-buffer sig-help)))
              (should (bufferp result-buffer))
              (with-current-buffer result-buffer
                (if expect-empty-p
                    (should (string= (buffer-string) ""))
                  (should (not (string= (buffer-string) ""))))))))))))

;; Test: eglot-signature--buffer-frame-size
(ert-deftest test-eglot-signature--buffer-frame-size-dead-buffer ()
  "Test returns nil when buffer is not live."
  (let ((dead-buffer "non-existent-buffer")
        (w-edges '(0 0 800 600)))
    (cl-letf* (((symbol-function 'buffer-live-p)
                (lambda (_buf) nil)))
      (should (eq (eglot-signature--buffer-frame-size dead-buffer w-edges) nil)))))

(ert-deftest test-eglot-signature--buffer-frame-size-basic-calculation ()
  "Test basic frame size calculation with simple content."
  (let ((mock-size '(150 . 40))
        (w-edges '(0 0 800 600)))
    (with-temp-buffer
      (insert "test content")
      (cl-letf* (((symbol-function 'default-font-height)
                  (lambda () 10))
                 ((symbol-function 'window-text-pixel-size)
                  (lambda (&rest _args) mock-size))
                 (eglot-signature--doc-separator-lines nil))
        (let ((result (eglot-signature--buffer-frame-size (current-buffer) w-edges)))
          ;; Should return the size from window-text-pixel-size
          (should (consp result))
          (should (equal (car result) 150))
          (should (equal (cdr result) 40))
          ;; Verify result is cached
          (should (equal eglot-signature--cached-frame-size mock-size)))))))

(ert-deftest test-eglot-signature--buffer-frame-size-max-height-constraint ()
  "Test height is constrained by eglot-signature-max-height."
  (let ((mock-size '(150 . 400))
        (w-edges '(0 0 800 600))
        (eglot-signature-max-height 10))  ; Max 10 lines = 100 pixels
    (with-temp-buffer
      (insert "test content")
      (cl-letf* (((symbol-function 'default-font-height)
                  (lambda () 10))
                 ((symbol-function 'display-graphic-p)
                  (lambda () t))
                 ((symbol-function 'window-text-pixel-size)
                  (lambda (&rest args)
                    ;; The 5th arg is max-height constraint
                    (let ((max-height (nth 4 args)))
                      ;; Return height limited by the constraint
                      (cons 150 (min 400 max-height)))))
                 (eglot-signature--doc-separator-lines nil))
        (let ((result (eglot-signature--buffer-frame-size (current-buffer) w-edges)))
          ;; Height should be constrained by max-height
          (should (consp result))
          (should (equal (car result) 150))
          (should (<= (cdr result) 100)))))))  ; 10 lines * 10 pixels

;; Test: eglot-signature--frame-geometry
(ert-deftest test-eglot-signature--frame-geometry-basic-above-cursor ()
  "Test basic geometry calculation with sufficient space above cursor."
  (let ((buf-size '(200 . 50))    ; width=200, height=50
        (cursor-xy '(100 . 300))   ; x=100, y=300
        (frame-width 1000)         ; Frame width for overflow check
        (line-height 20))          ; Line height for position calculation
    (cl-letf* (((symbol-function 'frame-pixel-width)
                (lambda () frame-width))
               ((symbol-function 'default-line-height)
                (lambda () line-height))
               ((symbol-function 'display-graphic-p)
                (lambda () t)))
      (let ((result (eglot-signature--frame-geometry buf-size cursor-xy)))
        ;; Verify result is a list of 4 elements
        (should (listp result))
        (should (= (length result) 4))
        ;; Verify x position (should be cursor-x since no overflow)
        (should (= (nth 0 result) 100))
        ;; Verify y position (should be above cursor)
        ;; cursor-above-y = 300 - 50 - 4 = 246
        (should (= (nth 1 result) 246))
        ;; Verify width-pixel and height-pixel
        (should (= (nth 2 result) 200))
        (should (= (nth 3 result) 50))))))

(ert-deftest test-eglot-signature--frame-geometry-insufficient-space-above ()
  "Test frame placed below cursor when insufficient space above."
  (let ((buf-size '(200 . 50))
        (cursor-xy '(100 . 60))    ; Close to top
        (frame-width 1000)
        (line-height 20))
    (cl-letf* (((symbol-function 'frame-pixel-width)
                (lambda () frame-width))
               ((symbol-function 'default-line-height)
                (lambda () line-height))
               ((symbol-function 'display-graphic-p)
                (lambda () t)))
      (let ((result (eglot-signature--frame-geometry buf-size cursor-xy)))
        (should (listp result))
        (should (= (length result) 4))
        (should (= (nth 0 result) 100))
        ;; cursor-above-y = 60 - 50 - 4 = 6 <= 20, so place below
        (should (= (nth 1 result) 84))
        (should (= (nth 2 result) 200))
        (should (= (nth 3 result) 50))))))

(ert-deftest test-eglot-signature--frame-geometry-horizontal-adjustment ()
  "Test horizontal adjustment to prevent right overflow (combined test cases)."
  (let ((test-cases '(;; (name buf-size cursor-xy frame-width line-height expect-x expect-y)
                      ("no overflow, use cursor-x"
                       (200 . 50) (100 . 300) 1000 20 100 246)
                      ("right overflow, adjust to frame edge"
                       (200 . 50) (900 . 300) 1000 20 796 246)
                      ("exactly at frame edge, use cursor-x"
                       (200 . 50) (796 . 300) 1000 20 796 246)
                      ("just before overflow, use cursor-x"
                       (200 . 50) (795 . 300) 1000 20 795 246))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,buf-size ,cursor-xy ,frame-width ,line-height ,expect-x ,expect-y) test-case))
        (cl-letf* (((symbol-function 'frame-pixel-width)
                    (lambda () frame-width))
                   ((symbol-function 'default-line-height)
                    (lambda () line-height))
                   ((symbol-function 'display-graphic-p)
                    (lambda () t)))
          (let ((result (eglot-signature--frame-geometry buf-size cursor-xy)))
            (should (listp result))
            (should (= (length result) 4))
            (should (= (nth 0 result) expect-x))
            (should (= (nth 1 result) expect-y))
            (should (= (nth 2 result) (car buf-size)))
            (should (= (nth 3 result) (cdr buf-size)))))))))

(ert-deftest test-eglot-signature--frame-geometry-edge-case-line-height ()
  "Test edge case: cursor-above-y equals line-height."
  (let ((buf-size '(200 . 50))
        (cursor-xy '(100 . 74))    ; cursor-above-y = 74 - 50 - 4 = 20 = line-height
        (frame-width 1000)
        (line-height 20))
    (cl-letf* (((symbol-function 'frame-pixel-width)
                (lambda () frame-width))
               ((symbol-function 'default-line-height)
                (lambda () line-height))
               ((symbol-function 'display-graphic-p)
                (lambda () t)))
      (let ((result (eglot-signature--frame-geometry buf-size cursor-xy)))
        (should (listp result))
        (should (= (length result) 4))
        (should (= (nth 0 result) 100))
        ;; cursor-above-y is NOT > line-height, so place below
        (should (= (nth 1 result) 98))
        (should (= (nth 2 result) 200))
        (should (= (nth 3 result) 50))))))

(ert-deftest test-eglot-signature--frame-geometry-edge-case-terminal ()
  "Test edge case: terminal display (non-graphic) has zero padding."
  (let ((buf-size '(200 . 50))
        (cursor-xy '(100 . 300))
        (frame-width 1000)
        (line-height 20))
    (cl-letf* (((symbol-function 'frame-pixel-width)
                (lambda () frame-width))
               ((symbol-function 'default-line-height)
                (lambda () line-height))
               ((symbol-function 'display-graphic-p)
                (lambda () nil)))  ; Terminal mode
      (let ((result (eglot-signature--frame-geometry buf-size cursor-xy)))
        (should (listp result))
        (should (= (length result) 4))
        (should (= (nth 0 result) 100))
        ;; cursor-above-y = 300 - 50 - 0 = 250 (no padding in terminal)
        (should (= (nth 1 result) 250))
        (should (= (nth 2 result) 200))
        (should (= (nth 3 result) 50))))))

;; Test: eglot-signature--render-sig-frame-at-point
(ert-deftest test-eglot-signature--render-sig-frame-at-point-new-live-buffer ()
  "Test render with new live signature buffer."
  (let ((mock-sig-buffer (get-buffer-create " *test-sig-buffer*"))
        (mock-frame 'mock-new-frame)
        (mock-root-window 'mock-root-window)
        (w-edges '(100 100 800 600))
        (update-called nil)
        (update-args nil)
        (eglot-signature--active-frame nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf* (((symbol-function 'selected-window)
                      (lambda () 'mock-parent-window))
                     ((symbol-function 'window-inside-pixel-edges)
                      (lambda (_win) w-edges))
                     ((symbol-function 'posn-at-point)
                      (lambda (_pt _win)
                        (list (point) '(0 . 0) '(10 . 20) 0)))
                     ((symbol-function 'frame-live-p)
                      (lambda (_frame) nil))
                     ((symbol-function 'window-live-p)
                      (lambda (_win) t))
                     ((symbol-function 'eglot-signature--make-frame)
                      (lambda () mock-frame))
                     ((symbol-function 'frame-root-window)
                      (lambda (_frame) mock-root-window))
                     ((symbol-function 'window-buffer)
                      (lambda (_win) nil))
                     ((symbol-function 'set-window-buffer)
                      (lambda (_win _buf) nil))
                     ((symbol-function 'eglot-signature--buffer-frame-size)
                      (lambda (_buf _edges) '(200 . 50)))
                     ((symbol-function 'eglot-signature--frame-geometry)
                      (lambda (_buf-size _cursor-xy)
                        '(110 120 200 50)))
                     ((symbol-function 'eglot-signature--update-frame-size-and-position)
                      (lambda (frame x y w h)
                        (setq update-called t)
                        (setq update-args (list frame x y w h))))
                     ((symbol-function 'frame-visible-p)
                      (lambda (_frame) nil))
                     ((symbol-function 'make-frame-visible)
                      (lambda (_frame) nil)))
            (eglot-signature--render-sig-frame-at-point mock-sig-buffer)
            (should update-called)
            (should (eq (nth 0 update-args) mock-frame))
            (should (eq (nth 1 update-args) 110))
            (should (eq (nth 2 update-args) 120))
            (should (eq (nth 3 update-args) 200))
            (should (eq (nth 4 update-args) 50))))
      (kill-buffer mock-sig-buffer))))

(ert-deftest test-eglot-signature--render-sig-frame-at-point-reuse-live-frame ()
  "Test render with existing live frame (reuses active frame)."
  (let ((mock-sig-buffer (get-buffer-create " *test-sig-buffer*"))
        (mock-active-frame 'mock-active-frame)
        (mock-root-window 'mock-root-window)
        (w-edges '(100 100 800 600))
        (update-called nil)
        (update-args nil))
    (setq eglot-signature--active-frame mock-active-frame)
    (unwind-protect
        (with-temp-buffer
          (cl-letf* (((symbol-function 'selected-window)
                      (lambda () 'mock-parent-window))
                     ((symbol-function 'window-inside-pixel-edges)
                      (lambda (_win) w-edges))
                     ((symbol-function 'posn-at-point)
                      (lambda (_pt _win)
                        (list (point) '(0 . 0) '(10 . 20) 0)))
                     ((symbol-function 'frame-live-p)
                      (lambda (_frame) t))
                     ((symbol-function 'window-live-p)
                      (lambda (_win) t))
                     ((symbol-function 'frame-root-window)
                      (lambda (_frame) mock-root-window))
                     ((symbol-function 'window-buffer)
                      (lambda (_win) nil))
                     ((symbol-function 'set-window-buffer)
                      (lambda (_win _buf) nil))
                     ((symbol-function 'eglot-signature--buffer-frame-size)
                      (lambda (_buf _edges) '(200 . 50)))
                     ((symbol-function 'eglot-signature--frame-geometry)
                      (lambda (_buf-size _cursor-xy)
                        '(110 120 200 50)))
                     ((symbol-function 'eglot-signature--update-frame-size-and-position)
                      (lambda (frame x y w h)
                        (setq update-called t)
                        (setq update-args (list frame x y w h))))
                     ((symbol-function 'frame-visible-p)
                      (lambda (_frame) nil))
                     ((symbol-function 'make-frame-visible)
                      (lambda (_frame) nil)))
            (eglot-signature--render-sig-frame-at-point mock-sig-buffer)
            (should update-called)
            ;; Should reuse the existing active frame, not create a new one
            (should (eq (nth 0 update-args) mock-active-frame))
            (should (eq (nth 1 update-args) 110))
            (should (eq (nth 2 update-args) 120))
            (should (eq (nth 3 update-args) 200))
            (should (eq (nth 4 update-args) 50))))
      (kill-buffer mock-sig-buffer))))

(ert-deftest test-eglot-signature--render-sig-frame-at-point-no-sig-buf-cached ()
  "Test render without sig-buf uses cached size."
  (let ((mock-active-frame 'mock-active-frame)
        (mock-root-window 'mock-root-window)
        (w-edges '(100 100 800 600))
        (set-position-called nil)
        (set-position-args nil))
    (setq eglot-signature--active-frame mock-active-frame)
    (setq eglot-signature--cached-frame-size '(200 . 50))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'selected-window)
                  (lambda () 'mock-parent-window))
                 ((symbol-function 'window-inside-pixel-edges)
                  (lambda (_win) w-edges))
                 ((symbol-function 'posn-at-point)
                  (lambda (_pt _win)
                    (list (point) '(0 . 0) '(10 . 20) 0)))
                 ((symbol-function 'frame-live-p)
                  (lambda (_frame) t))
                 ((symbol-function 'window-live-p)
                  (lambda (_win) t))
                 ((symbol-function 'frame-root-window)
                  (lambda (_frame) mock-root-window))
                 ((symbol-function 'eglot-signature--frame-geometry)
                  (lambda (_buf-size _cursor-xy)
                    '(110 120 200 50)))
                 ((symbol-function 'set-frame-position)
                  (lambda (frame x y)
                    (setq set-position-called t)
                    (setq set-position-args (list frame x y))))
                 ((symbol-function 'frame-visible-p)
                  (lambda (_frame) nil))
                 ((symbol-function 'make-frame-visible)
                  (lambda (_frame) nil)))
        ;; Call without sig-buf (or with nil)
        (eglot-signature--render-sig-frame-at-point nil)
        (should set-position-called)
        (should (eq (nth 0 set-position-args) mock-active-frame))
        (should (eq (nth 1 set-position-args) 110))  ; x
        (should (eq (nth 2 set-position-args) 120)))))) ; y

(ert-deftest test-eglot-signature--render-sig-frame-at-point-make-visible ()
  "Test frame is made visible when not visible."
  (let ((mock-sig-buffer (get-buffer-create " *test-sig-buffer*"))
        (mock-active-frame 'mock-active-frame)
        (mock-root-window 'mock-root-window)
        (w-edges '(100 100 800 600))
        (make-visible-called nil))
    (setq eglot-signature--active-frame mock-active-frame)
    (unwind-protect
        (with-temp-buffer
          (cl-letf* (((symbol-function 'selected-window)
                      (lambda () 'mock-parent-window))
                     ((symbol-function 'window-inside-pixel-edges)
                      (lambda (_win) w-edges))
                     ((symbol-function 'posn-at-point)
                      (lambda (_pt _win)
                        (list (point) '(0 . 0) '(10 . 20) 0)))
                     ((symbol-function 'frame-live-p)
                      (lambda (_frame) t))
                     ((symbol-function 'window-live-p)
                      (lambda (_win) t))
                     ((symbol-function 'frame-root-window)
                      (lambda (_frame) mock-root-window))
                     ((symbol-function 'window-buffer)
                      (lambda (_win) nil))
                     ((symbol-function 'set-window-buffer)
                      (lambda (_win _buf) nil))
                     ((symbol-function 'eglot-signature--buffer-frame-size)
                      (lambda (_buf _edges) '(200 . 50)))
                     ((symbol-function 'eglot-signature--frame-geometry)
                      (lambda (_buf-size _cursor-xy)
                        '(110 120 200 50)))
                     ((symbol-function 'eglot-signature--update-frame-size-and-position)
                      (lambda (_frame _x _y _w _h) nil))
                     ((symbol-function 'frame-visible-p)
                      (lambda (_frame) nil))  ; Frame is not visible
                     ((symbol-function 'make-frame-visible)
                      (lambda (_frame)
                        (setq make-visible-called t))))
            (eglot-signature--render-sig-frame-at-point mock-sig-buffer)
            (should make-visible-called)))
      (kill-buffer mock-sig-buffer))))

(ert-deftest test-eglot-signature--render-sig-frame-at-point-set-window-buffer ()
  "Test window buffer is set when sig buffer changed."
  (let ((mock-sig-buffer (get-buffer-create " *test-sig-buffer*"))
        (mock-other-buffer (get-buffer-create " *test-other-buffer*"))
        (mock-active-frame 'mock-active-frame)
        (mock-root-window 'mock-root-window)
        (w-edges '(100 100 800 600))
        (set-window-buffer-called nil)
        (set-window-buffer-args nil))
    (setq eglot-signature--active-frame mock-active-frame)
    (unwind-protect
        (with-temp-buffer
          (cl-letf* (((symbol-function 'selected-window)
                      (lambda () 'mock-parent-window))
                     ((symbol-function 'window-inside-pixel-edges)
                      (lambda (_win) w-edges))
                     ((symbol-function 'posn-at-point)
                      (lambda (_pt _win)
                        (list (point) '(0 . 0) '(10 . 20) 0)))
                     ((symbol-function 'frame-live-p)
                      (lambda (_frame) t))
                     ((symbol-function 'window-live-p)
                      (lambda (_win) t))
                     ((symbol-function 'frame-root-window)
                      (lambda (_frame) mock-root-window))
                     ((symbol-function 'window-buffer)
                      (lambda (_win) mock-other-buffer))  ; Window has different buffer
                     ((symbol-function 'set-window-buffer)
                      (lambda (win buf)
                        (setq set-window-buffer-called t)
                        (setq set-window-buffer-args (list win buf))))
                     ((symbol-function 'eglot-signature--buffer-frame-size)
                      (lambda (_buf _edges) '(200 . 50)))
                     ((symbol-function 'eglot-signature--frame-geometry)
                      (lambda (_buf-size _cursor-xy)
                        '(110 120 200 50)))
                     ((symbol-function 'eglot-signature--update-frame-size-and-position)
                      (lambda (_frame _x _y _w _h) nil))
                     ((symbol-function 'frame-visible-p)
                      (lambda (_frame) t))
                     ((symbol-function 'make-frame-visible)
                      (lambda (_frame) nil)))
            (eglot-signature--render-sig-frame-at-point mock-sig-buffer)
            ;; set-window-buffer should be called when window buffer is different
            (should set-window-buffer-called)
            (should (eq (nth 0 set-window-buffer-args) mock-root-window))
            (should (eq (nth 1 set-window-buffer-args) mock-sig-buffer))))
      ;; Cleanup
      (kill-buffer mock-sig-buffer)
      (kill-buffer mock-other-buffer))))

;; Test: eglot-signature--on-self-insert
(ert-deftest test-eglot-signature--on-self-insert-no-op-provider-nil ()
  "Test no-op when eglot-signature--provider is nil."
  (let ((eglot-signature--provider nil)
        (render-called nil)
        (debounce-request-called nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda ()
                    (setq render-called t)))
                 ((symbol-function 'eglot-signature--debounce-request)
                  (lambda (&rest _args)
                    (setq debounce-request-called t))))
        ;; Set last-command-event to simulate character insertion
        (setq last-command-event ?x)
        (eglot-signature--on-self-insert)

        ;; Should not call render or debounce-request when provider is nil
        (should (not render-called))
        (should (not debounce-request-called))))))

(ert-deftest test-eglot-signature--on-self-insert-render-frame-when-active ()
  "Test render frame called when signature is active."
  (let ((eglot-signature--provider '(:triggerCharacters ["("]))
        (render-called nil)
        (debounce-request-called nil)
        (debounce-request-args nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                  (lambda () t))
                 ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda ()
                    (setq render-called t)))
                 ((symbol-function 'eglot-signature--debounce-request)
                  (lambda (trigger-kind &optional trigger-char)
                    (setq debounce-request-called t)
                    (setq debounce-request-args (list trigger-kind trigger-char)))))
        ;; Insert a non-trigger character
        (setq last-command-event ?x)
        (eglot-signature--on-self-insert)

        ;; Should call render since signature is active
        (should render-called)

        ;; Should call debounce-request with :content-change since char is not a trigger
        (should debounce-request-called)
        (should (eq (car debounce-request-args) :content-change))
        (should (not (cadr debounce-request-args)))))))

(ert-deftest test-eglot-signature--on-self-insert-trigger-char-auto-show ()
  "Test trigger character with auto-show enabled triggers :trigger-character request."
  (let ((eglot-signature--provider '(:triggerCharacters ["("]))
        (eglot-signature-auto-show t)
        (render-called nil)
        (debounce-request-called nil)
        (debounce-request-args nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                  (lambda () nil))
                 ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda ()
                    (setq render-called t)))
                 ((symbol-function 'eglot-signature--debounce-request)
                  (lambda (trigger-kind &optional trigger-char)
                    (setq debounce-request-called t)
                    (setq debounce-request-args (list trigger-kind trigger-char)))))
        ;; Insert trigger character
        (setq last-command-event ?\()
        (eglot-signature--on-self-insert)

        ;; Should not call render since signature is not active
        (should (not render-called))

        ;; Should call debounce-request with :trigger-character and char as string
        (should debounce-request-called)
        (should (eq (car debounce-request-args) :trigger-character))
        (should (string= (cadr debounce-request-args) "("))))))

(ert-deftest test-eglot-signature--on-self-insert-active-trigger ()
  "Test active signature + trigger character."
  (let ((eglot-signature--provider '(:triggerCharacters ["("]))
        (eglot-signature-auto-show nil))
    (with-temp-buffer
      (let ((render-called nil)
            (debounce-request-called nil)
            (debounce-request-args nil))
        (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                    (lambda () t))
                   ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                    (lambda ()
                      (setq render-called t)))
                   ((symbol-function 'eglot-signature--debounce-request)
                    (lambda (trigger-kind &optional trigger-char)
                      (setq debounce-request-called t)
                      (setq debounce-request-args (list trigger-kind trigger-char)))))
          (setq last-command-event ?\()
          (eglot-signature--on-self-insert)
          (should render-called)
          (should debounce-request-called)
          (should (eq (car debounce-request-args) :trigger-character))
          (should (string= (cadr debounce-request-args) "(")))))))

(ert-deftest test-eglot-signature--on-self-insert-active-retrigger ()
  "Test active signature + retrigger character."
  (let ((eglot-signature--provider '(:triggerCharacters ["("] :retriggerCharacters [","]))
        (eglot-signature-auto-show nil))
    (with-temp-buffer
      (let ((render-called nil)
            (debounce-request-called nil)
            (debounce-request-args nil))
        (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                    (lambda () t))
                   ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                    (lambda ()
                      (setq render-called t)))
                   ((symbol-function 'eglot-signature--debounce-request)
                    (lambda (trigger-kind &optional trigger-char)
                      (setq debounce-request-called t)
                      (setq debounce-request-args (list trigger-kind trigger-char)))))
          (setq last-command-event ?\,)
          (eglot-signature--on-self-insert)
          (should render-called)
          (should debounce-request-called)
          (should (eq (car debounce-request-args) :trigger-character))
          (should (string= (cadr debounce-request-args) ",")))))))

(ert-deftest test-eglot-signature--on-self-insert-active-other-char ()
  "Test active signature + other character triggers :content-change."
  (let ((eglot-signature--provider '(:triggerCharacters ["("] :retriggerCharacters [","]))
        (eglot-signature-auto-show nil))
    (with-temp-buffer
      (let ((render-called nil)
            (debounce-request-called nil)
            (debounce-request-args nil))
        (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                    (lambda () t))
                   ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                    (lambda ()
                      (setq render-called t)))
                   ((symbol-function 'eglot-signature--debounce-request)
                    (lambda (trigger-kind &optional trigger-char)
                      (setq debounce-request-called t)
                      (setq debounce-request-args (list trigger-kind trigger-char)))))
          (setq last-command-event ?x)
          (eglot-signature--on-self-insert)
          (should render-called)
          (should debounce-request-called)
          (should (eq (car debounce-request-args) :content-change))
          (should (not (cadr debounce-request-args))))))))

(ert-deftest test-eglot-signature--on-self-insert-not-active-nothing ()
  "Test not active + non-trigger character does nothing."
  (let ((eglot-signature--provider '(:triggerCharacters ["("]))
        (eglot-signature-auto-show nil))
    (with-temp-buffer
      (let ((render-called nil)
            (debounce-request-called nil))
        (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                    (lambda () nil))
                   ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                    (lambda ()
                      (setq render-called t)))
                   ((symbol-function 'eglot-signature--debounce-request)
                    (lambda (&rest _args)
                      (setq debounce-request-called t))))
          (setq last-command-event ?x)
          (eglot-signature--on-self-insert)
          (should (not render-called))
          (should (not debounce-request-called)))))))

(ert-deftest test-eglot-signature--on-self-insert-not-active-trigger-no-auto-show ()
  "Test not active + trigger char + auto-show disabled does nothing."
  (let ((eglot-signature--provider '(:triggerCharacters ["("]))
        (eglot-signature-auto-show nil))
    (with-temp-buffer
      (let ((render-called nil)
            (debounce-request-called nil))
        (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                    (lambda () nil))
                   ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                    (lambda ()
                      (setq render-called t)))
                   ((symbol-function 'eglot-signature--debounce-request)
                    (lambda (&rest _args)
                      (setq debounce-request-called t))))
          (setq last-command-event ?\()
          (eglot-signature--on-self-insert)
          (should (not render-called))
          (should (not debounce-request-called)))))))

;; Test: eglot-signature--on-post-command
(ert-deftest test-eglot-signature--on-post-command-no-op-active-signature-nil ()
  "Test no-op when active-signature is nil."
  (let ((eglot-signature--active-signature nil)
        (render-called nil)
        (debounce-request-called nil))
    (with-temp-buffer
      (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                  (lambda () t))
                 ((symbol-function 'eglot-signature--valid-win-buf-p)
                  (lambda () t))
                 ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda ()
                    (setq render-called t)))
                 ((symbol-function 'eglot-signature--debounce-request)
                  (lambda (&rest _args)
                    (setq debounce-request-called t))))
        (eglot-signature--on-post-command)

        ;; Should not call render or debounce-request when active-signature is nil
        (should (not render-called))
        (should (not debounce-request-called))))))

(ert-deftest test-eglot-signature--on-post-command-no-op-point-unchanged ()
  "Test no-op when point equals active-point."
  (let ((eglot-signature--active-signature '(:signatures [(:label "test")]))
        (eglot-signature--active-point 100)
        (render-called nil)
        (debounce-request-called nil))
    (with-temp-buffer
      ;; Set point to equal active-point
      (insert (make-string 100 ?x))
      (goto-char 100)
      (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                  (lambda () t))
                 ((symbol-function 'eglot-signature--valid-win-buf-p)
                  (lambda () t))
                 ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                  (lambda ()
                    (setq render-called t)))
                 ((symbol-function 'eglot-signature--debounce-request)
                  (lambda (&rest _args)
                    (setq debounce-request-called t))))
        (eglot-signature--on-post-command)

        ;; Should not call render or debounce-request when point unchanged
        (should (not render-called))
        (should (not debounce-request-called))))))

;; Test: eglot-signature--on-post-command no-op scenarios combined
(ert-deftest test-eglot-signature--on-post-command-no-op-combined ()
  "Test no-op scenarios (combined test cases)."
  (let ((test-cases
         '(;; (name active-sig point-changed sig-active-p valid-win-buf-p executing-kbd this-cmd expect-action)
           ("sig-active-p returns nil" t t nil nil nil nil nil)
           ("valid-win-buf-p returns nil" t t t nil nil nil nil)
           ("executing-kbd-macro is true" t t t t t nil nil)
           ("this-command is self-insert-command" t t t t nil self-insert-command nil)
           ("this-command is corfu-insert" t t t t nil corfu-insert nil)
           ("this-command is company--insert-candidate" t t t t nil company--insert-candidate nil)
           ("all conditions met" t t t t nil forward-char t))))
    (dolist (test-case test-cases)
      (pcase-let*
          ((`(,_name ,active-sig ,point-changed ,sig-active-p ,valid-win-buf-p ,executing-kbd ,this-cmd ,expect-action)
            test-case))
        (let ((render-called nil)
              (debounce-request-called nil)
              (eglot-signature--active-signature (when active-sig '(:signatures [(:label "test")])))
              (eglot-signature--active-point (if point-changed 50 100)))
          (with-temp-buffer
            ;; Set point appropriately
            (insert (make-string 100 ?x))
            (goto-char (if point-changed 100 50))
            (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                        (lambda () sig-active-p))
                       ((symbol-function 'eglot-signature--valid-win-buf-p)
                        (lambda () valid-win-buf-p))
                       ((symbol-function 'eglot-signature--render-sig-frame-at-point)
                        (lambda () (setq render-called t)))
                       ((symbol-function 'eglot-signature--debounce-request)
                        (lambda (&rest _) (setq debounce-request-called t)))
                       (executing-kbd-macro executing-kbd)
                       (this-command this-cmd))
              (eglot-signature--on-post-command)
              (if expect-action
                  (progn
                    (should render-called)
                    (should debounce-request-called))
                (progn
                  (should (not render-called))
                  (should (not debounce-request-called)))))))))))

;; Test: eglot-signature--next-sig
(ert-deftest test-eglot-signature--next-sig-increment-index ()
  "Test navigate to next signature increments index and calls active."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)") (:label "foo(x, y, z)") (:label "foo(x, y, z, a)")]
                           :activeSignature 0))
         (active-called nil)
         (active-args nil)
         (eglot-signature--active-signature mock-sig-help))
    (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--valid-win-buf-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--active)
                (lambda (sig-help)
                  (setq active-called t)
                  (setq active-args sig-help))))
      (eglot-signature--next-sig)

      ;; Verify eglot-signature--active was called
      (should active-called)

      ;; Verify the activeSignature was incremented from 0 to 1
      (should (eq (plist-get active-args :activeSignature) 1))

      ;; Verify eglot-signature--active-signature was updated
      (should (eq (plist-get eglot-signature--active-signature :activeSignature) 1)))))

(ert-deftest test-eglot-signature--next-sig-decrement-index ()
  "Test navigate to previous signature decrements index."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)") (:label "foo(x, y, z)") (:label "foo(x, y, z, a)")]
                           :activeSignature 2))
         (active-called nil)
         (active-args nil)
         (eglot-signature--active-signature mock-sig-help))
    (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--valid-win-buf-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--active)
                (lambda (sig-help)
                  (setq active-called t)
                  (setq active-args sig-help))))
      (eglot-signature--next-sig 'prev)

      ;; Verify eglot-signature--active was called
      (should active-called)

      ;; Verify the activeSignature was decremented from 2 to 1
      (should (eq (plist-get active-args :activeSignature) 1))

      ;; Verify eglot-signature--active-signature was updated
      (should (eq (plist-get eglot-signature--active-signature :activeSignature) 1)))))

(ert-deftest test-eglot-signature--next-sig-wrap-last-to-first ()
  "Test wraps around from last signature to first signature."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)") (:label "foo(x, y, z)") (:label "foo(x, y, z, a)")]
                           :activeSignature 2))
         (active-called nil)
         (active-args nil)
         (eglot-signature--active-signature mock-sig-help))
    (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--valid-win-buf-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--active)
                (lambda (sig-help)
                  (setq active-called t)
                  (setq active-args sig-help))))
      (eglot-signature--next-sig)

      ;; Verify eglot-signature--active was called
      (should active-called)

      ;; Verify the activeSignature wrapped from 2 to 0
      (should (eq (plist-get active-args :activeSignature) 0))

      ;; Verify eglot-signature--active-signature was updated
      (should (eq (plist-get eglot-signature--active-signature :activeSignature) 0)))))

(ert-deftest test-eglot-signature--next-sig-wrap-first-to-last-prev ()
  "Test wraps around from first signature to last signature using prev."
  (let* ((mock-sig-help '(:signatures [(:label "foo(x, y)") (:label "foo(x, y, z)") (:label "foo(x, y, z, a)")]
                           :activeSignature 0))
         (active-called nil)
         (active-args nil)
         (eglot-signature--active-signature mock-sig-help))
    (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--valid-win-buf-p)
                (lambda () t))
               ((symbol-function 'eglot-signature--active)
                (lambda (sig-help)
                  (setq active-called t)
                  (setq active-args sig-help))))
      (eglot-signature--next-sig 'prev)

      ;; Verify eglot-signature--active was called
      (should active-called)

      ;; Verify the activeSignature wrapped from 0 to 2
      (should (eq (plist-get active-args :activeSignature) 2))

      ;; Verify eglot-signature--active-signature was updated
      (should (eq (plist-get eglot-signature--active-signature :activeSignature) 2)))))

(ert-deftest test-eglot-signature--next-sig-no-op-combined ()
  "Test no-op scenarios (combined test cases)."
  (let ((test-cases '(;; (name sig-active-p valid-win-buf-p sig-help expect-active-called)
                      ("sig-active-p returns nil"
                       nil t (:signatures [(:label "foo") (:label "bar")]) nil)
                      ("valid-win-buf-p returns nil"
                       t nil (:signatures [(:label "foo") (:label "bar")]) nil)
                      ("signatures list is nil"
                       t t (:signatures nil) nil)
                      ("signatures is not a vector"
                       t t (:signatures ((:label "foo") (:label "bar"))) nil)
                      ("only one signature"
                       t t (:signatures [(:label "foo")]) nil))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,sig-active-p ,valid-win-buf-p ,sig-help ,expect-active-called) test-case))
        (let ((active-called nil))
          (setq eglot-signature--active-signature sig-help)
          (cl-letf* (((symbol-function 'eglot-signature--sig-active-p)
                      (lambda () sig-active-p))
                     ((symbol-function 'eglot-signature--valid-win-buf-p)
                      (lambda () valid-win-buf-p))
                     ((symbol-function 'eglot-signature--active)
                      (lambda (_sig-help)
                        (setq active-called t))))
            (eglot-signature--next-sig)
            (should (eq active-called expect-active-called))))))))

;; Test: eglot-signature--client-capabilities
(ert-deftest test-eglot-signature--client-capabilities ()
  "Test client capabilities modification (combined test cases)."
  (let ((test-cases
         ;; (name orig-caps)
         ;; Basic: adds contextSupport to signatureHelp
         '(("basic capability modification adds contextSupport to signatureHelp"
            (:textDocument (:signatureHelp (:otherProp "value"))))
           ;; Preserves other textDocument capabilities
           ("preserves other textDocument capabilities when modifying signatureHelp"
            (:textDocument
              (:signatureHelp (:otherProp "value"))
              :completion (:completionItem (:snippetSupport t))
              :hover (:contentFormat ["markdown" "plaintext"])))
           ;; Preserves other top-level capabilities
           ("preserves other top-level capabilities when modifying textDocument"
            (:textDocument (:signatureHelp (:otherProp "value"))
              :workspace (:symbol (:symbolKind (:valueSet [1 2 3]))))))))
    (dolist (test-case test-cases)
      (pcase-let* ((`(,_name ,orig-caps) test-case))
        ;; Mock the original function to return orig-caps
        (cl-letf* (((symbol-function 'eglot-client-capabilities)
                    (lambda (&rest _args) orig-caps)))
          ;; Call the advice function
          (let ((result (apply #'eglot-signature--client-capabilities
                               #'eglot-client-capabilities
                               nil)))
            ;; Verify contextSupport is added to signatureHelp
            (let* ((text-doc (plist-get result :textDocument))
                   (sig-help (plist-get text-doc :signatureHelp)))
              (should (plist-get sig-help :contextSupport)))))))))


(provide 'eglot-signature-test)
