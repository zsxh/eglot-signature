;;; eglot-signature.el --- Signature help for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  zsxh

;; Author: zsxh <bnbvbchen@gmail.com>
;; Maintainer: zsxh <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/eglot-signature
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (compat "30.1.0.0") (eglot "1.17.30") (jsonrpc "1.0.24"))
;; Keywords: eglot signature tools languages lsp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; eglot-signature provides signature help (parameter hints) for Eglot.
;;
;; It implements the LSP Signature Help protocol (3.17) to display
;; function signatures with active parameter highlighting when typing
;; function arguments.
;;
;; Features:
;;
;; - Automatic triggering on trigger characters (e.g., `(`, `,`)
;; - Active parameter highlighting
;; - Multi-signature overload navigation
;; - Function and parameter documentation display
;; - Child frame popup display
;; - Context support for retriggering signature help
;;
;; Usage:
;;
;;   (require 'eglot)
;;   (require 'eglot-signature)
;;   (eglot-signature-setup)
;;   (add-hook 'eglot-managed-mode-hook 'eglot-signature-mode)
;;
;; Or use `use-package':
;;
;;   (use-package eglot-signature
;;     :vc (:url "https://github.com/zsxh/eglot-signature"
;;         :rev :newest)
;;     :hook (eglot-managed-mode . eglot-signature-mode)
;;     :init
;;     (with-eval-after-load 'eglot
;;       (eglot-signature-setup)))
;;
;; Customization:
;;
;;   `eglot-signature-max-height' - Maximum frame height (default: 10 lines)
;;   `eglot-signature-max-width' - Maximum frame width (default: 60 chars)
;;   `eglot-signature-show-doc' - Show documentation (default: t)
;;   `eglot-signature-show-param-doc' - Show parameter documentation (default: t)
;;   `eglot-signature-debounce-delay' - Request debounce delay in seconds (default: 0.2)
;;
;; Faces:
;;
;;   `eglot-signature-active-parameter' - Face for active parameter highlighting
;;   `eglot-signature-doc-default' - Face for the signature help display (background, colors)
;;   `eglot-signature-doc-separator-face' - Face for document separator line
;;
;; Commands:
;;
;;   `eglot-signature-toggle' - Toggle signature help mode
;;   `eglot-signature-show' - Manually show signature help at point
;;   `eglot-signature-quit' - Quit signature help
;;   `eglot-signature-next' - Navigate to next signature overload
;;   `eglot-signature-prev' - Navigate to previous signature overload
;;   `eglot-signature-switch-to-doc-buffer' - Switch to documentation buffer
;;
;; Key Bindings:
;;
;;   C-c s - Manually invoke signature help
;;
;; When signature help is displayed, use the transient keymap:
;;
;;   <up> / <down> - Navigate between signature overloads
;;   C-g / <escape> - Hide signature help
;;
;; Protocol Details:
;;
;; This package implements the LSP Signature Help protocol (3.17) with:
;;
;; - contextSupport: Enables retriggering with active signature help state
;; - labelOffsetSupport: Handles parameter labels as offset ranges
;; - SignatureInformation.activeParameter: Per-signature active parameters
;;
;; Trigger types:
;;   1. Invoked - Manual trigger via command/keybinding
;;   2. TriggerCharacter - Typing trigger character (e.g., `(`)
;;   3. ContentChange - Continuing to edit while signature is active

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eglot)
(require 'jsonrpc)


(defgroup eglot-signature nil
  "Signature help for Eglot."
  :group 'eglot
  :prefix "eglot-signature-"
  :link '(url-link :tag "GitHub" "https://github.com/zsxh/eglot-signature")
  :link '(url-link :tag "LSP Spec" "https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/"))

;; Faces

(defface eglot-signature-active-parameter
  '((t (:inherit font-lock-function-name-face)))
  "Face for highlighting the active parameter."
  :group 'eglot-signature)

(defface eglot-signature-doc-default
  '((((class color) (min-colors 88) (background dark)) :background "#191a1b")
    (((class color) (min-colors 88) (background light)) :background "#f0f0f0")
    (((background dark)) :background "gray" :foreground "black")
    (t :background "gray"))
  "Face for the signature help frame."
  :group 'eglot-signature)

(defface eglot-signature-doc-separator-face
  '((t (:overline t :extend t :height 0.1)))
  "Face for the signature document separator line."
  :group 'eglot-signature)

;; Customization

(defcustom eglot-signature-max-height 10
  "Maximum height of the signature frame in lines."
  :type 'integer
  :group 'eglot-signature)

(defcustom eglot-signature-max-width 60
  "Maximum width of the signature frame in characters."
  :type 'integer
  :group 'eglot-signature)

(defcustom eglot-signature-show-doc t
  "Whether to show documentation in signature help."
  :type 'boolean
  :group 'eglot-signature)

(defcustom eglot-signature-show-param-doc t
  "Whether to show parameter documentation in signature help."
  :type 'boolean
  :group 'eglot-signature)

(defcustom eglot-signature-debounce-delay 0.2
  "Delay in seconds before debouncing signature help requests."
  :type 'float
  :group 'eglot-signature)

;; Variables

(defconst eglot-signature--doc-buffer-name " *eglot-signature*"
  "Name of the signature help display buffer.")

(defconst eglot-signature--trigger-kinds
  '((:invoked . 1)
    (:trigger-character . 2)
    (:content-change . 3))
  "LSP SignatureHelpTriggerKind symbol to value mapping.")

(defvar eglot-signature--active-signature nil
  "Active signature help.")

(defvar eglot-signature--active-frame nil
  "Active signature frame.")

(defvar eglot-signature--cached-frame-size nil
  "Cached frame size (width-pixel . height-pixel).")

(defvar eglot-signature--active-buffer nil
  "Active buffer where signature help was requested.")

(defvar eglot-signature--active-point nil
  "Point position when signature help was activated.")

(defvar eglot-signature--doc-separator-lines nil
  "Number of separator lines between signature and documentation.")

(defvar eglot-signature--doc-buffer nil
  "Buffer used to display signature help content.")

(defvar eglot-signature--debounce-timer nil
  "Debounce timer for signature help requests.")

(defvar-local eglot-signature--provider nil
  "The LSP server provider capability for signature help.")

;; Core Request

(defun eglot-signature--request (trigger-kind &optional trigger-character)
  "Request signature help from server.
TRIGGER-KIND is the trigger type symbol (:invoked, :trigger-character, :content-change).
TRIGGER-CHARACTER is the character that triggered the request.

Side effects:
- Sets `eglot-signature--active-buffer' and `eglot-signature--active-point'
- Sends pending text changes before requesting"
  (when-let* ((server (and (eglot-managed-p) (eglot-current-server))))
    (let* ((buffer (current-buffer))
           (request-point (point))
           (active-sig eglot-signature--active-signature)
           (trigger-kind-val (alist-get trigger-kind eglot-signature--trigger-kinds))
           (retrigger-p (and (not (eq trigger-kind :invoked))
                             (eglot-signature--sig-active-p)
                             active-sig))
           (context (list
                     :triggerKind trigger-kind-val
                     :isRetrigger (if retrigger-p t :json-false))))

      (when (and trigger-character (eq trigger-kind :trigger-character))
        (setq context (append context (list :triggerCharacter trigger-character))))

      (when retrigger-p
        (setq context (append context (list :activeSignatureHelp active-sig))))

      (setq eglot-signature--active-buffer buffer)
      (setq eglot-signature--active-point request-point)

      ;; send pending changes before making request
      (eglot--signal-textDocument/didChange)

      (jsonrpc-async-request
       server
       :textDocument/signatureHelp
       (append
        (eglot--TextDocumentPositionParams)
        `(:context ,context))
       :success-fn
       (lambda (response)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (eq buffer eglot-signature--active-buffer)
                        (eq (point) request-point))
               (setq eglot-signature--active-signature response)
               (eglot-signature--handle-response response)))))
       :error-fn
       (lambda (err)
         (message "SignatureHelp error: %S" err)
         (eglot-signature--quit))
       :deferred :textDocument/signatureHelp))))

(defun eglot-signature--handle-response (response)
  "Handle signature help RESPONSE from server."
  (if (or (null response)
          (let ((sig-list (plist-get response :signatures)))
            (or (null sig-list)
                (not (vectorp sig-list))
                (not (length> sig-list 0)))))
      (eglot-signature--quit)
    (eglot-signature--active response)))

;; Helper

(defun eglot-signature--sig-active-p ()
  "Check if signature help is currently active."
  (let ((frame eglot-signature--active-frame)
        (active-sig eglot-signature--active-signature))
    (and active-sig
         frame
         (frame-live-p frame)
         (frame-visible-p frame))))

(defun eglot-signature--active (signature-help)
  "Display signature help popup with SIGNATURE-HELP.
Sets up transient map and window change hooks to manage the
lifetime and interactivity of the signature frame."
  (let ((sig-buffer (eglot-signature--prepare-buffer signature-help)))
    (eglot-signature--render-sig-frame-at-point sig-buffer)
    (unless (memq 'eglot-signature--window-change window-buffer-change-functions)
      (add-hook 'window-buffer-change-functions #'eglot-signature--window-change nil t))
    (unless (memq 'eglot-signature--window-change window-selection-change-functions)
      (add-hook 'window-selection-change-functions #'eglot-signature--window-change nil t))
    (set-transient-map eglot-signature-popup-map t)))

(defun eglot-signature--quit ()
  "Quit signature help display."
  (let ((timer eglot-signature--debounce-timer)
        (frame eglot-signature--active-frame)
        (doc-buf eglot-signature--doc-buffer)
        (active-buf eglot-signature--active-buffer))

    ;; Cancel pending timer
    (when (timerp timer)
      (cancel-timer timer))

    ;; Hide frame
    (when (and frame (frame-live-p frame))
      (make-frame-invisible frame))

    ;; Clear doc buffer
    (when (buffer-live-p doc-buf)
      (unless (get-buffer-window doc-buf)
        (with-current-buffer doc-buf
          (let ((inhibit-read-only t))
            (erase-buffer)))))

    ;; Remove hooks from active buffer
    (when (buffer-live-p active-buf)
      (with-current-buffer active-buf
        (remove-hook 'window-buffer-change-functions
                     #'eglot-signature--window-change t)
        (remove-hook 'window-selection-change-functions
                     #'eglot-signature--window-change t))))

  ;; Reset all state
  (setq eglot-signature--debounce-timer nil
        eglot-signature--active-signature nil
        eglot-signature--active-buffer nil
        eglot-signature--active-point nil
        eglot-signature--doc-separator-lines nil
        eglot-signature--cached-frame-size nil))

(defun eglot-signature--window-change (win)
  "Handle window/buffer change events.
WIN is the window that changed."
  (when (and (eglot-signature--sig-active-p)
             (not (eglot-signature--valid-win-buf-p))
             (not (eq (window-buffer (selected-window))
                      eglot-signature--doc-buffer)))
    (eglot-signature--quit)))

(defun eglot-signature--valid-win-buf-p ()
  "Check if current window/buffer is valid for signature help.
Returns non-nil if selected window shows the active buffer
or is within the signature frame."
  (let* ((win (selected-window))
         (buf (window-buffer win)))
    (or
     (eq buf eglot-signature--active-buffer)
     (eq (window-frame win) eglot-signature--active-frame))))

(defun eglot-signature--valid-place-p ()
  "Check if current position is valid for signature help.
Returns non-nil if point is in valid buffer and hasn't moved
since signature was activated."
  (and (eglot-signature--valid-win-buf-p)
       (eq (point) eglot-signature--active-point)))

;; Frame Display

(defun eglot-signature--doc-empty-p (doc)
  "Check if DOC content is empty.
Handles both string and markup-content (with :value)."
  (let ((str (if (stringp doc) doc (plist-get doc :value))))
    (or (null str)
        (string= str ""))))

(defun eglot-signature--doc-highlight-label (label active-param)
  "Highlight active parameter in LABEL string.
 Applies `eglot-signature-active-parameter' to the active parameter
 specified by ACTIVE-PARAM (either offset vector or string).
 Returns the highlighted string."
  (if (not (stringp label))
      ""
    (with-temp-buffer
      (insert label)
      ;; Apply parameter highlighting
      (when-let* ((param-label (and active-param
                                    (plist-get active-param :label))))
        (cond
         ;; Offset label [start, end]
         ((vectorp param-label)
          (let ((start (elt param-label 0))
                (end (elt param-label 1)))
            (add-face-text-property (1+ start) (1+ end)
                                    'eglot-signature-active-parameter)))
         ;; String label - find and highlight it
         ((stringp param-label)
          (goto-char (point-min))
          (when (search-forward param-label nil t)
            (add-face-text-property (match-beginning 0)
                                    (match-end 0)
                                    'eglot-signature-active-parameter)))))
      (buffer-string))))

(defun eglot-signature--prepare-buffer (signature-help)
  "Prepare signature buffer with SIGNATURE-HELP and return BUFFER."
  (let* ((sig-list (plist-get signature-help :signatures))
         (active-sig-idx (max 0 (or (plist-get signature-help :activeSignature) 0)))
         (sig-length (and sig-list (length sig-list)))
         (sig (and sig-list
                   (> sig-length active-sig-idx)
                   (aref sig-list active-sig-idx)))
         (buffer eglot-signature--doc-buffer)
         (sig-counter (when (> (length sig-list) 1)
                        (format "[%d/%d]" (1+ active-sig-idx) (length sig-list)))))

    (unless (and buffer (buffer-live-p buffer))
      (setq buffer (get-buffer-create eglot-signature--doc-buffer-name))
      (setq eglot-signature--doc-buffer buffer)
      (let ((fr face-remapping-alist))
        (with-current-buffer buffer
          (setq-local header-line-format nil
                      mode-line-format nil
                      left-fringe-width 0
                      right-fringe-width 0
                      left-margin-width 0
                      right-margin-width 0
                      fringes-outside-margins 0)
          (setq-local face-remapping-alist (copy-tree fr))
          (cl-pushnew 'eglot-signature-doc-default
                      (alist-get 'default face-remapping-alist)))))

    (with-current-buffer buffer
      (setq eglot-signature--doc-separator-lines 0)
      (with-silent-modifications
        (erase-buffer)
        (save-excursion
          (when sig
            ;; Format and insert signature
            (let* ((label (plist-get sig :label))
                   (active-param-idx (max 0 (or (plist-get sig :activeParameter)
                                                (plist-get signature-help :activeParameter)
                                                0)))
                   (params (plist-get sig :parameters))
                   (params-length (and params (length params)))
                   (active-param (and params
                                      (> params-length active-param-idx)
                                      (aref params active-param-idx)))
                   (fill-column eglot-signature-max-width))

              ;; Add signature counter
              (when sig-counter
                (insert sig-counter " "))

              ;; Add signature label
              (insert (eglot-signature--doc-highlight-label label active-param))

              ;; Add parameter documentation (active parameter only)
              (when-let* ((doc (and eglot-signature-show-param-doc
                                    active-param
                                    (plist-get active-param :documentation)))
                          (_ (not (eglot-signature--doc-empty-p doc)))
                          (formatted-doc (eglot--format-markup doc)))
                (insert "\n"
                        (propertize "\n" 'face 'eglot-signature-doc-separator-face))
                (insert formatted-doc)
                (cl-incf eglot-signature--doc-separator-lines))

              ;; Add documentation
              (when-let* ((doc (and eglot-signature-show-doc
                                    (plist-get sig :documentation)))
                          (_ (not (eglot-signature--doc-empty-p doc)))
                          (formatted-doc (eglot--format-markup doc)))
                (insert "\n"
                        (propertize "\n" 'face 'eglot-signature-doc-separator-face))
                (insert formatted-doc)
                (cl-incf eglot-signature--doc-separator-lines))

              ;; Fill columns
              (goto-char (point-min))
              (while (< (point) (point-max))
                (let ((line-beg (point))
                      (line-end (line-end-position)))
                  (when (> (- line-end line-beg) fill-column)
                    (fill-region line-beg line-end)))
                (forward-line 1)))))))
    buffer))

(defun eglot-signature--buffer-frame-size (buffer w-edges)
  "Calculate required frame size for BUFFER content.

W-EDGES is the window pixel edges (left top right bottom) used for
width constraint calculation.

Returns (width-pixel . height-pixel) cons cell where width and height
are in pixels. Height is constrained by `eglot-signature-max-height'
and separator lines are accounted for with reduced height contribution.

Results are cached in `eglot-signature--cached-frame-size'."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (cl-letf* (((window-dedicated-p) nil)
                 ((window-buffer) (current-buffer))
                 (char-height (default-font-height))
                 (max-height-pixel
                  (+ (* char-height eglot-signature-max-height)
                     (ceiling (* eglot-signature--doc-separator-lines
                                 char-height
                                 0.1))))
                 (w-width (- (nth 2 w-edges) (nth 0 w-edges)))
                 (w-height (- (nth 3 w-edges) (nth 1 w-edges)))
                 (size (window-text-pixel-size
                        nil (point-min) (point-max)
                        w-width (min (/ w-height 3)
                                     max-height-pixel))))
        (setq eglot-signature--cached-frame-size size)
        size))))

(defun eglot-signature--frame-geometry (buf-size cursor-xy)
  "Calculate frame position and size for signature help.

BUF-SIZE is (width-pixel . height-pixel) cons cell with the required
frame dimensions in pixels.

CURSOR-XY is (x . y) cons cell with cursor position in pixels relative
to window origin.

Returns list (x y width-pixel height-pixel) with frame geometry:
- x, y: Frame position in pixels (relative to frame origin)
- width-pixel, height-pixel: Frame dimensions in pixels

Positions frame above cursor if space is available, otherwise below.
Horizontally adjusts to prevent frame overflow beyond frame width."
  (let* ((line-height (default-line-height))
         (size buf-size)
         (width-pixel (car size))
         (height-pixel (cdr size))
         (cursor-x (car cursor-xy))
         (cursor-y (cdr cursor-xy))
         (fw (frame-pixel-width))
         (padding 4)
         (x (if (> (+ cursor-x width-pixel padding) fw)
                (- fw width-pixel padding)
              cursor-x))
         (cursor-above-y (- cursor-y height-pixel padding))
         (y (if (> cursor-above-y line-height)
                cursor-above-y
              (+ cursor-y line-height padding))))
    (list x y width-pixel height-pixel)))

(defun eglot-signature--update-frame-size-and-position (frame x y width-pixel height-pixel)
  "Update existing FRAME with new position and size.
Makes frame visible."
  (if (functionp 'set-frame-size-and-position-pixelwise)
      (let ((frame-resize-pixelwise t))
        (set-frame-size-and-position-pixelwise frame width-pixel height-pixel x y))
    (set-frame-size frame width-pixel height-pixel t)
    (set-frame-position frame x y))
  (unless (frame-visible-p frame)
    (make-frame-visible frame)))

(defun eglot-signature--make-frame (x y)
  "Create a child frame for signature help at X Y coordinates.
Returns the new frame configured as a popup child frame."
  (let* ((border-color (face-foreground 'default nil t))
         (parent (window-frame))
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (user-position t)
                   (minibuffer . ,(minibuffer-window parent))
                   (left . ,x)
                   (top . ,y)
                   (visibility . nil)
                   (child-frame-border-width . 1)
                   (border-width . 0)
                   (outer-border-width . 0)
                   (min-width . 0)
                   (min-height . 0)
                   (tool-bar-lines . 0)
                   (menu-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (undecorated . t)
                   (unsplittable . t)
                   (fullscreen . nil)
                   (no-other-frame . t)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (cursor-type . nil)
                   (no-special-glyphs . t)
                   (desktop-dont-save . t)
                   (skip-taskbar . t)
                   (z-group . above)))))
    (setq eglot-signature--active-frame frame)
    (set-face-attribute 'internal-border frame :background border-color)
    (set-face-attribute 'child-frame-border frame :background border-color)
    frame))

(defun eglot-signature--render-sig-frame-at-point (&optional sig-buf)
  "Render signature help frame at cursor position.
Creates or updates child frame with content from SIG-BUFFER."
  (let* ((sig-changed-p (and sig-buf (buffer-live-p sig-buf)))
         (w-edges (window-inside-pixel-edges))
         (buf-size (if sig-changed-p
                       (eglot-signature--buffer-frame-size sig-buf w-edges)
                     eglot-signature--cached-frame-size))
         (cursor-xy (let* ((xy (posn-x-y (posn-at-point)))
                           (x (+ (nth 0 w-edges) (car xy)))
                           (y (+ (nth 1 w-edges) (cdr xy))))
                      (cons x y)))
         (geometry (eglot-signature--frame-geometry buf-size cursor-xy))
         (x (nth 0 geometry))
         (y (nth 1 geometry))
         (width-pixel (nth 2 geometry))
         (height-pixel (nth 3 geometry))
         (frame eglot-signature--active-frame))
    (unless (and frame (frame-live-p frame))
      (setq frame (eglot-signature--make-frame x y)))
    (let ((win (frame-root-window frame)))
      (when (or sig-changed-p (or (eq (window-buffer win) sig-buf)))
        (set-window-buffer win sig-buf)))
    (if sig-changed-p
        (eglot-signature--update-frame-size-and-position
         frame x y width-pixel height-pixel)
      (set-frame-position frame x y))))

;; Debounce Helper

(defun eglot-signature--debounce-request (trigger-kind &optional trigger-character)
  "Debounce signature help request.
Uses `eglot-signature-debounce-delay' for the delay time.
TRIGGER-KIND is trigger type symbol (:invoked, :trigger-character, :content-change).
TRIGGER-CHARACTER is the character that triggered the request."
  (when eglot-signature-mode
    (if (timerp eglot-signature--debounce-timer)
        (timer-set-idle-time eglot-signature--debounce-timer
                             eglot-signature-debounce-delay)
      (setq eglot-signature--debounce-timer
            (run-with-idle-timer
             eglot-signature-debounce-delay nil
             (lambda (doc-buf)
               (setq eglot-signature--debounce-timer nil)
               (when (buffer-live-p doc-buf)
                 (with-current-buffer doc-buf
                   (when (or eglot-signature-mode
                             (not (eglot-signature--sig-active-p))
                             (eglot-signature--valid-win-buf-p))
                     (eglot-signature--request trigger-kind trigger-character)))))
             (current-buffer))))))

;; Trigger Handling

(defun eglot-signature--on-self-insert ()
  "Handle character insertion for trigger characters.
Renders current signature frame positionally, then triggers
debounced request based on whether character is a trigger,
retrigger, or content-change trigger."
  (when eglot-signature--provider
    (let* ((char (string last-command-event))
           (provider eglot-signature--provider)
           (trigger-chars (and provider (plist-get provider :triggerCharacters)))
           (retrigger-chars (and provider (plist-get provider :retriggerCharacters)))
           (sig-active-p (eglot-signature--sig-active-p)))
      (when sig-active-p
        (eglot-signature--render-sig-frame-at-point))
      (cond
       ((and trigger-chars (seq-contains-p trigger-chars char))
        (eglot-signature--debounce-request :trigger-character char))
       ((and sig-active-p retrigger-chars (seq-contains-p retrigger-chars char))
        (eglot-signature--debounce-request :trigger-character char))
       (sig-active-p
        (eglot-signature--debounce-request :content-change))
       (t nil)))))

(defun eglot-signature--on-post-command ()
  "Handle post-command for content change retriggers.
Renders frame and debounces request when point has changed."
  (when (and eglot-signature--active-signature
             (eglot-signature--sig-active-p)
             (eglot-signature--valid-win-buf-p)
             (not executing-kbd-macro)
             (not (member this-command '(self-insert-command
                                         corfu-insert
                                         company--insert-candidate))))
    (unless (eq (point) eglot-signature--active-point)
      (eglot-signature--render-sig-frame-at-point)
      (eglot-signature--debounce-request :content-change))))

(defun eglot-signature--next-sig (&optional prev)
  "Navigate to next or previous signature.
If PREV is non-nil, navigate to previous signature.
Otherwise, navigate to next signature. Wraps around at boundaries."
  (when-let* ((_ (eglot-signature--sig-active-p))
              (_ (eglot-signature--valid-win-buf-p))
              (sig-help eglot-signature--active-signature)
              (sig-list (plist-get sig-help :signatures))
              (sig-length (and sig-list (vectorp sig-list) (length sig-list)))
              (_ (> sig-length 1))
              (sig-idx (plist-get sig-help :activeSignature))
              (sig-idx (max 0 (or sig-idx 0)))
              (new-sig-idx (if prev (1- sig-idx) (1+ sig-idx)))
              (new-sig-idx (cond
                            ((>= new-sig-idx sig-length)
                             (- new-sig-idx sig-length))
                            ((< new-sig-idx 0)
                             (+ new-sig-idx sig-length))
                            (t new-sig-idx))))
    (setf (plist-get sig-help :activeSignature) new-sig-idx)
    (setq eglot-signature--active-signature sig-help)
    (eglot-signature--active sig-help)))

;; Cleanup

(defun eglot-signature--cleanup ()
  "Clean up signature help state."
  (let ((sig-frame eglot-signature--active-frame)
        (sig-doc-buf eglot-signature--doc-buffer))
    (eglot-signature--quit)
    (when (and sig-frame (frame-live-p sig-frame))
      (delete-frame sig-frame))
    (when (buffer-live-p sig-doc-buf)
      (kill-buffer sig-doc-buf))))

;; Keymap

(defvar eglot-signature-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") #'eglot-signature-show)
    map)
  "Keymap for `eglot-signature-mode'.")

(defvar eglot-signature-popup-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'eglot-signature-prev)
    (define-key map (kbd "<down>") #'eglot-signature-next)
    (define-key map (kbd "<escape>") #'eglot-signature-quit)
    (define-key map (kbd "C-g") #'eglot-signature-quit)
    map)
  "Transient keymap active while signature help is displayed.")

;; Minor Mode

(defun eglot-signature--client-capabilities (orig-fn &rest args)
  "Advice around `eglot-client-capabilities' to add signatureHelp contextSupport.
Returns modified capabilities with contextSupport enabled for signature help."
  (let* ((caps (apply orig-fn args))
         (text-doc (plist-get caps :textDocument))
         (sig-help (plist-get text-doc :signatureHelp)))
    (setq sig-help (plist-put sig-help :contextSupport t))
    (setq text-doc (plist-put text-doc :signatureHelp sig-help))
    (setq caps (plist-put caps :textDocument text-doc))
    caps))

(defun eglot-signature--capf-wrapper ()
  "Wrap `eglot-completion-at-point' to trigger signature help on exit."
  (when-let* ((capf (eglot-completion-at-point)))
    (cl-destructuring-bind (start end table &rest plist) capf
      (let ((orig-exit (plist-get plist :exit-function)))
        (setq plist
              (plist-put plist :exit-function
                         (lambda (proxy status)
                           (when orig-exit
                             (funcall orig-exit proxy status))
                           (when (and eglot-signature-mode
                                      (memq status '(finished exact)))
                             (when-let* ((lsp-item (get-text-property 0 'eglot--lsp-item proxy))
                                         (kind (plist-get lsp-item :kind))
                                         ;; Method, Function, Constructor, Class
                                         (_ (memq kind '(2 3 4 5))))
                               (eglot-signature--request :invoked))))))
        (append (list start end table) plist)))))

(defun eglot-signature--enable ()
  "Enable signature help in current buffer."
  (setq-local eglot-signature--provider (eglot-server-capable :signatureHelpProvider))
  (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
  (add-hook 'completion-at-point-functions #'eglot-signature--capf-wrapper nil t)
  (add-hook 'post-self-insert-hook #'eglot-signature--on-self-insert nil t)
  (add-hook 'post-command-hook #'eglot-signature--on-post-command nil t)
  (eglot-signature--quit))

(defun eglot-signature--disable ()
  "Disable signature help in current buffer."
  (remove-hook 'post-self-insert-hook #'eglot-signature--on-self-insert t)
  (remove-hook 'post-command-hook #'eglot-signature--on-post-command t)
  (remove-hook 'completion-at-point-functions #'eglot-signature--capf-wrapper t)
  (when (eglot-managed-p)
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t))
  (eglot-signature--cleanup))

(defun eglot-signature-setup ()
  "Setup Eglot signature support by advising client capabilities."
  (advice-add 'eglot-client-capabilities :around #'eglot-signature--client-capabilities)
  (when (and (bound-and-true-p corfu-mode)
             (functionp 'cape-wrap-buster))
    (advice-add 'eglot-signature--capf-wrapper :around #'cape-wrap-buster)))

(defun eglot-signature-teardown ()
  "Teardown Eglot signature support by removing client capabilities advice."
  (advice-remove 'eglot-client-capabilities #'eglot-signature--client-capabilities)
  (when (advice-member-p #'cape-wrap-buster 'eglot-signature--capf-wrapper)
    (advice-remove 'eglot-signature--capf-wrapper #'cape-wrap-buster)))

;;;###autoload
(define-minor-mode eglot-signature-mode
  "Toggle signature help for Eglot.

When enabled, shows function signature help when typing arguments.
Uses the LSP Signature Help protocol."
  :group 'eglot-signature
  :keymap eglot-signature-mode-map
  (cond
   (eglot-signature-mode
    (if (and (eglot-managed-p) (eglot-server-capable :signatureHelpProvider))
        (eglot-signature--enable)
      (message "Eglot server does not support signature help")
      (eglot-signature-mode -1)))
   (t
    (eglot-signature--disable))))

;; Commands

;;;###autoload
(defun eglot-signature-toggle ()
  "Toggle signature help mode.
Enables or disables eglot-signature-mode in the current buffer."
  (interactive)
  (if (eglot-managed-p)
      (if eglot-signature-mode
          (progn
            (eglot-signature-mode -1)
            (message "eglot-signature-mode disabled"))
        (eglot-signature-mode 1)
        (message "eglot-signature-mode enabled")
        (eglot-signature-show))
    (message "eglot is not active in this buffer")))

;;;###autoload
(defun eglot-signature-show ()
  "Show signature help at current point."
  (interactive)
  (if (eglot-managed-p)
      (eglot-signature--request :invoked)
    (message "eglot is not active in this buffer")))

;;;###autoload
(defun eglot-signature-quit ()
  "Quit signature help in current buffer."
  (interactive)
  (eglot-signature--quit))

;;;###autoload
(defun eglot-signature-switch-to-doc-buffer ()
  "Switch to the signature help documentation buffer.
Opens the documentation buffer in another window."
  (interactive)
  (let ((buf eglot-signature--doc-buffer))
    (when (and buf (buffer-live-p buf))
      (switch-to-buffer-other-window buf))))

;;;###autoload
(defun eglot-signature-next ()
  "Navigate to next signature overload.
Shows the next function signature in the list."
  (interactive)
  (eglot-signature--next-sig))

;;;###autoload
(defun eglot-signature-prev ()
  "Navigate to previous signature overload.
Shows the previous function signature in the list."
  (interactive)
  (eglot-signature--next-sig 'prev))


(provide 'eglot-signature)

;;; eglot-signature.el ends here
