;;; sclang-helper.el --- Enhance the functionality of scel -*- lexical-binding: t; -*-

;;; Commentary:
;; This program contains serveral custom additional functions to scel.

;;; Code:

(require 'cl-lib)
(require 'sclang)
(require 'pulse)

(defun sclang-helper-eval-line (&optional silent-p)
  "Execute the current line as SuperCollider code with highlight."
  (interactive "P")
  (let ((string (sclang-line-at-point)))
    (when string
      (sclang-eval-string string (not silent-p)))
    (pulse-momentary-highlight-one-line (point)) ;; highlighting
    string))

(defun sclang-helper-eval-region (&optional silent-p)
  "Execute the region as SuperCollider code with highlight"
  (interactive "P")
  (sclang-eval-string
   (buffer-substring-no-properties (region-beginning) (region-end))
   (not silent-p))
  (pulse-momentary-highlight-region (region-beginning) (region-end))) ;; highlighting

(defun sclang-helper-eval-region-or-line (&optional silent-p)
  "Execute the line or region as SuperCollider code with highlight"
  (interactive "P")
  (if (and transient-mark-mode mark-active)
      (sclang-helper-eval-region silent-p)
    (sclang-helper-eval-line silent-p)))

;; TODO: It's not functional when the prentheses don't match.
(defun sclang-helper-eval-paren-region (&optional silent-p)
  "Execute the region inside the outermost parentheses.
Return t if the evaluation happens, nil if it doens't happen."
  (interactive "P")
  (cl-flet
      ((check-char-text (char)
                        "Check if the char is normal text"
                        (if (and (characterp char)
                                 (not (member (get-char-code-property char 'general-category) '(Zs Cc))))
                            t
                          nil)))

    (save-excursion
      ;; iteratively check if there's an upper level left parenthesis
      (let ((paren-matched nil))
        (while
            (condition-case nil
                (progn
                  (if (equal (char-to-string (char-after)) "(")
                      (setq paren-matched t)
                    (setq paren-matched nil))
                  (backward-up-list)
                  t)
              (error nil)))
        (if paren-matched
            (progn
              (let ((point-paren-1 (point))
                    (point-paren-2 (progn (forward-list) (point))))
                ;; check if there's nothing around the paired parentheses
                (if (or (check-char-text (char-before point-paren-1))
                        ;; after calling forward-list, point-paren-2 is actually the char after
                        ;; the right parenthesis
                        (check-char-text (char-after point-paren-2)))
                    nil
                  (progn
                    (set-mark point-paren-1)
                    (setq mark-active t)
                    (sclang-helper-eval-region-or-line silent-p)
                    (setq mark-active nil)
                    t))))
          nil)))))

(defun sclang-helper-auto-eval (&optional silent-p)
  (interactive "P")
  (if (and transient-mark-mode mark-active)
      (sclang-eval-region silent-p)
    (if (not (sclang-helper-eval-paren-region silent-p))
        (sclang-helper-eval-line silent-p))))

(defun sclang-helper-show-meter ()
  (interactive)
  (sclang-eval-string "Server.local.meter"))

(defun sclang-helper-show-freqscope()
  (interactive)
  (sclang-eval-string "Server.local.freqscope"))

(defun sclang-helper-show-tree()
  (interactive)
  (sclang-eval-string "Server.local.plotTree"))

;;;###autoload
(define-minor-mode sclang-helper-mode
  "Toggle sclang mode."
  :lighter " sclang-helper"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<s-return>") 'sclang-helper-auto-eval)
            (define-key map (kbd "<C-return>") 'sclang-helper-auto-eval)
            (define-key map (kbd "s-m") 'sclang-helper-show-meter)
            (define-key map (kbd "s-f") 'sclang-helper-show-freqscope)
            (define-key map (kbd "s-n") 'sclang-helper-show-tree)
            (define-key map (kbd "s-.") 'sclang-main-stop)
            (define-key map (kbd "s-d") 'sclang-find-help-in-gui)
            map))

(provide 'sclang-helper)

;;; sclang-helper.el ends here
