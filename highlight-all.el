;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(hl-line-mode)

(defun hl-line-highlight2 ()
  "Activate the Hl-Line overlay on the current line."
  (interactive)
        (unless (overlayp hl-line-overlay)
          (setq hl-line-overlay (hl-line-make-overlay))) ; To be moved.
        (overlay-put hl-line-overlay
                     'window (unless hl-line-sticky-flag (selected-window)))
	(hl-line-move hl-line-overlay)
        (hl-line-maybe-unhighlight))

(defun hl-all-my ()
  (interactive)
  (hl-line-highlight2)
  (apply-function-all 'hl-line-highlight2)
  )

(defun apply-function-all (func)
  "Apply function FUNC with argument ARG to all visible windows."
  (let ((num-windows (count-windows))
	(count 1))
    (when (> num-windows 1)
      (other-window 1)
      (while (< count num-windows)
	(condition-case nil
	    (funcall func)
	  ;; Ignore beginning- or end-of-buffer error in other windows.
          (error nil))
	(other-window 1)
	(setq count (1+ count))))))


(add-hook 'post-command-hook 'hl-all-my)
(scroll-all-mode)
;;(remove-hook 'post-command-hook 'scroll-all-check-to-scroll)
;;(add-hook 'post-command-hook 'scroll-all-check-to-scroll)
