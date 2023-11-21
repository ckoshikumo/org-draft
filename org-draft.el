;;; -*- lexical-binding: t; -*-

(defun org-draft-remove-all ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'org-draft-indent t)
  (remove-overlays (point-min) (point-max) 'org-draft-padding t))

(defun org-draft-add-padding ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((ov (make-overlay (point) (1+ (point)) nil t)))
      (overlay-put ov 'org-draft-padding t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'before-string (propertize "\n" 'face 'default 'line-height 0.8)))))

(add-hook 'org-insert-heading-hook #'org-draft-add-padding)

(defun org-draft-auto-indent (_beg _end _len)
  (when (bound-and-true-p org-draft-mode)
    (let ((beg (save-excursion (search-backward "\n\n" nil 'limit 3) (point)))
          (end (save-excursion (search-forward "\n\n" nil 'limit  3) (point))))
      (remove-overlays beg end 'org-draft-indent t)
      (org-draft-add-indents beg end))))

(defun org-draft-add-indents (&optional beg end)
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\n+\\(\n\\)" end t)
      (unless (save-match-data (looking-at "\\*+ \\|#\+ \\|\|"))
        (org-draft--add-indent (match-beginning 1) (match-end 1))))))

(defun org-draft--add-indent (beg end)
  (let ((ov (make-overlay beg end nil t)))
    (overlay-put ov 'org-draft-indent t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'display "    ")))

(defun org-draft-goto-eol (&rest _args)
  (when (and (bound-and-true-p org-draft-mode)
             (get-char-property (point) 'org-draft-indent))
    (end-of-visual-line)))

(defun org-draft-goto-bol (orig-fun &rest args)
  (let ((orig (point)))
    (apply orig-fun args)
    (when (and (bound-and-true-p org-draft-mode)
               (get-char-property (point) 'org-draft-indent))
      (forward-line)
      (when (= orig (point))
        (forward-line -1)))))

(defun org-draft--at-indent-p ()
  (and (bound-and-true-p org-draft-mode)
       (get-char-property (point) 'org-draft-indent)))

(defvar org-draft-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode org-draft-mode
  "Minor mode for writing with indented paragraphs."
  :lighter " draft"
  (advice-add 'org-end-of-line :after #'org-draft-goto-eol)
  (advice-add 'org-beginning-of-line :around #'org-draft-goto-bol)

  (add-hook 'after-change-functions #'org-draft-auto-indent)
  (keymap-set org-draft-mode-map "<remap> <org-end-of-line>" 'org-draft-goto-eol)
  (org-draft-add-indents))

(provide 'org-draft-mode)
