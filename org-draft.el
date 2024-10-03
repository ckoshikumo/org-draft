;;; -*- lexical-binding: t; -*-

(require 'org)
(require 'seq)

(defgroup org-draft nil
  "Improve experience when using org-mode for writing long manuscripts."
  :prefix "org-draft-"
  :group 'convenience)

(defcustom org-draft-padding-before-headings t
  "Non-nil if you want to add vertical whitespace before headings
without having to use newlines."
  :group 'org-draft
  :local t
  :type 'bool)

(defcustom org-draft-hide-all-stars t
  "Non-nil to hide ALL leading heading stars, leaving only
fontification as the marker of a heading."
  :group 'org-draft
  :local t
  :type 'bool)

(defcustom org-draft-auto-indentation t
  "Non-nil if you want to translate an empty line between paragraphs
as indentation for the second paragraph."
  :group 'org-draft
  :local t
  :type 'bool)

(defcustom org-draft-indent-string "    "
  "The string to be used as indentation."
  :group 'org-draft
  :local t
  :type 'string)

(defcustom org-draft-auto-paginate t
  "Non-nil to automatically paginate document."
  :group 'org-draft
  :local t
  :type 'bool)

(defcustom org-draft-soft-page-break-char ?─
  "Character to use to make an automatic page break."
  :group 'org-draft
  :type 'character)

(defcustom org-draft-hard-page-break-char ?=
  "Character to use to make a hard page break."
  :group 'org-draft
  :type 'character)

(defcustom org-draft-page-break-spacing 1.1
  "Float to use as line-spacing for page breaks."
  :group 'org-draft
  :local t
  :type 'float)

(defcustom org-draft-lines-per-page 22
  "How many lines (approximatelly) in a page."
  :group 'org-draft
  :local t
  :type 'integer)

(defcustom org-draft-partial-repagination-count 3000
  "How many characters to repaginate each time."
  :group 'org-draft
  :type 'integer)

(defcustom org-draft-page-break-face 'org-draft-page-break-face
  "Which face to use for page breaks dividers."
  :group 'org-draft
  :type 'face)

(defface org-draft-page-break-face
  '((t :inherit 'fixed-pitch))
  "Default face for the page break dividers.")


;;; Commands and fixes:

(defvar-local org-draft--page-break-cache nil)
(defvar-local org-draft--idle-pagination-timer nil)

(defun org-draft-remove-overlays ()
  (interactive)
  (setq org-draft--page-break-cache nil)
  (remove-overlays (point-min) (point-max) 'org-draft-ov t))

(defun org-draft--visual-bol-pos ()
  (if (org-draft--on-page-break-p)
      (point)
    (save-excursion
      (vertical-motion 0)
      (point))))

(defun org-draft--visual-eol-pos ()
  (let ((eol (save-excursion
               (vertical-motion (cons (window-width) 0))
               (point)))
        (break (or (org-draft--get-next-page-break-pos (point)) (point-max))))
    (min eol break)))

(defun org-draft-goto-eol ()
  (interactive)
  (let ((orig (point)))
    (goto-char (org-draft--visual-eol-pos))
    (when (and (eq (point) orig)
               (save-excursion (org-draft-goto-bol) (org-at-heading-p)))
      (org-end-of-line))))

(defun org-draft-goto-bol ()
  (interactive)
  (goto-char (org-draft--visual-bol-pos)))

(defun org-draft-next-line (&optional arg)
  (interactive "p")
  (setq arg (abs (or arg 1)))
  (let ((col (- (point) (org-draft--visual-bol-pos))))
    (vertical-motion 1)
    (forward-char (min (- (org-draft--visual-eol-pos)
                          (org-draft--visual-bol-pos))
                       col))))

(defun org-draft-previous-line (&optional arg)
  (interactive "p")
  (setq arg (abs (or arg 1)))

  (let* ((bol (org-draft--visual-bol-pos))
         (col (- (point) bol)))
    (while (and (< 0 arg)
                (not (bobp)))

      (cond ((org-draft--on-page-break-p)
             (forward-char -1)
             (vertical-motion 0))

            ((org-draft--on-first-page-line-p)
             (vertical-motion 0)
             (forward-char -1)
             (vertical-motion 0))

            (t (let* ((pt (point)))
                 (vertical-motion -1)
                 (when (or (eq pt (point))
                           (eq bol (org-draft--visual-bol-pos)))
                   (vertical-motion -2)))))

      (setq arg (1- arg)))

    (forward-char (min (- (org-draft--visual-eol-pos)
                          (org-draft--visual-bol-pos))
                       col))))

(defun org-draft--beginning-of-line-ad (orig-fun &rest args)
  (let ((orig (point)))
    (apply orig-fun args)
    (when (and (bound-and-true-p org-draft-mode)
               (get-char-property (point) 'org-draft-indent))
      (forward-line)
      (when (= orig (point))
        (forward-line -1)))))

(defun org-draft--point-at-first-line-p ()
  (let* ((top (save-excursion (move-to-window-line 0) (point)))
         (lines (count-screen-lines top (save-excursion (move-to-window-line -1) (point)))))
    (<= (- lines (- lines (count-screen-lines top (point)))) 1)))

(defun org-draft--point-at-last-line-p ()
  (let ((top (save-excursion (move-to-window-line 0) (point))))
    (<= (- (count-screen-lines top (save-excursion (move-to-window-line -1) (point)))
           (count-screen-lines top (point)))
        1)))

(defun org-draft--fix-scroll-down-line-ad (orig-fun &rest args)
  (if (bound-and-true-p org-draft-mode)
      (let ((last-line-p (org-draft--point-at-last-line-p))
            (orig-pt (point))
            (this-scroll-margin
	     (min (max 0 scroll-margin)
		  (truncate (/ (window-body-height) 4.0)))))
        (move-to-window-line 0)
        (org-draft-previous-line)
        (recenter this-scroll-margin t)
        (if last-line-p
            (move-to-window-line -2)
          (goto-char orig-pt)))
    (apply orig-fun args)))

(defun org-draft--fix-scroll-up-line-ad (orig-fun &rest args)
  (if (bound-and-true-p org-draft-mode)
      (let ((first-line-p (org-draft--point-at-first-line-p))
            (orig-pt (point))
            (this-scroll-margin
	     (min (max 0 scroll-margin)
		  (truncate (/ (window-body-height) 4.0)))))
        (move-to-window-line 1)
        (recenter this-scroll-margin t)
        (if first-line-p
            (move-to-window-line 0)
          (goto-char orig-pt)))
    (apply orig-fun args)))


;;; Indentation

(defun org-draft-indent-buffer ()
  (interactive)
  (org-draft-indent-region (point-min) (point-max)))

(defun org-draft-indent-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\n+\\(\n\\)" end t)
      (unless (save-match-data (looking-at "\\*+ \\| *\- \\|#\\|\|\\|^:\\|INT\.\\|EXT\.\\|I/E\."))
        (org-draft--make-overlay
         (match-beginning 1) (match-end 1)
         'org-draft-indent t
         'evaporate t
         'display org-draft-indent-string)))))

(defun org-draft--auto-indent (beg end _len)
  (when (bound-and-true-p org-draft-mode)
    (let ((start (save-excursion
                   (goto-char beg)
                   (org-draft-goto-bol)
                   (skip-chars-backward "\n")
                   (point)))
          (stop (save-excursion
                  (goto-char end)
                  (org-draft-goto-eol)
                  (skip-chars-forward "\n")
                  (point))))
      (remove-overlays start stop 'org-draft-indent t)
      (org-draft-indent-region start stop))))


;;; Headings

(defun org-draft-format-headings-buffer ()
  (interactive)
  (org-draft-format-headings-region (point-min) (point-max)))

(defun org-draft-format-headings-region (beg end)
  (interactive "r")
  (when (or org-draft-padding-before-headings
            org-draft-hide-all-stars)
    (remove-overlays beg end 'org-draft-heading t)
    (save-excursion
      (goto-char beg)
      (when (and (org-at-heading-p)
                 (not (org-draft--at-inline-task-p)))
        (org-draft--format-heading))
      (while (and (outline-next-heading)
                  (< (point) end))
        (when (not (org-draft--at-inline-task-p))
          (org-draft--format-heading))))))

(defun org-draft--hide-heading-stars ()
  (let* ((beg (point))
         (end (progn (skip-chars-forward "* ") (point))))
    (org-draft--make-overlay
     beg end
     'org-draft-heading t
     'org-draft-stars-hidden t
     'evaporate t
     'invisible t)
    end))

(defun org-draft--add-heading-padding ()
  (org-draft--make-overlay
   (point) (1+ (point))
   'org-draft-heading t
   'org-draft-padding t
   'evaporate t
   'before-string (propertize "\n" 'face 'default 'line-height 0.8)))

(defun org-draft--format-heading ()
  (save-excursion
    (org-draft-goto-bol)
    (when org-draft-hide-all-stars
      (goto-char (org-draft--hide-heading-stars)))
    (when (and org-draft-padding-before-headings
               (not (looking-at "\n")))
      (org-draft--add-heading-padding))))

(defun org-draft--auto-format-headings (beg end _len)
  (when (bound-and-true-p org-draft-mode)
    (let ((beg (save-excursion (goto-char beg) (pos-bol)))
          (end (save-excursion (goto-char end) (pos-eol))))
      (org-draft-format-headings-region beg end))))

(defun org-draft--format-heading-from-org ()
  (when (bound-and-true-p org-draft-mode)
    (let ((beg (save-excursion (forward-line -1) (pos-bol)))
          (end (save-excursion (forward-line 1) (pos-eol))))
      (org-draft-format-headings-region beg end))))

(defun org-draft--at-inline-task-p ()
  (and (fboundp 'org-inlinetask-at-task-p)
       (fboundp 'org-inlinetask-end-p)
       (or (org-inlinetask-at-task-p)
           (org-inlinetask-end-p))))


;;; Page breaks

(defun org-draft-paginate-buffer ()
  (interactive)
  (setq org-draft--page-break-cache nil)
  (remove-overlays (point-min) (point-max) 'org-draft-page-break t)
  (org-draft--paginate-forward (current-buffer)))

(defvar-local org-draft--page-break-template nil)
(defvar-local org-draft--page-break-template-hard nil)
(defvar-local org-draft--page-break-current-number-length nil)

(defun org-draft--make-page-break-string (n)
  (let* ((page-num (number-to-string n))
         (nlen (length page-num)))
    (if (and org-draft--page-break-current-number-length
             (= org-draft--page-break-current-number-length nlen))
        (concat (car org-draft--page-break-template) page-num (cdr org-draft--page-break-template))
      (let* ((width (- (window-max-chars-per-line nil org-draft-page-break-face) 4))
             (base-str-len (/ width 2))
             (left-str (concat "\n"
                               (make-string base-str-len org-draft-soft-page-break-char)
                               "[ "))
             (right-str (concat " ]"
                                (make-string (- width (length left-str)) org-draft-soft-page-break-char)
                                "\n")))
        (setq org-draft--page-break-template `(,left-str . ,right-str)
              org-draft--page-break-current-number-length nlen)
        (concat left-str page-num right-str)))))

(defun org-draft--insert-page-break (page-num)
  (let ((ov (org-draft--make-overlay
             (point) (1+ (point))
             'org-draft-page-break t
             'org-draft-page-num page-num
             'evaporate t
             'after-string (propertize (org-draft--make-page-break-string page-num)
                                       'face 'org-draft-page-break-face
                                       'line-spacing org-draft-page-break-spacing))))
    (push ov org-draft--page-break-cache)))

(defun org-draft--on-page-break-p ()
  (unless (bobp) (get-char-property (1- (point)) 'org-draft-page-break)))

(defun org-draft--on-first-page-line-p ()
  (save-excursion
    (vertical-motion 0)
    (org-draft--on-page-break-p)))

(defun org-draft--rebuild-page-break-cache ()
  (when org-draft--page-break-cache
    (setq org-draft--page-break-cache
          (seq-filter (lambda (break)
                        (overlay-start break))
                      org-draft--page-break-cache)))

  (when org-draft--page-break-cache
    (setq org-draft--page-break-cache
          (seq-sort (lambda (lhs rhs)
                      (< (overlay-start lhs) (overlay-start rhs)))
                    org-draft--page-break-cache)))

  (not (seq-empty-p org-draft--page-break-cache)))

(defun org-draft--get-next-page-break-pos (pt)
  (when (org-draft--rebuild-page-break-cache)
    (let ((break (seq-find (lambda (break)
                             (>= (overlay-start break) pt))
                           org-draft--page-break-cache)))
      (when break (overlay-start break)))))

(defun org-draft--forward-page ()
  (interactive)
  (vertical-motion (+ 2 org-draft-lines-per-page)))

(defun org-draft--cancel-pagination-timer ()
  (when org-draft--idle-pagination-timer
    (cancel-timer org-draft--idle-pagination-timer)
    (setq org-draft--idle-pagination-timer nil)))

(defun org-draft--insert-last-page-break (page-num)
  (unless (org-draft--on-page-break-p)
    (forward-char -1)
    (org-draft--insert-page-break page-num)))

(defun org-draft--paginate-forward (buffer &optional chars-to-paginate)
  (when (eq buffer (current-buffer))
    (setq chars-to-paginate (or chars-to-paginate org-draft-partial-repagination-count))

    (let (start page-num)
      (if (org-draft--rebuild-page-break-cache)
          (let ((last-break (car (last org-draft--page-break-cache))))
            (setq page-num (1+ (overlay-get last-break 'org-draft-page-num))
                  start (overlay-start last-break)))
        (setq page-num 1
              start (point-min)))

      (save-excursion
        (let ((end (+ start chars-to-paginate)))
          (goto-char start)

          (when (= page-num 1)
            ;; Special case for first page: since there's no page break above, we
            ;; shouldn't add 2 to the line-count. Do it outside of the while loop,
            ;; so we don't have to keep checking the page number every iteration.
            (vertical-motion org-draft-lines-per-page)
            (unless (eobp)
              (org-draft-goto-eol)
              (org-draft--insert-page-break page-num)
              (setq page-num (1+ page-num))))

          (while (and (<= (point) end)
                      (not (eq 0 (org-draft--forward-page)))
                      (not (eobp)))
            (vertical-motion (cons (window-width) 0))
            (while (org-at-heading-p)
              (vertical-motion 1))
            (vertical-motion (cons (window-width) 0))
            (org-draft--insert-page-break page-num)
            (setq page-num (1+ page-num))))

        (org-draft--cancel-pagination-timer)

        (if (eobp)
            (org-draft--insert-last-page-break page-num)
          (setq org-draft--idle-pagination-timer
                (run-with-idle-timer 0.5 nil 'org-draft--paginate-forward (current-buffer))))))))

(defun org-draft--delete-expired-breaks-from-cache (beg-of-change next-break-idx)
  (when org-draft--page-break-cache
    (let* ((last-break-pos (overlay-start (car (last org-draft--page-break-cache))))
           (expired-breaks-idx
            (or (when (> beg-of-change last-break-pos) (1- (length org-draft--page-break-cache)))
                (when (and next-break-idx (> next-break-idx 0)) (1- next-break-idx))
                0)))
      (mapc (lambda (break)
              (delete-overlay break))
            (seq-subseq org-draft--page-break-cache expired-breaks-idx))
      (setq org-draft--page-break-cache (seq-take org-draft--page-break-cache expired-breaks-idx)))))

(defun org-draft--auto-repaginate (beg _end _len)
  (when (bound-and-true-p org-draft-mode)
    (let ((next-break-idx nil)
          (dont-repaginate nil))
      (when (and (org-draft--rebuild-page-break-cache)
                 (setq next-break-idx (seq-position org-draft--page-break-cache beg
                                                    (lambda (break beg)
                                                      (>= (overlay-end break) beg)))))
        (let ((page-num (overlay-get (nth next-break-idx org-draft--page-break-cache) 'org-draft-page-num))
              (start (if (= next-break-idx 0)
                         (point-min)
                       (overlay-start (nth (1- next-break-idx) org-draft--page-break-cache)))))
          (save-excursion
            (goto-char start)
            (if (= page-num 1)
                (vertical-motion org-draft-lines-per-page)
              (org-draft--forward-page))
            (org-draft-goto-eol)
            (setq dont-repaginate (eq page-num (get-char-property (point) 'org-draft-page-num))))))

      (unless dont-repaginate
        (org-draft--cancel-pagination-timer)
        (org-draft--delete-expired-breaks-from-cache beg next-break-idx)
        (org-draft--paginate-forward (current-buffer))
        (setq org-draft--idle-pagination-timer
              (run-with-idle-timer 0.5 nil 'org-draft--paginate-forward (current-buffer)))))))


;;; Mode definition:

(defvar org-draft-mode-map (make-sparse-keymap))

(defun org-draft--maybe-headings ()
  (when (or org-draft-padding-before-headings
            org-draft-hide-all-stars)
    (add-hook 'after-change-functions #'org-draft--auto-format-headings -10 t)
    (add-hook 'org-insert-heading-hook #'org-draft--format-heading-from-org 0 t)
    (add-hook 'org-after-demote-entry-hook #'org-draft--format-heading-from-org 0 t)
    (add-hook 'org-after-promote-entry-hook #'org-draft--format-heading-from-org 0 t)
    (org-draft-format-headings-buffer)))

(defun org-draft--maybe-indent ()
  (when org-draft-auto-indentation
    (add-hook 'after-change-functions #'org-draft--auto-indent 0 t)
    (org-draft-indent-buffer)))

(defun org-draft--maybe-paginate ()
  (when org-draft-auto-paginate
    (add-hook 'after-change-functions #'org-draft--auto-repaginate 10 t)
    (org-draft-paginate-buffer)))

;;;###autoload
(define-minor-mode org-draft-mode
  "Minor mode for writing with indented paragraphs."
  :lighter " draft"

  ;; (advice-add 'org-end-of-line :after #'org-draft-goto-eol)
  (advice-add 'org-beginning-of-line :around #'org-draft--beginning-of-line-ad)
  (advice-add 'scroll-up-line :around #'org-draft--fix-scroll-up-line-ad)
  (advice-add 'scroll-down-line :around #'org-draft--fix-scroll-down-line-ad)

  (keymap-set org-draft-mode-map "<remap> <org-end-of-line>" 'org-draft-goto-eol)
  (keymap-set org-draft-mode-map "<remap> <next-line>" 'org-draft-next-line)
  (keymap-set org-draft-mode-map "<remap> <_next-line>" 'org-draft-next-line)
  (keymap-set org-draft-mode-map "<remap> <previous-line>" 'org-draft-previous-line)
  (keymap-set org-draft-mode-map "<remap> <_previous-line>" 'org-draft-previous-line)

  (if org-draft-mode
      (progn (org-draft--maybe-headings)
             (org-draft--maybe-indent)
             (org-draft--maybe-paginate))

    (org-draft--cancel-pagination-timer)
    (remove-hook 'after-change-functions #'org-draft--auto-format-headings t)
    (remove-hook 'org-insert-heading-hook #'org-draft--format-heading-from-org t)
    (remove-hook 'org-after-demote-entry-hook #'org-draft--format-heading-from-org t)
    (remove-hook 'org-after-promote-entry-hook #'org-draft--format-heading-from-org t)
    (remove-hook 'after-change-functions #'org-draft--auto-indent t)
    (remove-hook 'after-change-functions #'org-draft--auto-repaginate t)
    (setq org-draft--page-break-template nil
          org-draft--page-break-template-hard nil
          org-draft--page-break-current-number-length nil)
    (org-draft-remove-overlays)))


;;; Utilities:

(defun org-draft--make-overlay (beg end &rest pairs)
  "Create overlay and add multiple properties at once."
  (let ((ov (make-overlay beg end nil t))
        (prop (pop pairs))
        (value (pop pairs)))
    (overlay-put ov 'org-draft-ov t)
    (while prop
      (overlay-put ov prop value)
      (setq prop (pop pairs)
            value (pop pairs)))
    ov))

(provide 'org-draft)

;;; org-draft.el ends here
