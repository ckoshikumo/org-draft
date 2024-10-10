;;; -*- lexical-binding: t; -*-

(require 'org)
(require 'seq)

(defgroup org-draft nil
  "Improve experience when using org-mode for writing long manuscripts."
  :prefix "org-draft-"
  :group 'convenience)

(defcustom org-draft-padding-before-headings nil
  "Non-nil if you want to add vertical whitespace before headings
without having to use newlines."
  :group 'org-draft
  :local t
  :type 'bool)

(defcustom org-draft-hide-all-stars nil
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

(defcustom org-draft-auto-pagination t
  "Non-nil to automatically paginate document."
  :group 'org-draft
  :local t
  :type 'bool)

(defcustom org-draft-padding-line-height 0.8
  "Float that will be used for the line-height of padding before
headings."
  :group 'org-draft
  :local t
  :type 'float)

(defcustom org-draft-indent-string "    "
  "The string to be used as indentation."
  :group 'org-draft
  :local t
  :type 'string)

(defcustom org-draft-indent-ignore-lines
  "\\*+ \\| *\- \\|#\\|\|\\|^:\\|INT\.\\|EXT\.\\|I/E\."
  "Regexp to match lines that should not be indented (the previous
blank line remains visible)."
  :group 'org-draft
  :local t
  :type 'regexp)

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

(defcustom org-draft-partial-repagination-count 50000
  "How many characters to repaginate each time."
  :group 'org-draft
  :type 'integer)

(defcustom org-draft-face 'org-draft-face
  "Which face to use for page breaks dividers."
  :group 'org-draft
  :type 'face)

(defface org-draft-face
  '((t :inherit 'fixed-pitch
       :foreground "gray50"))
  "Face for indentation and page breaks. Should be monospaced.")


;;; Internal variables:

(defvar-local org-draft--page-break-cache nil)
(defvar-local org-draft--soft-page-break-templates nil)
(defvar-local org-draft--hard-page-break-templates nil)
(defvar-local org-draft--idle-pagination-timer nil)
(defvar-local org-draft--scroll-margin nil)
(defvar-local org-draft--visibility nil)

(defmacro org-draft--with-visibility (beg end &rest body)
  (declare (indent 2))
  `(save-restriction
     (narrow-to-region ,beg ,end)
     (org-draft--save-visibility)
     (org-fold-show-all '(blocks headings))
     (unwind-protect
         (progn ,@body)
       (org-draft--restore-visibility))))


;;; Commands and fixes:

(defun org-draft-remove-overlays ()
  (interactive)
  (setq org-draft--page-break-cache nil)
  (remove-overlays (point-min) (point-max) 'org-draft-ov t))

(defun org-draft--simple-goto-bol ()
  (interactive)
  (vertical-motion 0)
  (point))

(defun org-draft--simple-goto-eol ()
  (interactive)
  (vertical-motion (cons (window-width) 0))
  (point))

(defun org-draft--get-next-page-break-pos (pt)
  (when (org-draft--rebuild-page-break-cache)
    (let ((break (seq-find (lambda (break)
                             (> (overlay-end break) pt))
                           org-draft--page-break-cache)))
      (when break (overlay-start break)))))

(defun org-draft--visual-eol-pos ()
  (let ((eol (save-excursion (org-draft--simple-goto-eol)))
        (break (or (org-draft--get-next-page-break-pos (point)) (point-max))))
    (min eol break)))

(defun org-draft--visual-bol-pos ()
  (if (and (org-draft--at-page-break-p)
           (not (eq (point) (org-draft--visual-eol-pos))))
      (point)
    (save-excursion
      (vertical-motion 0)
      (point))))

(defun org-draft-goto-eol ()
  (interactive)
  (let ((orig (point)))
    (goto-char (org-draft--visual-eol-pos))
    (when (and (eq (point) orig)
               (save-excursion (org-draft-goto-bol) (org-at-heading-p)))
      (org-end-of-line)))
  (point))

(defun org-draft-goto-bol ()
  (interactive)
  (goto-char (org-draft--visual-bol-pos))
  (point))

(defun org-draft-next-line (&optional arg)
  (interactive "p")
  (setq arg (abs (or arg 1)))

  (let* ((bol (org-draft--visual-bol-pos))
         (col (if (save-excursion
                    (goto-char bol)
                    (get-char-property (point) 'org-draft-indent))
                  0
                (- (point) bol))))
    (while (and (> arg 0)
                (not (eobp)))
      (vertical-motion 1)
      (setq arg (1- arg))
      (while (and (get-char-property (point) 'org-draft-page-break)
                  (not (eobp)))
        (forward-char 1)))

    (if (> arg 0)
        nil
      (let ((f (min (- (org-draft--visual-eol-pos)
                       (org-draft--visual-bol-pos))
                    col)))
        (unless (> 0 f)
          (forward-char f))
        t))
    (when (get-char-property (point) 'org-draft-indent)
      (forward-line 1))))

(defun org-draft-previous-line (&optional arg)
  (interactive "p")
  (setq arg (abs (or arg 1)))

  (let* ((bol (org-draft--visual-bol-pos))
         (col (if (save-excursion
                    (goto-char bol)
                    (get-char-property (point) 'org-draft-indent))
                  0
                (- (point) bol))))
    (while (and (< 0 arg)
                (not (bobp)))
      (setq arg (1- arg))

      (cond ((org-draft--at-page-break-p)
             (setq col 0)
             (forward-char -1)
             (while (and (org-invisible-p (point))
                         (not (bobp)))
               (org-draft--previous-heading)))

            ((and (org-draft--at-heading-p)
                  (save-excursion
                    (goto-char (pos-bol))
                    (org-draft--at-page-break-p)))
             (setq col (- (point) (pos-bol)))
             (goto-char (pos-bol))
             (forward-char -1))

            ((org-draft--at-first-page-line-p)
             (org-draft-goto-bol)
             (forward-char -1))

            (t (let ((pt (point)))
                 (vertical-motion -1)
                 (when (or (eq pt (point))
                           (eq bol (org-draft--visual-bol-pos)))
                   (vertical-motion -2))))))

    (let (skipped)
      (while (get-char-property (point) 'org-draft-page-break)
        (forward-char -1)
        (setq skipped t))
      (when skipped (forward-char -1)))

    (org-draft--simple-goto-bol)

    (let ((f (min (- (org-draft--visual-eol-pos)
                     (org-draft--visual-bol-pos))
                  col)))
      (unless (> 0 f)
        (forward-char f)))
    (when (get-char-property (point) 'org-draft-indent)
      (forward-line 1))))

(defvar-local org-draft--complex-heading-regexp nil)

(defun org-draft--make-complex-heading-regexp ()
  (setq org-draft--complex-heading-regexp
        (concat "\\(?:" org-todo-regexp "\\)?"
		"\\(?: +\\(\\[#.\\]\\)\\)?"
		"\\(?: +\\(.*?\\)\\)??"
		"\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
		"[ \t]*$")))

(defun org-draft--beginning-of-line-ad (orig-fun &rest args)
  (let ((orig (point)))
    (apply orig-fun args)
    (when (bound-and-true-p org-draft-mode)
      (when (get-char-property (point) 'org-draft-indent)
        (forward-line)
        (when (= orig (point))
          (forward-line -1)))

      (let ((case-fold-search nil) pos)
        (when (and (eq (point) (org-draft--visual-bol-pos))
                   (looking-at org-draft--complex-heading-regexp))
          (when (setq pos (or (match-end 2) (match-end 1)))
            (setq pos (1+ pos))
            (unless (eq orig pos)
              (goto-char pos))))))))

(defun org-draft--at-page-break-p ()
  (unless (bobp)
    (get-char-property (1- (point)) 'org-draft-page-break)))

(defun org-draft--at-first-screen-line-p ()
  (let* ((top (window-start))
         (lines (count-screen-lines top (save-excursion (move-to-window-line -1) (point)))))
    (<= (- lines (- lines (count-screen-lines top (point)))) 1)))

(defun org-draft--at-last-screen-line-p ()
  (let ((top (window-start)))
    (<= (- (count-screen-lines top (save-excursion (move-to-window-line -1) (point)))
           (count-screen-lines top (point)))
        1)))

(defun org-draft--at-first-page-line-p ()
  (or (save-excursion
        (org-draft--simple-goto-bol)
        (org-draft--at-page-break-p))
      (save-excursion
        (and (org-draft--at-heading-p)
             (progn (goto-char (pos-bol))
                    (org-draft--at-page-break-p))))))

(defun org-draft--at-last-page-line-p ()
  (save-excursion
    (org-draft-next-line)
    (org-draft--at-first-page-line-p)))

(defun org-draft--fix-scroll-down-line-ad (orig-fun &rest args)
  (if (bound-and-true-p org-draft-mode)
      (let ((orig-last-window-line-p (org-draft--at-last-screen-line-p))
            (orig-first-page-line-p (org-draft--at-first-page-line-p))
            (orig-pt (point))
            target-on-last-page-line-p
            target-on-first-page-line-p)
        (move-to-window-line 0)
        (org-draft-previous-line)
        (setq target-on-last-page-line-p (org-draft--at-last-page-line-p)
              target-on-first-page-line-p (org-draft--at-first-page-line-p))
        (recenter org-draft--scroll-margin t)

        (if orig-last-window-line-p
            (cond ((or target-on-first-page-line-p target-on-last-page-line-p)
                   (move-to-window-line -4))
                  (orig-first-page-line-p
                   (move-to-window-line -3))
                  (t (move-to-window-line -2)))
          (goto-char orig-pt)))
    (apply orig-fun args))
  (when (get-char-property (point) 'org-draft-indent)
    (forward-line 1)))

(defun org-draft--fix-scroll-up-line-ad (orig-fun &rest args)
  (interactive)
  (if (bound-and-true-p org-draft-mode)
      (let ((orig-pt (point))
            (orig-first-screen-line-p (org-draft--at-first-screen-line-p))
            (orig-first-page-line-p (org-draft--at-first-screen-line-p))
            first-screen-line-p
            first-page-line-p)
        (save-excursion
          (move-to-window-line 0)
          (setq first-screen-line-p (org-draft--at-first-screen-line-p)
                first-page-line-p (org-draft--at-first-page-line-p)))
        (cond ((and first-screen-line-p
                    first-page-line-p
                    (not orig-first-page-line-p))
               (move-to-window-line 2))
              (first-page-line-p
               (move-to-window-line 1))
              (t (move-to-window-line 1)))
        (recenter org-draft--scroll-margin t)
        (if (not orig-first-screen-line-p)
            (goto-char orig-pt)))
    (apply orig-fun args))
  (when (get-char-property (point) 'org-draft-indent)
    (forward-line 1)))

(defun org-draft--keyboard-quit-ad (&rest _args)
  (when (and (bound-and-true-p org-draft-mode)
             org-draft--idle-pagination-timer)
    (cancel-timer org-draft--idle-pagination-timer)
    (setq org-draft--idle-pagination-timer
          (run-with-idle-timer 0.5 nil 'org-draft--paginate-forward (current-buffer)))))


;;; Indentation

(defun org-draft-indent-buffer ()
  (interactive)
  (org-draft-indent-region (point-min) (point-max)))

(defun org-draft-indent-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\n+\\(\n\\)" end t)
      (unless (save-match-data (looking-at org-draft-indent-ignore-lines))
        (org-draft--make-overlay
         (match-beginning 1) (match-end 1)
         'org-draft-indent t
         'evaporate t
         'display (propertize org-draft-indent-string 'face 'org-draft-face))))))

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

(defvar-local org-draft--formatting-headers nil)

(defun org-draft-format-headings-buffer ()
  (interactive)
  (org-draft-format-headings-region (point-min) (point-max)))

(defun org-draft-format-headings-region (beg end)
  (interactive "r")
  (when (or org-draft-padding-before-headings
            org-draft-hide-all-stars)
    (save-excursion
      (goto-char beg)

      ;; We only need to check if we're at at a heading in the first case:
      (when (and (org-at-heading-p)
                 (not (org-draft--at-inline-task-p)))
        (org-draft--format-heading))

      (outline-next-heading)
      (while (< (point) end)
        (unless (org-draft--at-inline-task-p)
          (org-draft--format-heading))
        (outline-next-heading)))))

(defun org-draft--format-heading ()
  (remove-overlays (pos-bol) (pos-eol) 'org-draft-heading t)
  (save-excursion
    (goto-char (pos-bol))
    (when org-draft-hide-all-stars
      (let* ((beg (point))
             (end (progn (skip-chars-forward "* ") (point))))
        (org-draft--make-overlay
         beg end
         'org-draft-heading t
         'org-draft-stars-hidden t
         'evaporate t
         'display "")
        (goto-char end)))

    (when (and org-draft-padding-before-headings
               (not (looking-at "\n")))
      (org-draft--make-overlay
       (point) (1+ (point))
       'org-draft-heading t
       'org-draft-padding t
       'evaporate t
       'before-string (propertize "\n" 'face 'default 'line-height org-draft-padding-line-height)))))

(defun org-draft--auto-format-headings (beg end _len)
  (when (bound-and-true-p org-draft-mode)
    (save-excursion
      (let ((start (progn (goto-char beg) (org-draft-goto-bol)))
            (stop (progn (goto-char end) (org-draft-goto-eol))))
        (org-draft-format-headings-region start stop)))))

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
  (org-draft--make-page-break-templates)
  (org-draft--paginate-forward (current-buffer)))

(defun org-draft--rebuild-page-break-cache ()
  (let (res)
    (when org-draft--page-break-cache
      (setq org-draft--page-break-cache
            (seq-filter (lambda (break)
                          (overlay-end break))
                        org-draft--page-break-cache)))

    (when org-draft--page-break-cache
      (setq res t
            org-draft--page-break-cache
            (seq-sort (lambda (lhs rhs)
                        (< (overlay-end lhs) (overlay-end rhs)))
                      org-draft--page-break-cache)))
    res))

(defun org-draft--skip-page-lines ()
  (let ((res (not (eq 0 (vertical-motion org-draft-lines-per-page)))))
    (while (and (org-at-heading-p)
                (not (eq (pos-eol) (point-max))))
      (vertical-motion 1))
    res))

(defun org-draft--cancel-pagination-timer ()
  (when org-draft--idle-pagination-timer
    (cancel-timer org-draft--idle-pagination-timer)
    (setq org-draft--idle-pagination-timer nil)))

(defun org-draft--get-pagination-start-pos ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward "# pagination start" nil t)
        (progn (org-draft-next-line)
               (org-draft-goto-bol)
               (skip-chars-forward "\n")))
      (point))))

(defun org-draft--make-page-break-templates ()
  (unless org-draft--soft-page-break-templates
    (dotimes (i 5)
      (let* ((n (1+ i))
             (width (- fill-column 4 n))
             (base-str-len (/ width 2))

             (left-str-soft (concat "\n" (make-string
                                          base-str-len
                                          org-draft-soft-page-break-char)
                                    "[ "))
             (right-str-soft (concat " ]" (make-string
                                           (- width (length left-str-soft))
                                           org-draft-soft-page-break-char)
                                     "\n")))
        (push (concat left-str-soft "%s" right-str-soft) org-draft--soft-page-break-templates)))

    (setq org-draft--soft-page-break-templates (nreverse org-draft--soft-page-break-templates)
          org-draft--hard-page-break-templates
          (mapcar (lambda (str)
                    (string-replace (char-to-string org-draft-soft-page-break-char)
                                    (char-to-string org-draft-hard-page-break-char)
                                    str))
                  org-draft--soft-page-break-templates))))

(defun org-draft--make-soft-page-break-string (n)
  (let* ((page-num (number-to-string n))
         (nlen (length page-num)))
    (format (nth nlen org-draft--soft-page-break-templates) page-num)))

(defun org-draft--insert-page-break (page-num)
  (let ((ov (org-draft--make-overlay
             (point) (1+ (point))
             'org-draft-page-break t
             'org-draft-page-num page-num
             'evaporate t
             'after-string (propertize (org-draft--make-soft-page-break-string page-num)
                                       'face 'org-draft-face
                                       'line-spacing org-draft-page-break-spacing))))
    (push ov org-draft--page-break-cache))
  (unless (eobp)
    (forward-char 1)))

(defun org-draft--at-heading-p ()
  (save-excursion
    (goto-char (pos-bol))
    (looking-at outline-regexp)))

(defun org-draft--org-previous-visible-heading-ad (&rest _args)
  (when (bound-and-true-p org-draft-mode)
    (while (and (not (bobp))
                (get-char-property (point) 'org-draft-stars-hidden))
      (forward-char 1))
    (point)))

(defun org-draft--previous-heading (&optional arg)
  (setq arg (or arg 1))
  (while (and (> arg 0)
              (not (bobp)))
    (when (org-draft--at-heading-p)
      (forward-line -1))
    (if (re-search-backward "^*+ " nil t)
        (unless (org-draft--at-inline-task-p)
          (setq arg (1- arg)))
      (goto-char (point-min))
      (setq arg 0)))
  (when (org-draft--at-heading-p)
    (org-draft-goto-bol))
  (point))

(defun org-draft--next-heading (&optional arg)
  (setq arg (or arg 1))
  (while (and (> arg 0)
              (not (eobp)))
    (when (org-draft--at-heading-p)
      (forward-line 1))
    (if (re-search-forward "^*+ " nil t)
        (progn (unless (org-draft--at-inline-task-p)
                 (setq arg (1- arg)))
               (unless (= arg 0)
                 (org-draft-next-line 1)))
      (goto-char (point-max))
      (setq arg 0)))
  (when (org-draft--at-heading-p)
    (org-draft-goto-bol))
  (point))

(defun org-draft--save-visibility ()
  (save-excursion
    (goto-char (point-min))
    (unless (org-draft--at-heading-p)
      (org-draft--next-heading))
    (let ((count 0))
      (while (and (not (eobp))
                  (not (eq (pos-eol) (point-max)))
                  (org-draft--at-heading-p)
                  (< count 100))
        (setq count (1+ count))
        (push `(:point ,(point)
                :heading-inv ,(invisible-p (point))
                :entry-inv ,(save-excursion
                              (forward-line 1)
                              (org-fold-folded-p (point) (org-fold-core-folding-spec-list))))
              org-draft--visibility)
        (org-draft--next-heading)))
    (setq org-draft--visibility (nreverse org-draft--visibility))))

(defun org-draft--restore-visibility ()
  (when org-draft--visibility
    (save-excursion
      (mapc (lambda (entry)
              (goto-char (plist-get entry :point))
              (when (plist-get entry :entry-inv) (org-fold-hide-entry))
              (when (plist-get entry :heading-inv) (org-fold-heading t)))
            org-draft--visibility))
    (setq org-draft--visibility nil)))

(defun org-draft--delete-expired-breaks-from-cache (beg-of-change next-break-idx)
  (when org-draft--page-break-cache
    (let* ((last-break-pos (overlay-end (car (last org-draft--page-break-cache))))
           (expired-breaks-idx
            (or (when (> beg-of-change last-break-pos) (1- (length org-draft--page-break-cache)))
                (when (and next-break-idx (> next-break-idx 0)) next-break-idx)
                0)))
      (mapc (lambda (break)
              (delete-overlay break))
            (seq-subseq org-draft--page-break-cache expired-breaks-idx))
      (setq org-draft--page-break-cache (seq-take org-draft--page-break-cache expired-breaks-idx)))))

(defun org-draft--paginate-forward (buffer &optional chars-to-paginate)
  (when (eq buffer (current-buffer))
    (org-draft--cancel-pagination-timer)

    (setq chars-to-paginate (or chars-to-paginate org-draft-partial-repagination-count))

    (let (beg page-num end last)
      (if (org-draft--rebuild-page-break-cache)
          (let ((last-break (car (last org-draft--page-break-cache))))
            (setq page-num (1+ (overlay-get last-break 'org-draft-page-num))
                  beg (overlay-end last-break)))
        (setq page-num 1
              beg (org-draft--get-pagination-start-pos)))

      (setq end (min (point-max) (+ beg chars-to-paginate))
            last (eq end (point-max)))

      (save-excursion
        (let ((beg-vis (progn (goto-char beg)
                              (org-draft--previous-heading)
                              (unless (bobp) (forward-line -1))
                              (point)))
              (end-vis (progn (goto-char end)
                              (org-draft--next-heading))))

          (org-draft--with-visibility beg-vis end-vis
            (goto-char beg)
            (while (and (org-draft--skip-page-lines)
                        (not (eobp)))
              (org-draft--simple-goto-eol)
              (org-draft--insert-page-break page-num)
              (setq page-num (1+ page-num)))))

        (if last
            (unless (or (org-draft--at-first-page-line-p)
                        (org-draft--at-last-page-line-p))
              (forward-char -1)
              (org-draft--insert-page-break page-num))

          (setq org-draft--idle-pagination-timer
                (run-with-idle-timer 0.5 nil 'org-draft--paginate-forward (current-buffer))))))))

(defun org-draft--auto-repaginate (beg end _len)
  (when (bound-and-true-p org-draft-mode)
    (let ((beg (min beg end))
          next-break-idx dont-repaginate)

      (save-excursion
        (when (and (org-draft--rebuild-page-break-cache)
                   (setq next-break-idx (seq-position org-draft--page-break-cache beg
                                                      (lambda (break beg)
                                                        (>= (overlay-end break) beg)))))

          (let* ((page-num (overlay-get (nth next-break-idx org-draft--page-break-cache) 'org-draft-page-num))
                 (start (if (= next-break-idx 0)
                            (org-draft--get-pagination-start-pos)
                          (overlay-end (nth (1- next-break-idx) org-draft--page-break-cache))))
                 (ov-stop (overlay-end (nth next-break-idx org-draft--page-break-cache)))
                 (stop (min (point-max) (1+ ov-stop)))
                 (beg-vis (progn (goto-char start)
                                 (org-draft--previous-heading)
                                 (unless (bobp) (forward-line -1))
                                 (point)))
                 (end-vis (progn (goto-char stop)
                                 (org-draft--next-heading))))

            (org-draft--with-visibility beg-vis end-vis
              (goto-char start)
              (org-draft--skip-page-lines)
              (setq dont-repaginate
                    (eq page-num (get-char-property (org-draft--visual-eol-pos) 'org-draft-page-num)))))))

      (unless dont-repaginate
        (org-draft--delete-expired-breaks-from-cache beg next-break-idx)
        (org-draft--make-page-break-templates)
        (org-draft--paginate-forward (current-buffer))))))


;;; Mode definition:

(defvar org-draft-mode-map (make-sparse-keymap))
(setq org-draft-mode-map (make-sparse-keymap))

(defun org-draft--maybe-headings ()
  (when (or org-draft-padding-before-headings
            org-draft-hide-all-stars)
    (add-hook 'after-change-functions #'org-draft--auto-format-headings -10 t)
    (org-draft-format-headings-buffer)))

(defun org-draft--maybe-indent ()
  (when org-draft-auto-indentation
    (add-hook 'after-change-functions #'org-draft--auto-indent -10 t)
    (org-draft-indent-buffer)))

(defun org-draft--maybe-paginate ()
  (when org-draft-auto-pagination
    (add-hook 'after-change-functions #'org-draft--auto-repaginate 0 t)
    (org-draft-paginate-buffer)))

(defun org-draft--org-todo-ad (orig-fun &rest args)
  (when (bound-and-true-p org-draft-mode)
    (let ((inhibit-modification-hooks t))
      (apply orig-fun args)
      (org-draft--format-heading))))

;;;###autoload
(define-minor-mode org-draft-mode
  "Minor mode for writing with indented paragraphs."
  :lighter " draft"

  (advice-add 'org-todo :around #'org-draft--org-todo-ad)
  (advice-add 'org-beginning-of-line :around #'org-draft--beginning-of-line-ad)
  (advice-add 'scroll-up-line :around #'org-draft--fix-scroll-up-line-ad)
  (advice-add 'scroll-down-line :around #'org-draft--fix-scroll-down-line-ad)
  (advice-add 'org-previous-visible-heading :after #'org-draft--org-previous-visible-heading-ad)
  (advice-add 'keyboard-quit :after #'org-draft--keyboard-quit-ad)
  (when (fboundp '_keyboard-quit)
    (advice-add '_keyboard-quit :after #'org-draft--keyboard-quit-ad))

  (keymap-set org-draft-mode-map "<remap> <org-end-of-line>" 'org-draft-goto-eol)
  (keymap-set org-draft-mode-map "<remap> <next-line>" 'org-draft-next-line)
  (keymap-set org-draft-mode-map "<remap> <_next-line>" 'org-draft-next-line)
  (keymap-set org-draft-mode-map "<remap> <previous-line>" 'org-draft-previous-line)
  (keymap-set org-draft-mode-map "<remap> <_previous-line>" 'org-draft-previous-line)

  (if org-draft-mode
      (progn (setq org-draft--scroll-margin (min (max 0 scroll-margin)
		                                 (truncate (/ (window-body-height) 4.0))))
             (org-draft--make-complex-heading-regexp)
             (org-draft--maybe-headings)
             (org-draft--maybe-indent)
             (org-draft--maybe-paginate))

    (org-draft--cancel-pagination-timer)
    (remove-hook 'after-change-functions #'org-draft--auto-format-headings t)
    (remove-hook 'after-change-functions #'org-draft--auto-indent t)
    (remove-hook 'after-change-functions #'org-draft--auto-repaginate t)
    (setq org-draft--soft-page-break-templates nil
          org-draft--hard-page-break-templates nil
          org-draft--visibility nil
          org-draft--complex-heading-regexp nil)
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
