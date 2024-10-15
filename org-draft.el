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

(defcustom org-draft-soft-page-break-char ?-
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

(defcustom org-draft-keep-column-commands
  '( org-draft-next-line org-draft-previous-line
     scroll-up-line scroll-down-line
     _scroll-line-down _scroll-line-up )
  "Commands that will keep point at target col."
  :group 'org-draft
  :type '(repeat function))

(defface org-draft-face
  '((t :inherit fixed-pitch
       :foreground "gray50"))
  "Face for indentation and page breaks. Should be monospaced.")


;;; Internal variables:

(defvar-local org-draft--scroll-margin nil)
(defvar-local org-draft--window-width nil)
(defvar-local org-draft--page-break-cache nil)
(defvar-local org-draft--page-break-cache-reverse nil)
(defvar-local org-draft--col-cache nil)
(defvar-local org-draft--soft-page-break-templates nil)
(defvar-local org-draft--hard-page-break-templates nil)
(defvar-local org-draft--visibility nil)
(defvar-local org-draft--complex-heading-regexp nil)
(defvar-local org-draft--auto-repagination-change-pt nil)
(defvar-local org-draft--auto-repagination-idle-timer nil)
(defvar-local org-draft--keep-paginating-idle-timer nil)
(defvar-local org-draft--update-page-breaks-idle-timer nil)

(defun teste ()
  (interactive)
  (outline-next-heading)
  (message "%s" (and (not (eobp)) (not (org-inlinetask-end-p)))))

(defmacro org-draft--with-visibility (beg end &rest body)
  (declare (indent 2))
  `(save-restriction
     (let ((inhibit-redisplay nil)
           (beg-vis (progn (goto-char ,beg)
                           (outline-previous-heading)
                           (org-draft--goto-heading-bol)))
           (end-vis (progn (goto-char ,end)
                           (outline-next-heading)
                           (1- (org-draft--goto-heading-bol)))))
       (narrow-to-region beg-vis end-vis)
       (org-draft--save-visibility)
       (org-fold-show-all '(blocks headings))
       (unwind-protect
           (progn ,@body)
         (org-draft--restore-visibility)))))

(defun org-draft--save-visibility ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (org-draft--at-heading-p)
      (outline-next-heading))
    (let ((inline-task-fn (if (fboundp 'org-inlinetask-end-p) 'org-inlinetask-end-p 'ignore)))
      (while (not (eobp))
        (unless (apply inline-task-fn nil)
          (push `(:point ,(point)
                  :text ,(buffer-substring-no-properties (pos-bol) (pos-eol))
                  :heading-inv ,(invisible-p (point))
                  :entry-inv ,(save-excursion
                                (forward-line 1)
                                (org-fold-folded-p (point) (org-fold-core-folding-spec-list))))
                org-draft--visibility))
        (outline-next-heading)))
    (setq org-draft--visibility (nreverse org-draft--visibility))))

(defun org-draft--restore-visibility ()
  (interactive)
  (when org-draft--visibility
    (save-excursion
      (mapc (lambda (entry)
              (goto-char (plist-get entry :point))
              (if (plist-get entry :entry-inv)
                  (org-fold-hide-entry)
                (org-fold-show-entry))
              (org-fold-heading (plist-get entry :heading-inv)))
            org-draft--visibility))
    (setq org-draft--visibility nil)))


;;; Commands and fixes:

(defun org-draft-remove-overlays ()
  (interactive)
  (setq org-draft--page-break-cache nil)
  (remove-overlays (point-min) (point-max) 'org-draft-ov t))

(defun org-draft-goto-eol ()
  (interactive)
  (let ((orig (point)))
    (goto-char (org-draft--visual-eol-pos))
    (when (and (eq (point) orig)
               (org-at-heading-p))
      (org-end-of-line)))
  (point))

(defun org-draft-goto-bol ()
  (interactive)
  (let* ((orig-pt (point))
         (bol (org-draft--visual-bol-pos))
         (after-indent-p (and (not (bobp))
                              (org-draft--prop-p 'org-draft-indent (1- (point)))))
         (case-fold-search nil) pos)

    (goto-char bol)

    (when (not after-indent-p)
      (org-draft--adjust-indent))

    (when (and (looking-at org-draft--complex-heading-regexp))
      (when (setq pos (or (match-end 2) (match-end 1)))
        (setq pos (1+ pos))
        (unless (eq orig-pt pos)
          (goto-char pos)))))

  (point))

(defun org-draft-next-line (&optional arg)
  (interactive "p")
  (setq arg (abs (or arg 1)))

  (while (> arg 0)
    (setq arg (1- arg))
    (cond ((org-draft--prop-p 'org-draft-page-break (point)))
          ((org-draft--at-heading-p)
           (goto-char (pos-eol)))
          (t (org-draft-goto-eol)))

    (if (eobp)
        (setq arg 0)

      (forward-char 1)
      (let ((pt (point)))
        (while (and (org-invisible-p)
                    (not (eobp)))
          (outline-next-heading))
        (when (and (not (eobp))
                   (not (looking-at "^\n"))
                   (not (eq pt (save-excursion
                                 (forward-char 1)
                                 (org-draft--simple-goto-bol)))))
          (forward-char 1)))))

  (org-draft--simple-goto-bol)
  (org-draft--forward-col)
  (org-draft--adjust-indent))

(defun org-draft-previous-line (&optional arg)
  (interactive "p")
  (setq arg (abs (or arg 1)))

  (while (> arg 0)
    (cond ((org-draft--at-heading-p)
           (goto-char (pos-bol)))
          (t (org-draft--simple-goto-bol)))

    (forward-char -1)
    (setq arg (if (bobp) 0 (1- arg))))

  (org-draft--simple-goto-bol)
  (org-draft--forward-col)
  (org-draft--adjust-indent))

(modality-set-keys "M-n" #'teste)


;;; Utilities:

(defun org-draft--simple-goto-bol ()
  (interactive)
  (vertical-motion 0)
  (point))

(defun org-draft--simple-goto-eol ()
  (vertical-motion (cons (window-width) 0))
  (point))

(defun org-draft--top-pos ()
  (save-excursion
    (move-to-window-line 0)
    (org-draft--simple-goto-bol)))

(defun org-draft--bottom-pos ()
  (save-excursion
    (move-to-window-line -1)
    (org-draft--simple-goto-bol)))

(defun org-draft--get-next-page-break (pt)
  (let* ((idx -1)
         (break (seq-find (lambda (break)
                            (setq idx (1+ idx))
                            (and (overlay-end break)
                                 (> (overlay-end break) pt)))
                          org-draft--page-break-cache)))
    (when break `(,break . ,idx))))

(defun org-draft--get-prev-page-break (pt)
  (let* ((idx -1)
         (break (seq-find (lambda (break)
                            (setq idx (1+ idx))
                            (and (overlay-end break)
                                 (< (overlay-end break) pt)))
                          org-draft--page-break-cache-reverse)))
    (when break `(,break . ,idx))))

(defun org-draft--get-next-page-break-start-pos (pt)
  (let ((break (org-draft--get-next-page-break pt)))
    (when break (overlay-start (car break)))))

(defun org-draft--visual-eol-pos ()
  (let ((eol (save-excursion (org-draft--simple-goto-eol)))
        (break (or (org-draft--get-next-page-break-start-pos (point))
                   (point-max))))
    (min eol break)))

(defun org-draft--visual-bol-pos ()
  (if (and (org-draft--at-page-break-p)
           (not (eq (point) (org-draft--visual-eol-pos))))
      (point)
    (save-excursion
      (org-draft--simple-goto-bol))))

(defun org-draft--prop-p (prop &optional pos)
  (get-char-property (or pos (point)) prop))

(defun org-draft--at-page-break-p ()
  (unless (bobp)
    (org-draft--prop-p 'org-draft-page-break (1- (point)))))

(defun org-draft--goto-heading-bol ()
  (goto-char (pos-bol))
  (when (and (org-draft--at-page-break-p)
             (not (eobp)))
    (forward-char 1))
  (point))

(defun org-draft--at-heading-p ()
  (save-excursion
    (org-draft--goto-heading-bol)
    (when (looking-at outline-regexp)
      (point))))

(defun org-draft--at-inline-task-p ()
  (and (fboundp 'org-inlinetask-at-task-p)
       (fboundp 'org-inlinetask-end-p)
       (or (org-inlinetask-at-task-p)
           (org-inlinetask-end-p))))

(defun org-draft--current-screen-line ()
  (save-excursion
    (let ((orig-bol (or (org-draft--at-heading-p)
                        (org-draft--visual-bol-pos)))
          (top-bol (save-excursion
                     (move-to-window-line 0)
                     (or (org-draft--at-heading-p)
                         (org-draft--visual-bol-pos)))))
      (count-screen-lines orig-bol top-bol))))

(defun org-draft--at-first-screen-line-p ()
  (eq 0 (org-draft--current-screen-line)))

(defun org-draft--at-last-screen-line-p ()
  (let* ((orig-bol (or (org-draft--at-heading-p)
                       (org-draft--visual-bol-pos)))
         (bottom-bol (save-excursion
                       (move-to-window-line -1)
                       (or (org-draft--at-heading-p)
                           (org-draft--visual-bol-pos)))))
    (<= 1 (count-screen-lines orig-bol bottom-bol))))

(defun org-draft--at-first-page-line-p ()
  (save-excursion
    (goto-char (or (org-draft--at-heading-p) (org-draft--simple-goto-bol)))
    (org-draft--at-page-break-p)))

(defun org-draft--at-last-page-line-p ()
  (save-excursion
    (if (org-draft--at-heading-p)
        (goto-char (pos-eol))
      (org-draft-goto-eol))
    (org-draft--prop-p 'org-draft-page-break (point))))

(defun org-draft--make-complex-heading-regexp ()
  (setq org-draft--complex-heading-regexp
        (concat "\\(?:" org-todo-regexp "\\)?"
		"\\(?: +\\(\\[#.\\]\\)\\)?"
		"\\(?: +\\(.*?\\)\\)??"
		"\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?"
		"[ \t]*$")))

(defun org-draft--adjust-indent ()
  (when (and (org-draft--prop-p 'org-draft-indent)
             (not (eq (point) (org-draft--visual-eol-pos))))
    (forward-char 1)))

(defun org-draft--forward-col ()
  (ignore-errors
    (forward-char (min (or org-draft--col-cache (point-max))
                       (- (org-draft--visual-eol-pos)
                          (org-draft--visual-bol-pos))))))

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

(defun org-draft--org-previous-visible-heading-ad (&rest _args)
  (when (bound-and-true-p org-draft-mode)
    (while (and (not (bobp))
                (org-draft--prop-p 'org-draft-stars-hidden))
      (forward-char 1))
    (point)))

(defun org-draft--fix-scroll-up-line-ad (orig-fun &rest args)
  (if (not (bound-and-true-p org-draft-mode))
      (apply orig-fun args)
    (let* ((orig-pt (point))
           (orig-first-screen-line-p (org-draft--at-first-screen-line-p)))

      (move-to-window-line 0)
      (org-draft-next-line)
      (recenter org-draft--scroll-margin t)

      (if (not orig-first-screen-line-p)
          (goto-char orig-pt)
        (move-to-window-line 0)
        (org-draft--adjust-indent)
        (org-draft--forward-col)))))

(defun org-draft--fix-scroll-down-line-ad (orig-fun &rest args)
  (if (not (bound-and-true-p org-draft-mode))
      (apply orig-fun args)
    (let* ((orig (point))
           (final (progn (move-to-window-line -1)
                         (org-draft-previous-line 4)
                         (min orig (point)))))
      (move-to-window-line 0)
      (unless (bobp)
        (org-draft-previous-line))
      (recenter org-draft--scroll-margin t)
      (goto-char final))))

(defun org-draft--keyboard-quit-ad (&rest _args)
  (when (and (bound-and-true-p org-draft-mode)
             org-draft--keep-paginating-idle-timer)
    (cancel-timer org-draft--keep-paginating-idle-timer)
    (setq org-draft--keep-paginating-idle-timer
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
         'display (propertize org-draft-indent-string 'face org-draft-face))))))

(defun org-draft--auto-indent (beg end _len)
  (when (bound-and-true-p org-draft-mode)
    (let ((start (save-excursion
                   (goto-char beg)
                   (org-draft-goto-bol)
                   (skip-chars-backward "\n ")
                   (point)))
          (stop (save-excursion
                  (goto-char end)
                  (org-draft-goto-eol)
                  (skip-chars-forward "\n ")
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
    (save-excursion
      (goto-char beg)
      ;; We only need to check if we're at at a heading in the first case:
      (when (and (org-draft--at-heading-p)
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
    (when (and org-draft-hide-all-stars
               (not (org-draft--at-inline-task-p)))
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
      (let ((start (progn (goto-char beg)
                          (unless (bobp)
                            (org-draft-previous-line))
                          (org-draft--visual-bol-pos)))
            (stop (progn (goto-char end)
                         (unless (eobp)
                           (org-draft-next-line))
                         (org-draft--visual-eol-pos))))
        (org-draft-format-headings-region start stop)))))


;;; Page breaks

(defvar-local org-draft--must-rebuild-page-break-cache nil)

(defun org-draft--rebuild-page-break-cache ()
  (when org-draft--page-break-cache
    (setq org-draft--page-break-cache
          (seq-filter (lambda (break)
                        (overlay-end break))
                      org-draft--page-break-cache)))

  (when org-draft--page-break-cache
    (setq org-draft--page-break-cache
          (seq-sort (lambda (lhs rhs)
                      (< (overlay-end lhs) (overlay-end rhs)))
                    org-draft--page-break-cache))
    (setq org-draft--page-break-cache-reverse (reverse org-draft--page-break-cache))
    t))

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
      (setq org-draft--page-break-cache (seq-take org-draft--page-break-cache expired-breaks-idx))
      (setq org-draft--page-break-cache-reverse (reverse org-draft--page-break-cache)))))

(defun org-draft--skip-page-lines ()
  (let ((res (not (eq 0 (vertical-motion org-draft-lines-per-page)))))
    (while (and (org-at-heading-p)
                (not (eq (pos-eol) (point-max))))
      (vertical-motion 1))
    res))

(defun org-draft--cancel-pagination-timer ()
  (when org-draft--keep-paginating-idle-timer
    (cancel-timer org-draft--keep-paginating-idle-timer)
    (setq org-draft--keep-paginating-idle-timer nil)))

(defun org-draft--get-pagination-start-pos ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward "# start pagination" nil t)
        (progn (org-draft-next-line)
               (org-draft-goto-bol)
               (skip-chars-forward "\n"))))
    (point)))

(defun org-draft--make-page-break-templates (&optional force)
  (when (or (not org-draft--soft-page-break-templates)
            force)
    (let ((win-width (or org-draft--window-width
                         (window-max-chars-per-line (get-buffer-window) org-draft-face))))
      (setq org-draft--soft-page-break-templates nil)
      (dotimes (i 5)
        (let* ((n (1+ i))
               (width (- win-width n))
               (base-str-len (/ width 2))

               (left-str-soft (concat "\n" (make-string
                                            base-str-len
                                            org-draft-soft-page-break-char)
                                      "[ "))
               (right-str-soft (concat " ]" (make-string
                                             (- width (length left-str-soft))
                                             org-draft-soft-page-break-char)
                                       "\n")))
          (push (concat left-str-soft "%s" right-str-soft) org-draft--soft-page-break-templates))))

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
             'after-string (propertize
                            (org-draft--make-soft-page-break-string page-num)
                            'face org-draft-face
                            'line-spacing org-draft-page-break-spacing))))
    (push ov org-draft--page-break-cache))
  (unless (eobp) (forward-char 1)))

(defun org-draft--update-page-breaks (buffer)
  (when (eq buffer (current-buffer))
    (cancel-timer org-draft--update-page-breaks-idle-timer)
    (setq org-draft--update-page-breaks-idle-timer nil))

  (let ((win-width (window-max-chars-per-line (get-buffer-window) org-draft-face)))
    (when (and (wholenump win-width)
               (not (eq win-width org-draft--window-width)))
      (setq org-draft--window-width win-width)
      (org-draft--make-page-break-templates 'force)
      (mapc (lambda (break)
              (let ((page-num (overlay-get break 'org-draft-page-num)))
                (overlay-put break 'after-string (propertize (org-draft--make-soft-page-break-string page-num)
                                                             'face org-draft-face
                                                             'line-spacing org-draft-page-break-spacing))))
            org-draft--page-break-cache))))

(defun org-draft--schedule-update-page-breaks ()
  (when (bound-and-true-p org-draft-mode)
    (when org-draft--update-page-breaks-idle-timer
      (cancel-timer org-draft--update-page-breaks-idle-timer))
    (setq org-draft--update-page-breaks-idle-timer
          (run-with-idle-timer 0.5 nil #'org-draft--update-page-breaks (current-buffer)))))

(defun org-draft-paginate-buffer ()
  (interactive)
  (setq org-draft--page-break-cache nil)
  (setq org-draft--auto-repagination-change-pt (point-min))
  (remove-overlays (point-min) (point-max) 'org-draft-page-break t)
  (org-draft--make-page-break-templates)
  (org-draft--paginate-forward (current-buffer)))

(defun org-draft--paginate-forward (buffer)
  (when (eq buffer (current-buffer))
    (org-draft--cancel-pagination-timer)

    (let (beg page-num end)

      (if org-draft--page-break-cache
          (let ((last-break (car (last org-draft--page-break-cache))))
            (setq page-num (1+ (overlay-get last-break 'org-draft-page-num))
                  beg (overlay-end last-break)))
        (setq page-num 1
              beg (org-draft--get-pagination-start-pos)))

      (setq end (min (point-max) (+ beg org-draft-partial-repagination-count)))

      (save-excursion
        (org-draft--with-visibility beg end
          (goto-char beg)
          (while (and (org-draft--skip-page-lines)
                      (not (eobp)))
            (org-draft--simple-goto-eol)
            (org-draft--insert-page-break page-num)
            (setq page-num (1+ page-num))))

        (if (eq end (point-max))
            (unless (or (org-draft--at-first-page-line-p)
                        (org-draft--at-last-page-line-p))
              (forward-char -1)
              (org-draft--insert-page-break page-num))

          (setq org-draft--keep-paginating-idle-timer
                (run-with-idle-timer 0.5 nil 'org-draft--paginate-forward (current-buffer)))))

      (org-draft--rebuild-page-break-cache))))

(defun org-draft--auto-repaginate ()
  (when (bound-and-true-p org-draft-mode)
    (org-draft--rebuild-page-break-cache)

    (let* ((inhibit-redisplay t)
           (beg org-draft--auto-repagination-change-pt)
           (next-break (org-draft--get-next-page-break beg))
           (prev-break (org-draft--get-prev-page-break beg))

           (page-num (if next-break (overlay-get (car next-break) 'org-draft-page-num) 1))

           (start (if prev-break (overlay-end (car prev-break)) (org-draft--get-pagination-start-pos)))
           (stop (if next-break (min (1+ (overlay-end (car next-break))) (point-max)) (point-max)))

           dont-repaginate)

      (setq org-draft--auto-repagination-change-pt nil)

      (save-excursion
        (org-draft--with-visibility start stop
          (goto-char start)
          (org-draft--skip-page-lines)
          (setq dont-repaginate
                (eq page-num (org-draft--prop-p 'org-draft-page-num (org-draft--visual-eol-pos))))))

      (unless dont-repaginate
        (org-draft--delete-expired-breaks-from-cache beg (cdr next-break))
        (org-draft--make-page-break-templates)
        (org-draft--paginate-forward (current-buffer))))))

(defun org-draft--schedule-auto-repaginate (beg _end _len)
  (when org-draft--auto-repagination-idle-timer
    (cancel-timer org-draft--auto-repagination-idle-timer))
  (setq org-draft--auto-repagination-change-pt
        (min beg (or org-draft--auto-repagination-change-pt beg)))
  (setq org-draft--auto-repagination-idle-timer
        (run-with-idle-timer 0.5 nil #'org-draft--auto-repaginate)))


;;; Mode definition:

(defvar org-draft-mode-map (make-sparse-keymap))

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
    (add-hook 'window-state-change-hook #'org-draft--schedule-update-page-breaks -10 t)
    (add-hook 'after-change-functions #'org-draft--schedule-auto-repaginate 0 t)
    (org-draft-paginate-buffer)))

(defun org-draft--maybe-keep-column ()
  (when (bound-and-true-p org-draft-mode)
    (if (member this-command org-draft-keep-column-commands)
        (when (not org-draft--col-cache)
          (setq org-draft--col-cache (- (point) (org-draft--visual-bol-pos))))
      (setq org-draft--col-cache nil))))

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
  (advice-add 'scroll-up-line :around #'org-draft--fix-scroll-up-line-ad)
  (advice-add 'scroll-down-line :around #'org-draft--fix-scroll-down-line-ad)
  (advice-add 'org-previous-visible-heading :after #'org-draft--org-previous-visible-heading-ad)
  (advice-add 'keyboard-quit :before #'org-draft--keyboard-quit-ad)
  (when (fboundp '_keyboard-quit)
    (advice-add '_keyboard-quit :before #'org-draft--keyboard-quit-ad))

  (keymap-set org-draft-mode-map "<remap> <org-end-of-line>" 'org-draft-goto-eol)
  (keymap-set org-draft-mode-map "<remap> <org-beginning-of-line>" 'org-draft-goto-bol)
  (keymap-set org-draft-mode-map "<remap> <next-line>" 'org-draft-next-line)
  (keymap-set org-draft-mode-map "<remap> <_next-line>" 'org-draft-next-line)
  (keymap-set org-draft-mode-map "<remap> <previous-line>" 'org-draft-previous-line)
  (keymap-set org-draft-mode-map "<remap> <_previous-line>" 'org-draft-previous-line)

  (if org-draft-mode
      (progn
        (setq org-draft--scroll-margin (min (max 0 scroll-margin)
		                            (truncate (/ (window-body-height) 4.0))))
        (org-draft--make-complex-heading-regexp)
        (org-draft--maybe-headings)
        (org-draft--maybe-indent)
        (org-draft--maybe-paginate)
        (add-hook 'pre-command-hook #'org-draft--maybe-keep-column nil t))

    (org-draft--cancel-pagination-timer)
    (remove-hook 'after-change-functions #'org-draft--auto-format-headings t)
    (remove-hook 'after-change-functions #'org-draft--auto-indent t)
    (remove-hook 'after-change-functions #'org-draft--schedule-auto-repaginate t)
    (remove-hook 'window-state-change-hook #'org-draft--schedule-update-page-breaks t)
    (remove-hook 'pre-command-hook #'org-draft--maybe-keep-column t)

    (org-draft-remove-overlays)
    (setq org-draft--scroll-margin nil
          org-draft--window-width nil
          org-draft--page-break-cache nil
          org-draft--soft-page-break-templates nil
          org-draft--hard-page-break-templates nil
          org-draft--visibility nil
          org-draft--complex-heading-regexp nil
          org-draft--auto-repagination-change-pt nil
          org-draft--auto-repagination-idle-timer nil
          org-draft--keep-paginating-idle-timer nil)))

(provide 'org-draft)

;;; org-draft.el ends here
