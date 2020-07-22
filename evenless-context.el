(defvar evenless-context-word-buffer 6
  "Number of words to preserver before and after highlighted areas.")

(defvar evenless-context-invisible-min 5
  "Minimum region length between highlighted areas that can be made invisible.")

(defvar evenless-context-no-change-line-words 15
  "Number of words to keep at the beginning of a line without refined changes")

(defun evenless-context-merge-exclude (excludes)
  (let ((p excludes))
    (while (cdr p)
      (let ((left (car p))
	    (right (cadr p)))
	(if (>= (- (car right) (cadr left)) evenless-context-invisible-min)
	    (setq p (cdr p))
	  (setcar p (list (car left) (cadr right)))
	  (setcdr p (cddr p)))))))

(defun evenless-context-compute-hidden (beg end excludes)
  "Compute a list of ranges (from to) between position beg and end, 
skipping the ranges listed in EXCLUDES"
  (let ((hide (list (list beg (caar excludes))))
	(p excludes))
    (while (cdr p)
      (let ((left (car p))
	    (right (cadr p)))
	(push (list (cadr left) (car right)) hide)
	(setq p (cdr p))))
    (push (list (cadr (car (last excludes))) end) hide)
    (seq-filter (lambda (range)
		  (> (- (cadr range) (car range))
		     evenless-context-invisible-min))
		(nreverse hide))))

(defun evenless-context-make-invisible (beg end)
  (if (> (- end beg) evenless-context-invisible-min)
      (let ((protect
	     (mapcar (lambda (ov)
		       (let ((ovbeg (overlay-start ov))
			     (ovend (overlay-end ov))
			     pbeg pend)
			 (save-excursion
			   (goto-char ovbeg)
			   (backward-word evenless-context-word-buffer)
			   (setq pbeg (max beg (point)))
			   (goto-char ovend)
			   (forward-word evenless-context-word-buffer)
			   (setq pend (min end (point))))
			 (list pbeg pend)))
		     (sort 
		      (seq-filter (lambda (ov)
				    (eq (overlay-get ov 'diff-mode) 'fine))
				  (overlays-in beg end))
		      (lambda (a b) (< (overlay-start a) (overlay-start b))))))
	    hide)
	
	(if (memq (char-after beg) '(?+ ?-))
	    (setq beg (1+ beg)))

	(if (not protect) ;nothing specific changed, just show first words
	    (setq hide (list (list
			      (save-excursion
				(goto-char beg)
				(forward-word evenless-context-no-change-line-words)
				(min (point) end))
			      end)))
	  (evenless-context-merge-exclude protect)
	  (setq hide (evenless-context-compute-hidden beg end protect)))

	(dolist (range hide)
	  (add-text-properties (car range) (cadr range)
			       '(invisible evenless-context-invisible))))))

(defun evenless-context-mark (&rest rest)
  (dolist (x (seq-partition (seq-take rest 4) 2))
    (save-excursion
      (goto-char (car x))
      (while (< (point) (cadr x))
	(evenless-context-make-invisible (point) (line-end-position))
	(forward-line)))))

(advice-add #'smerge-refine-regions :after #'evenless-context-mark)

(defun evenless-context-enable-hiding ()
  (interactive)
  (add-to-invisibility-spec '(evenless-context-invisible . t)))
(add-hook 'magit-diff-mode-hook #'evenless-context-enable-hiding)

(defun evenless-context-disable-hiding ()
  (interactive)
  (setq buffer-invisibility-spec nil))

(defun evenless-context-toggle-hiding ()
  (interactive)
  (if buffer-invisibility-spec
      (evenless-context-disable-hiding)
    (evenless-context-enable-hiding)))


;; (transient-define-argument 'evenless-context-toggle
;;   :key "e" :argument "even less context"
(require 'magit-diff)
(transient-append-suffix 'magit-diff-refresh 'magit-diff-toggle-refine-hunk
  '("e" "even less context" evenless-context-toggle-hiding))

;(advice-remove #'smerge-refine-regions #'evenless-context-mark)


