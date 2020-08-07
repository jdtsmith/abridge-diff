;;; abridge-diff.el --- Abridge long line-based diffs -*- lexical-binding:t -*-

;; Copyright (C) 2020 J.D. Smith <jdtsmith AT gmail>

;; Author: J.D. Smith <jdtsmith AT gmail>
;; URL: https://github.com/jdtsmith/abridge-diff
;; Version: 0.0.1
;; Keywords: magit diffs 
;; Package-Requires: 
;; Prefix: abridge-diff
;; Separator: -

;;; Commentary:
;;
;; abridge-diff can be installed from Melpa with M-x `package-install' RET
;; abridge-diff.

;; To use, call M-x abridge-diff-toggle-hiding on any "refined" (word
;; change highlighted) diff hunk.  Typically this would be in a magit
;; log or status buffer, for which the special command `D a' will be
;; automatically added. Note that this works best with
;; 'magit-diff-refine-hunk set to 'all, so that all hunks are refined
;; at once.
;;
;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
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

(defcustom abridge-diff-word-buffer 3
  "Number of words to preserve around refined regions."
  :group 'abridge-diff
  :type 'integer)

(defcustom abridge-diff-invisible-min 5
  "Minimum region length (in characters) between refine areas that can be made invisible."
  :group 'abridge-diff
  :type 'integer)

(defcustom abridge-diff-no-change-line-words 12
  "Number of words to keep at the beginning of a line without any refined diffs."
  :group 'abridge-diff
  :type 'integer)

(defcustom abridge-diff-first-words-preserve 4
  "Keep at least this many words visible at the beginning of an abridged line with refined diffs."
  :group 'abridge-diff
  :type 'integer)

(defun abridge-diff-merge-exclude (excludes)
  (let ((p excludes))
    (while (cdr p)
      (let ((left (car p))
	    (right (cadr p)))
	(if (>= (- (car right) (cadr left)) abridge-diff-invisible-min)
	    (setq p (cdr p))
	  (setcar p (list (car left) (cadr right)))
	  (setcdr p (cddr p)))))))

(defun abridge-diff-compute-hidden (beg end excludes)
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
		     abridge-diff-invisible-min))
		(nreverse hide))))

(defun abridge-diff-make-invisible (beg end)
  (if (> (- end beg) abridge-diff-invisible-min)
      (let ((protect
	     (mapcar (lambda (ov)
		       (let ((ovbeg (overlay-start ov))
			     (ovend (overlay-end ov))
			     pbeg pend)
			 (save-excursion
			   (goto-char ovbeg)
			   (backward-word abridge-diff-word-buffer)
			   (setq pbeg (max beg (point)))
			   (goto-char ovend)
			   (forward-word abridge-diff-word-buffer)
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
				(forward-word abridge-diff-no-change-line-words)
				(min (point) end))
			      end)))
	  (save-excursion
	    (goto-char beg)
	    (forward-word abridge-diff-first-words-preserve)
	    (push (list beg (min end (point))) protect))
	  (abridge-diff-merge-exclude protect)
	  (setq hide (abridge-diff-compute-hidden beg end protect)))

	(dolist (range hide)
	  (add-text-properties (car range) (cadr range)
			       '(invisible abridge-diff-invisible))))))

(defun abridge-diff-abridge (&rest rest)
  (dolist (x (seq-partition (seq-take rest 4) 2))
    (save-excursion
      (goto-char (car x))
      (while (< (point) (cadr x))
	(abridge-diff-make-invisible (point) (line-end-position))
	(forward-line)))))

(advice-add #'smerge-refine-regions :after #'abridge-diff-abridge)

(defvar abridge-diff-hiding nil)
(defun abridge-diff-enable-hiding ()
  (interactive)
  (setq abridge-diff-hiding t)
  (add-to-invisibility-spec '(abridge-diff-invisible . t)))

(defun abridge-diff-disable-hiding ()
  (interactive)
  (setq abridge-diff-hiding nil)
  (remove-from-invisibility-spec '(abridge-diff-invisible . t)))

(defun abridge-diff-toggle-hiding ()
  (interactive)
  (if abridge-diff-hiding
      (abridge-diff-disable-hiding)
    (abridge-diff-enable-hiding)))

;; Add hooks into magit if it's loaded
(when (fboundp 'magit)
  (require 'magit-diff)
  (add-hook 'magit-diff-mode-hook #'abridge-diff-enable-hiding)
  (add-hook 'magit-status-mode-hook #'abridge-diff-enable-hiding)
  (transient-append-suffix 'magit-diff-refresh 'magit-diff-toggle-refine-hunk
    '("a" "abridge refined diffs" abridge-diff-toggle-hiding)))



