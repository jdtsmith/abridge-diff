;;; abridge-diff.el --- Abridge long line-based diff hunks, including in magit -*- lexical-binding:t -*-

;; Copyright (C) 2020 J.D. Smith <jdtsmith AT gmail>

;; Author: J.D. Smith <jdtsmith AT gmail>
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jdtsmith/abridge-diff
;; Version: 0.0.2
;; Keywords: magit, diffs, tools
;; Prefix: abridge-diff
;; Separator: -

;;; Commentary:
;;
;; abridge-diff can be installed from Melpa with M-x `package-install' RET
;; abridge-diff.
;;
;; Usage:
;;
;; Once installed, abridge-diff will immediately start abridging all
;; refined (word change highlighted) diff hunks, shortening them by
;; replacing unnecessary surround context with ellipses (...) . You
;; can enable and disable showing the abridged version using
;; abridge-diff-toggle-hiding.  Automatically configures itself to work
;; with magit, adding a new `D a' diff setup command, which toggles the
;; abridging.  Hunks are shown as abridged by default.
;;
;; Settings:
;;
;; You can customize settings with these variables; just M-x customize-group abridge-diff:
;;  abridge-diff-word-buffer: Number of words to preserve around refined regions.
;;  abridge-diff-first-words-preserve: Keep at least this many words visible at the beginning of an abridged line with refined diffs.
;;  abridge-diff-invisible-min: Minimum region length (in characters) between refined areas that can be made invisible.
;;  abridge-diff-no-change-line-words: Number of words to keep at the beginning of a line without any refined diffs.
;;
;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'seq)

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
  "Merge exclude regions EXCLUDES, a list of lists."
  (let ((p excludes))
    (while (cdr p)
      (let ((left (car p))
	    (right (cadr p)))
	(if (>= (- (car right) (cadr left)) abridge-diff-invisible-min)
	    (setq p (cdr p))
	  (setcar p (list (car left) (cadr right)))
	  (setcdr p (cddr p)))))))

(defun abridge-diff-compute-hidden (beg end excludes)
  "Compute a list of ranges (from to) between position BEG and END.
Skip the ranges listed in EXCLUDES"
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
  "Set invisibility for context surrounding refined diffs in region from BEG to END."
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
;;;###autoload
(defun abridge-diff-abridge (&rest rest)
  "Do the diff abridge, taking as REST the region argument of `smerge-refine-regions'."
  (dolist (x (seq-partition (seq-take rest 4) 2))
    (save-excursion
      (goto-char (car x))
      (while (< (point) (cadr x))
	(abridge-diff-make-invisible (point) (line-end-position))
	(forward-line)))))

;;;###autoload
(advice-add #'smerge-refine-regions :after #'abridge-diff-abridge)

(defvar abridge-diff-hiding nil)
;;;###autoload
(defun abridge-diff-enable-hiding ()
  "Enable abridged text hiding."
  (interactive)
  (setq abridge-diff-hiding t)
  (add-to-invisibility-spec '(abridge-diff-invisible . t)))

;;;###autoload
(defun abridge-diff-disable-hiding ()
  "Disable abridged text hiding."
  (interactive)
  (setq abridge-diff-hiding nil)
  (remove-from-invisibility-spec '(abridge-diff-invisible . t)))

;;;###autoload
(defun abridge-diff-toggle-hiding ()
  "Toggle abridged text hiding."
  (interactive)
  (if abridge-diff-hiding
      (abridge-diff-disable-hiding)
    (abridge-diff-enable-hiding)))

;; Add hooks into magit if it's loaded
;;;###autoload
(when (fboundp 'magit)
  (require 'magit-diff)
  (require 'transient)
  (add-hook 'magit-diff-mode-hook #'abridge-diff-enable-hiding)
  (add-hook 'magit-status-mode-hook #'abridge-diff-enable-hiding)
  (transient-append-suffix 'magit-diff-refresh 'magit-diff-toggle-refine-hunk
    '("a" "abridge refined diffs" abridge-diff-toggle-hiding)))

(provide 'abridge-diff)
;;; abridge-diff.el ends here
