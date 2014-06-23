;;; vcalendar.el --- Parse vcal documents
;; Copyright (C) 2014 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: calendar

;; This file is not part of GNU Emacs.

;; vcalendar.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; vcalendar.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun vcalendar-parse-region (start end)
  "Parse the vcalendar document between START and END."
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buffer start end)
      (goto-char (point-min))
      ;; Fold continuation lines.
      (while (re-search-forward "\n " nil t)
	(replace-match "" t t))      
      (goto-char (point-min))
      (vcalendar-parse-section))))

(defun vcalendar-parse-section ()
  ;; Skip past possible junk before the BEGIN line.
  (while (and (not (looking-at "BEGIN:\\(.*\\)"))
	      (not (eobp)))
    (forward-line 1))
  (unless (eobp)
    (let ((section-name (match-string 1))
	  stop result)
      (forward-line 1)
      (while (and (looking-at "\\([^:]+\\):\\(.*\\)")
		  (not stop))
	(let ((tag (match-string 1))
	      (value (replace-regexp-in-string "\\\\n" "\n" (match-string 2))))
	  (cond
	   ((equal tag "BEGIN")
	    (push (vcalendar-parse-section) result))
	   ((and (equal tag "END")
		 (equal value section-name))
	    (forward-line 1)
	    (setq stop t))
	   (t
	    ;; Some items look like DATE;TZ=CET:FOOBAR.
	    (if (string-match "\\`\\(.*\\);\\(.*\\)$" tag)
		(let ((note (match-string 2 tag)))
		  (push (list (intern (match-string 1 tag) obarray)
			      (cons :note note)
			      (cons :value value))
			result))
	      ;; Regular, un-semicoloned items.
	      (push (list (intern (downcase tag) obarray)
			  (cons :value value))
		    result))
	    (forward-line 1)))))
      (cons (intern (downcase section-name) obarray)
	    (nreverse result)))))

(provide 'vcalendar)

;;; vcalendar.el ends here

