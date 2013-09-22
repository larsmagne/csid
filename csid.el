;;; smid.el --- Generate Concert Listings
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; smid.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; smid.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'pp)

(defvar smid-database-file-name "~/.emacs.d/smid.data")

(defvar smid-sources
  '(("Revolver" "http://www.revolveroslo.no/nb/program" smid-parse-revolver)
    ("Blå" "http://www.blaaoslo.no/program/" smid-parse-blaa)
    ("Mir" "http://www.lufthavna.no/cafe-mir/" smid-parse-mir)
    ("Crossroads" "http://thecrossroadclub.no/program/" smid-parse-crossroads)
    ("Victoria" "http://nasjonaljazzscene.no/arrangement/" smid-parse-victoria)
    ("Rockefeller" "http://rockefeller.no/index.html" smid-parse-rockefeller)
    ("Mono" "http://www.cafemono.no/program/" smid-parse-mono)
    ("Parkteateret" "http://www.linticket.no/program/parkteatret/index.php3?"
     smid-parse-parkteateret)
    ))

(defvar smid-database nil)

(defun smid-write-database (data)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file smid-database-file-name
      (pp data (current-buffer)))))

(defun smid-update-database (data)
  (dolist (elem data)
    (let ((old (assoc (car elem) smid-database)))
      (when old
	(setq smid-database (delete old smid-database))))
    (push elem smid-database))
  smid-database)

(defun smid-read-database ()
  (let ((coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert-file-contents smid-database-file-name)
      (setq smid-database (read (current-buffer))))))

(defun smid-parse-sources (&optional type)
  (smid-write-database
   (smid-update-database
    (loop for (name url function) in smid-sources
	  when (or (not type)
		   (string= type name))
	  collect (cons name
			(with-current-buffer (url-retrieve-synchronously url)
			  (goto-char (point-min))
			  (when (search-forward "\n\n")
			    (let ((shr-base (shr-parse-base url)))
			      (funcall function
				       (shr-transform-dom 
					(libxml-parse-html-region
					 (point) (point-max))))))))))))

(defun smid-parse-revolver (dom)
  (loop for elem in (dom-elements-by-class dom "views-table")
	for date = (cdr (assq :content
			      (car (dom-elements-by-class elem "date-display-single"))))
	for link = (car (dom-elements-by-name elem 'a))
	collect (list (substring date 0 10)
		      (shr-expand-url (cdr (assq :href link)))
		      (cdr (assq 'text link)))))

(defun smid-parse-blaa (dom)
  (setq dom (car (dom-elements-by-class dom "calendar-content")))
  (loop for (date contents) on (cddr dom) by #'cddr
	for info = (car (dom-elements-by-class contents "event-info"))
	for link = (car (dom-elements-by-name info 'a))
	collect (list (smid-parse-month-date (cdr (assq 'text date)))
		      (shr-expand-url (cdr (assq :href link)))
		      (cdr (assq 'text link)))))

(defvar smid-months '("januar" "februar" "mars" "april" "mai" "juni" "juli"
		      "august" "september" "oktober" "november" "desember"))

;; "Fredag 27. september"
(defun smid-parse-month-date (string)
  (setq date (downcase string))
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat 'identity smid-months "\\|"))
		    string)
      (smid-expand-date (1+ (position (match-string 2 string) smid-months
				      :test 'equalp))
			(string-to-number (match-string 1 string)))
    string))

(defvar smid-english-months
  '("january" "february" "march" "april" "may" "june" "july"
    "august" "september" "october" "november" "december"))

(defun smid-parse-english-month-date (string)
  (setq date (downcase string))
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat 'identity smid-english-months "\\|"))
		    string)
      (smid-expand-date (1+ (position (match-string 2 string)
				      smid-english-months
				      :test 'equalp))
			(string-to-number (match-string 1 string)))
    string))

;; "06. aug 2013"
(defun smid-parse-short-month (string)
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\) \\([0-9]+\\)"
			    (mapconcat
			     (lambda (month)
			       (substring month 0 3))
			     smid-months "\\|"))
		    string)
      (format "%s-%02d-%s"
	      (match-string 3 string)
	      (1+ (position (match-string 2 string)
			    (mapcar
			     (lambda (month)
			       (substring month 0 3))
			     smid-months)
			    :test 'equalp))
	      (match-string 1 string))
    string))

;; "Ma. 23. sep. "
(defun smid-parse-short-yearless-month (string)
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat
			     (lambda (month)
			       (substring month 0 3))
			     smid-months "\\|"))
		    string)
      (smid-expand-date
       (1+ (position (match-string 2 string)
		     (mapcar
		      (lambda (month)
			(substring month 0 3))
		      smid-months)
		     :test 'equalp))
       (string-to-number (match-string 1 string)))
    string))

;; 23.09
(defun smid-parse-numeric-date (string)
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\)" string)
      (smid-expand-date (string-to-number (match-string 2 string))
			(string-to-number (match-string 1 string)))
    string))

;; "22.09.13"
(defun smid-parse-full-numeric-date (string)
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\)" string)
      (format "%04d-%02d-%02d"
	      (+ 2000 (string-to-number (match-string 3 string)))
	      (string-to-number (match-string 2 string))
	      (string-to-number (match-string 1 string)))
    string))

(defun smid-expand-date (month day)
  (let ((this-year (nth 5 (decode-time)))
	(this-month (nth 4 (decode-time))))
    (when (< month this-month)
      (incf this-year))
    (format "%s-%02d-%02d" this-year month day)))

(defun smid-parse-mir (dom)
  (loop for elem in (dom-elements-by-class dom "^mir_gig$")
	collect (list (smid-parse-month-date
		       (cdr (assq 'text (cdr (assq 'div elem)))))
		      (cdr (assq 'text (cdr (assq 'h3 elem))))
		      (shr-expand-url ""))))

(defun smid-parse-crossroads (dom)
  (loop for elem in (cdr (dom-elements-by-name
			  (car (dom-elements-by-name dom 'table))
			  'tr))
	for tds = (dom-elements-by-name elem 'td)
	collect (list (smid-parse-short-month (cdr (assq 'text (nth 0 tds))))
		      (cdr (assq :href (car (dom-elements-by-name (nth 3 tds) 'a))))
		      (cdr (assq 'text (nth 1 tds))))))

(defun smid-parse-victoria (dom)
  (loop for elem in (dom-elements-by-class dom "event-entry")
	for date = (car (dom-elements-by-class elem "show-for-small"))
	collect (list (smid-parse-numeric-date
		       (cdr (assq 'text (car (dom-elements-by-name date 'p)))))
		      (cdr (assq :href (car (dom-elements-by-name elem 'a))))
		      (cdr (assq 'text (car (dom-elements-by-name elem 'h2)))))))

(defun smid-parse-rockefeller (dom)
  (loop for elem in (dom-elements-by-name
		     (car (dom-elements-by-id dom "print"))
		    'table)
	for tds = (dom-elements-by-name elem 'td)
	for link = (assq 'a (nth 2 tds))
	collect (list (smid-parse-full-numeric-date (cdar (last (nth 1 tds))))
		      (shr-expand-url (cdr (assq :href (cdr link))))
		      (cdr (assq 'text (cdr link))))))

(defun smid-parse-mono (dom)
  (loop for elem in (dom-elements-by-class dom "artist")
	for link = (car (dom-elements-by-name
			 (car (dom-elements-by-name elem 'h2))
			 'a))
	collect (list (smid-parse-english-month-date
		       (cdr (assq 'text (car (dom-elements-by-name elem 'h3)))))
		      (cdr (assq :href (cdr link)))
		      (cdr (assq 'text (cdr link))))))

(defun smid-parse-parkteateret (dom)
  (loop for elem in (dom-elements-by-name dom 'tr)
	for link = (car (dom-elements-by-class elem "linticket_arrnavn"))
	when link
	collect (list (smid-parse-short-yearless-month
		       (cdr (assq 'text (car (dom-elements-by-class elem "linticket_info$")))))
		      (cdr (assq :href (cdr link)))
		      (cdr (assq 'text (cdr link))))))

(defun smid-parse-new (dom)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (pp dom (current-buffer))
  (goto-char (point-min)))

(defun smid-generate-html ()
  (let ((data
	 (sort
	  (loop for elem in smid-database
		append (loop for (date url name) in (cdr elem)
			     collect (list date (car elem) url name)))
	  (lambda (e1 e2)
	    (string< (car e1) (car e2)))))
	(coding-system-for-write 'utf-8)
	(now (format-time-string "%Y-%m-%d"))
	prev-date)
    (with-temp-file "/tmp/smid.html"
      (insert "<head><title>Crowd Sourcing Is Dead</title><link href='smid.css' rel='stylesheet' type='text/css'><img src='csid.png'>")
      (insert "<table>")
      (loop for (date venue url name) in data
	    unless (string< date now)
	    do (insert (format "<tr><td>%s<td>%s<td><a href='%s'>%s"
			       (if (equal prev-date date)
				   ""
				 date)
			       venue url name))
	    (setq prev-date date))
      (insert "</table>"))))

(provide 'smid)

;;; smid.el ends here
