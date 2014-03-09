;;; csid.el --- Generate Concert Listings
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; csid.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csid.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To test a new source: (csid-parse-sources "Vulkan")

;;; Code:

(require 'cl)
(require 'pp)
(require 'eww)
(require 'dom)

(defvar csid-database-file-name "~/.emacs.d/csid.data")

(defvar csid-sequence 0)

(defvar csid-sources
  '(("Revolver" "http://www.revolveroslo.no/nb/program" revolver)
    ("Blå" "http://www.blaaoslo.no/program/" blaa)
    ("Mir" "http://www.lufthavna.no/" mir)
    ("Crossroads" "http://thecrossroadclub.no/program/" crossroads)
    ("Victoria" "http://nasjonaljazzscene.no/arrangement/" victoria)
    ("Rockefeller" "http://rockefeller.no/index.html" rockefeller :multi)
    ("Mono" "http://www.cafemono.no/program/" mono)
    ("Parkteateret" "http://www.linticket.no/program/parkteatret/index.php3?"
     parkteateret)
    ("Konsertforeninga" "http://www.konsertforeninga.no/konserter"
     konsertforeninga)
    ("Maksitaksi" "http://maksitaksi.no/program-2/" maksitaksi)
    ("Betong" "https://studentersamfundet.no/program/" betong)
    ("Mu" "http://www.soundofmu.no/" mu :date)
    ("Bidrobon" "http://www.bidrobon.no/" bidrobon :date)
    ("Cosmopolite" "http://cosmopolite.no/program/cosmopolite" cosmopolite)
    ("Belleville" "http://cosmopolite.no/program/belleville" cosmopolite)
    ("Vulkan" "http://vulkanarena.no/shows" vulkan)
    ))

(defvar csid-database nil)

(defun csid-write-database (data)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file csid-database-file-name
      (pp data (current-buffer)))))

(defun csid-update-database (data)
  (dolist (elem data)
    ;; Don't update if we didn't get any data.
    (unless (cl-member (nth 4 elem) csid-database
		       :key (lambda (event)
			      (nth 4 event)))
      (push elem csid-database)))
  csid-database)

(defun csid-read-database ()
  (let ((coding-system-for-write 'utf-8))
    (when (file-exists-p csid-database-file-name)
      (with-temp-buffer
	(insert-file-contents csid-database-file-name)
	(setq csid-database (read (current-buffer))))))
  (dolist (elem csid-database)
    (setq csid-sequence (max (nth 4 elem) csid-sequence))))

(defun csid-parse-sources (&optional type)
  (csid-write-database
   (csid-update-database
    (loop for source in csid-sources
	  for (name url function) = source
	  when (or (not type)
		   (string= type name))
	  append (loop for result in
		       (csid-parse-source
			url
			(intern (format "csid-parse-%s" function) obarray))
		       unless (memq :multi source)
		       do (push name result)
		       collect (csid-add-id result (memq :date source)))))))

(defun csid-add-id (elem datep)
  (let ((found nil))
    (loop with index = (if datep 1 2)
	  for old in csid-database
	  when (and (equal (car elem) (car old))
		    (equal (nth index elem)
			   (nth index old)))
	  do (setq found (nth 4 old)))
    (append elem (list (or found (incf csid-sequence))))))

(defun csid-parse-source (url function)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (when (search-forward "\n\n")
      (let* ((headers (eww-parse-headers))
	     (content-type
	      (mail-header-parse-content-type
	       (or (cdr (assoc "content-type" headers))
		   "text/plain")))
	     (charset
	      (intern
	       (downcase
		(or (cdr (assq 'charset (cdr content-type)))
		    (eww-detect-charset t)
		    "utf8"))))
	     (shr-base (shr-parse-base url)))
	(decode-coding-region (point) (point-max) charset)
	(funcall function
		 (shr-transform-dom 
		  (libxml-parse-html-region
		   (point) (point-max))))))))

(defun csid-parse-revolver (dom)
  (loop for elem in (dom-by-class dom "views-table")
	for date = (dom-attr (car (dom-by-class elem "date-display-single"))
			     :content)
	for link = (car (dom-by-name elem 'a))
	collect (list (substring date 0 10)
		      (shr-expand-url (dom-attr link :href))
		      (dom-text link))))

(defun csid-parse-blaa (dom)
  (setq dom (car (dom-by-class dom "calendar-content")))
  (loop for (date contents) on (cddr dom) by #'cddr
	append (loop for info in (dom-by-class contents "event-info")
		     for link = (car (dom-by-name info 'a))
		     for text = (dom-text link)
		     when (plusp (length text))
		     collect (list (csid-parse-month-date (dom-text date))
				   (shr-expand-url (dom-attr link :href))
				   text))))

(defvar csid-months '("januar" "februar" "mars" "april" "mai" "juni" "juli"
		      "august" "september" "oktober" "november" "desember"))

;; "Fredag 27. september"
(defun csid-parse-month-date (string)
  (setq string (downcase string))
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat 'identity csid-months "\\|"))
		    string)
      (csid-expand-date (1+ (position (match-string 2 string) csid-months
				      :test 'equalp))
			(string-to-number (match-string 1 string)))
    string))

(defvar csid-english-months
  '("january" "february" "march" "april" "may" "june" "july"
    "august" "september" "october" "november" "december"))

(defun csid-parse-english-month-date (string)
  (setq string (downcase string))
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat 'identity csid-english-months "\\|"))
		    string)
      (csid-expand-date (1+ (position (match-string 2 string)
				      csid-english-months
				      :test 'equalp))
			(string-to-number (match-string 1 string)))
    string))

;; "06. aug 2013"
(defun csid-parse-short-month (string)
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\) \\([0-9]+\\)"
			    (mapconcat
			     (lambda (month)
			       (substring month 0 3))
			     csid-months "\\|"))
		    string)
      (format "%s-%02d-%s"
	      (match-string 3 string)
	      (1+ (position (match-string 2 string)
			    (mapcar
			     (lambda (month)
			       (substring month 0 3))
			     csid-months)
			    :test 'equalp))
	      (match-string 1 string))
    string))

;; "Ma. 23. sep. "
(defun csid-parse-short-yearless-month (string)
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat
			     (lambda (month)
			       (substring month 0 3))
			     csid-months "\\|"))
		    string)
      (csid-expand-date
       (1+ (position (match-string 2 string)
		     (mapcar
		      (lambda (month)
			(substring month 0 3))
		      csid-months)
		     :test 'equalp))
       (string-to-number (match-string 1 string)))
    string))

;; "2014-03-20T21:00:00+01:00"
(defun csid-parse-iso8601 (string)
  (substring string 0 10))

;; 23.09
(defun csid-parse-numeric-date (string)
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\)" string)
      (csid-expand-date (string-to-number (match-string 2 string))
			(string-to-number (match-string 1 string)))
    string))

;; "22.09.13"
(defun csid-parse-full-numeric-date (string)
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\)" string)
      (format "%04d-%02d-%02d"
	      (+ 2000 (string-to-number (match-string 3 string)))
	      (string-to-number (match-string 2 string))
	      (string-to-number (match-string 1 string)))
    string))

(defun csid-expand-date (month day &optional this-year-only)
  (let ((this-year (nth 5 (decode-time)))
	(this-month (nth 4 (decode-time))))
    (when (and (not this-year-only)
	       (< month this-month))
      (incf this-year))
    (format "%s-%02d-%02d" this-year month day)))

(defun csid-parse-mir (dom)
  (loop for elem in (dom-by-id dom "program")
	for time = (car (dom-by-class elem "programtid"))
	for link = (car (dom-by-name
			 (car (dom-by-class elem "programtittel")) 'a))
	collect (list (csid-parse-month-date (dom-text time))
		      (dom-attr link :href)
		      (dom-attr link :title))))

(defun csid-parse-crossroads (dom)
  (loop for elem in (cdr (dom-by-name
			  (car (dom-by-name dom 'table))
			  'tr))
	for tds = (dom-by-name elem 'td)
	collect (list (csid-parse-short-month (dom-text (nth 0 tds)))
		      (dom-attr (car (dom-by-name (nth 3 tds) 'a)) :href)
		      (dom-text (nth 1 tds)))))

(defun csid-parse-victoria (dom)
  (loop for elem in (dom-by-class dom "event-entry")
	for date = (car (dom-by-class elem "show-for-small"))
	collect (list (csid-parse-numeric-date
		       (dom-text (car (dom-by-name date 'p))))
		      (dom-attr (car (dom-by-name elem 'a)) :href)
		      (dom-text (car (dom-by-name elem 'h2))))))

(defun csid-parse-rockefeller (dom)
  (loop for elem in (dom-by-name
		     (car (dom-by-id dom "print"))
		     'table)
	for tds = (dom-by-name elem 'td)
	for link = (assq 'a (nth 2 tds))
	collect (list (csid-parse-rockefeller-stage
		       (dom-attr (car (dom-by-name (nth 0 tds) 'img)) :src)
		       (dom-text link))
		      (csid-parse-full-numeric-date (cdar (last (nth 1 tds))))
		      (shr-expand-url (dom-attr link :href))
		      (dom-text link))))

(defun csid-parse-rockefeller-stage (img text)
  (cond
   ((string-match "Bushwick" text)
    "Bushwick")
   ((string-match "Leiligheten" text)
    "Leiligheten")
   ((string-match "scene_R" img)
    "Rockefeller")
   ((string-match "scene_J" img)
    "John Dee")
   ((string-match "scene_S" img)
    "Sentrum")
   (t
    "Rockefeller")))

(defun csid-parse-mono (dom)
  (loop for elem in (dom-by-class dom "artist")
	for link = (car (dom-by-name
			 (car (dom-by-name elem 'h2))
			 'a))
	collect (list (csid-parse-english-month-date
		       (dom-text (car (dom-by-name elem 'h3))))
		      (dom-attr link :href)
		      (dom-text link))))

(defun csid-parse-parkteateret (dom)
  (loop for elem in (dom-by-name dom 'tr)
	for link = (car (dom-by-class elem "linticket_arrnavn"))
	when link
	collect (list (csid-parse-short-yearless-month
		       (dom-text (car (dom-by-class elem "linticket_info$"))))
		      (dom-attr link :href)
		      (dom-text link))))

(defun csid-parse-konsertforeninga (dom)
  (loop for elem in (dom-by-name
		     (car (dom-by-name dom 'table))
		     'tr)
	for tds = (dom-by-name elem 'td)
	for link = (car (dom-by-name (nth 2 tds) 'a))
	collect (list (csid-parse-month-date (cdr (car (last (nth 0 tds)))))
		      (shr-expand-url (dom-attr link :href))
		      (dom-text link))))

(defun csid-parse-maksitaksi (dom)
  (loop for elem in (dom-by-class dom "ai1ec-event-title-wrap")
	for link = (car (dom-by-class elem "ai1ec-load-event"))
	collect (list (csid-parse-month-date
		       (dom-text (car (dom-by-class elem "ai1ec-event-time"))))
		      (dom-attr link :href)
		      (dom-attr link :title))))

(defun csid-parse-betong (dom)
  (loop for elem in (dom-by-name
		     (car (dom-by-class dom "^table$"))
		     'tr)
	for tds = (dom-by-name elem 'td)
	for link = (car (dom-by-name (nth 1 tds) 'a))
	when (and link
		  (string-match "konsert" (dom-text (nth 3 tds))))
	collect (list (csid-parse-numeric-date (dom-text (nth 0 tds)))
		      (dom-attr link :href)
		      (dom-text link))))

(defun csid-parse-mu (dom)
  (loop for elem in (dom-by-name dom 'tr)
	for tds = (dom-by-name elem 'td)
	for type = (dom-text (nth 2 tds))
	when (and type
		  (string-match "^konsert$" type))
	collect (list (csid-parse-current-month (dom-text (nth 1 tds)))
		      (nth 3 shr-base)
		      (dom-text (nth 3 tds)))))

(defvar csid-weekdays '("mandag" "tirsdag" "onsdag" "torsdag"
			"fredag" "lørdag" "søndag"))

(defun csid-parse-current-month (string)
  (if (string-match (format "\\(%s\\) \\([0-9]+\\)"
			    (mapconcat 'identity csid-weekdays "\\|"))
		    string)
      (let ((day-number (1+ (position (downcase (match-string 1 string))
				      csid-weekdays
				      :test #'equal)))
	    (day (string-to-number (match-string 2 string)))
	    (time (decode-time))
	    found)
	;; Start the previous month, but don't skip back on
	;; Februaries, since they have four weeks, so the heuristic
	;; won't work.
	(when (or (not (= (nth 4 time) 3))
		  (date-leap-year-p (nth 5 time)))
	  (setcar (nthcdr 4 time) (1- (nth 4 time)))
	  (when (zerop (nth 4 time))
	    (setcar (nthcdr 4 time) 12)
	    (setcar (nthcdr 5 time) (1- (nth 5 time)))))
	(setcar (nthcdr 3 time) day)
	(dotimes (i 12)
	  (when (and (not found)
		     (= (string-to-number
			 (format-time-string "%u" (apply 'encode-time time)))
			day-number))
	    (setq found (copy-list time)))
	  (setcar (nthcdr 4 time) (1+ (nth 4 time)))
	  (when (= (nth 4 time) 13)
	    (setcar (nthcdr 4 time) 1)
	    (setcar (nthcdr 5 time) (1+ (nth 5 time)))))
	(format-time-string "%Y-%m-%d" (apply 'encode-time found)))
    string))

(defun csid-parse-bidrobon (dom)
  (loop for meta in (dom-by-name dom 'meta)
	when (equalp (dom-attr meta :http-equiv) "refresh")
	return (let ((url (dom-attr meta :content)))
		 (when (string-match "URL=\\(.*\\)" url)
		   (csid-parse-source (shr-expand-url (match-string 1 url))
				      'csid-parse-bidrobon-1)))))

(defun csid-parse-bidrobon-1 (dom)
  (loop for elem in (dom-by-name dom 'tr)
	for tds = (dom-by-name elem 'td)
	for text = (dom-texts (nth 0 tds))
	when (string-match "#[0-9]+\n.*?\\([0-9]+\\)/\\([0-9]+\\)" text)
	collect (list (csid-expand-date
		       (string-to-number (match-string 2 text))
		       (string-to-number (match-string 1 text))
		       t)
		      (nth 3 shr-base)
		      (csid-clean-string (dom-texts (nth 1 tds))))))

(defun csid-clean-string (string)
  (replace-regexp-in-string "^[\n\t ]\\|[\n\t ]$" "" 
			    (replace-regexp-in-string "[\n\t ]+" " " string)))

(defun csid-parse-cosmopolite (dom)
  (loop for elem in (dom-by-class dom "concert-item")
	for day = (dom-text (car (dom-by-class elem "day-of-the-month")))
	for month = (dom-text (car (dom-by-class elem "^month$")))
	collect (list (csid-parse-short-yearless-month
		       (format "%s %s" day month))
		      (shr-expand-url
		       (dom-attr (car (dom-by-name elem 'a))
				 :href))
		      (csid-clean-string
		       (dom-texts (car (dom-by-name elem 'h2)))))))

(defun csid-parse-vulkan (dom)
  (loop for elem in (dom-by-name dom 'article)
	for date = (cdr (assq :data-starts-at (cdr elem)))
	when date
	collect (list (csid-parse-iso8601 date)
		      (shr-expand-url
		       (cdr (assq :href (car (dom-by-name elem 'a)))))
		      (cdr (assq :data-title (cdr elem))))))

(defun csid-parse-new (dom)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (pp dom (current-buffer))
  (goto-char (point-min)))

(defun csid-generate-html (&optional file)
  (let ((data
	 (sort csid-database
	       (lambda (e1 e2)
		 (string< (cadr e1) (cadr e2)))))
	(coding-system-for-write 'utf-8)
	(now (format-time-string "%Y-%m-%d"))
	prev-date)
    (with-temp-file (or file "/tmp/csid.html")
      (insert "<head><title>Crowdsourcing Is Dead</title><meta charset='utf-8'><link href='csid.css' rel='stylesheet' type='text/css'><img src='csid.png'><p>(Also known as <a href='http://lars.ingebrigtsen.no/2013/09/crowdsourcing-is-dead.html'>Concerts In Oslo</a>.)</p><div id='selector'></div>")
      (insert "<table>")
      (loop for (venue date url name id) in data
	    unless (string< date now)
	    do (insert (format "<tr name='%s' id='event-%s'><td><div class='%s'>%s</div><td>%s<td><a href='%s'>%s</tr>"
			       (replace-regexp-in-string " " "_" venue)
			       id
			       (if (equal prev-date date)
				   "invisible"
				 "visible")
			       (csid-add-weekday date)
			       venue url
			       (url-insert-entities-in-string
				(if (> (length name) 1000)
				    (substring name 0 1000)
				  name))))
	    (setq prev-date date))
      (insert "</table><script type='text/javascript' src='jquery-1.10.2.min.js'></script><script type='text/javascript' src='jquery.cookie.js'></script><script type='text/javascript' src='csid.js'></script>"))))

(defun csid-add-weekday (date)
  (let ((time (encode-time 0 0 0
			   (string-to-number (substring date 8))
			   (string-to-number (substring date 5 7))
			   (string-to-number (substring date 0 4)))))
    (format-time-string "%a %b %d" time)))

(defun csid-update-html (file)
  (csid-read-database)
  (csid-parse-sources)
  (csid-generate-html file))

(provide 'csid)

;;; csid.el ends here
