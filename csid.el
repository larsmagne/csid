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

;; To test a new source: (csid-parse-sources "Rockefeller")

;;; Code:

(require 'cl)
(require 'pp)
(require 'eww)
(require 'dom)
(require 'vcalendar)
(require 'json)

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
    ("Parkteateret" "http://parkteatret.no/program/" parkteateret)
    ("Konsertforeninga" "http://www.konsertforeninga.no/konserter"
     konsertforeninga)
    ;;("Maksitaksi" "http://maksitaksi.no/program-2/" maksitaksi)
    ("Betong" "https://studentersamfundet.no/program/" betong)
    ("Bidrobon" "http://www.bidrobon.no/" bidrobon :date)
    ("Cosmopolite" "http://cosmopolite.no/program/cosmopolite" cosmopolite)
    ("Belleville" "http://cosmopolite.no/program/belleville" cosmopolite)
    ("Vulkan" "http://vulkanarena.no/shows" vulkan)
    ("Jakob" "http://www.jakob.no/program/" jakob)
    ("Vanguard" "http://www.fanrx.com/facebook/events.php?theme=custom&page=vanguardoslo&bgcolor=ffffff&textcolor=000000&linkcolor=555555&max=15" vanguard)
    ("Ultima" "http://ultima.no/program" ultima)
    ("Blitz" "http://www.blitz.no/kalender" blitz)
    ("Magneten" "http://magnetenpub.blogspot.no//feeds/pages/default?alt=json&v=2&dynamicviews=1"
     magneten :json)
    ("HerrNilsen" "http://www.herrnilsen.no/program2009.html" nilsen)
    ("Spektrum" "http://www.oslospektrum.no/" spektrum)
    ("NyMusikk" "http://nymusikk.no/no/hva-skjer/" nymusikk)
    ("Konserthuset" "http://oslokonserthus.no/public/eventschedule.jsp?month=8&year=2014" konserthuset)
    ("Riksscenen" "http://www.riksscenen.no/program.95415.no.html" riksscenen)
    ("Olsen" "http://olsenbar.no/?page_id=3447" olsen)
    ("Verkstedet" "http://www.verkstedetbar.no/program/" verkstedet :date)
    ("Gamla" "http://www.gamla.no/" gamla)
    ))

(defvar csid-database nil)

(defun csid-write-database (data)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file csid-database-file-name
      (pp data (current-buffer))))
  nil)

(defun csid-update-database (data)
  (dolist (elem data)
    (let ((old
	   (cl-member (nth 4 elem) csid-database
		       :key (lambda (event)
			      (nth 4 event)))))
      (if (not old)
	  (push elem csid-database)
	;; Update the title.
	(when (plusp (length (nth 3 elem)))
	  (setcar (nthcdr 3 (car old)) (nth 3 elem)))
	;; Update the date.
	(when (plusp (length (nth 1 elem)))
	  (setcar (nthcdr 1 (car old)) (nth 1 elem))))))
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
	  for function = (intern (format "csid-parse-%s" function) obarray)
	  when (or (not type)
		   (string= type name))
	  append (let ((results 
			(csid-parse-source
			 url
			 (if (fboundp function)
			     function
			   'csid-parse-new)
			 (if (memq :json source)
			     :json
			   :html))))
		   (unless results
		     (message "No results for type %s" type))
		   (loop for result in results
			 unless (memq :multi source)
			 do (push name result)
			 collect (csid-add-id result (memq :date source))))))))

(defun csid-add-id (elem datep)
  (let ((found nil))
    (loop with index = (if datep 1 2)
	  for old in csid-database
	  when (and (equal (car elem) (car old))
		    (equal (nth index elem)
			   (nth index old)))
	  do (setq found (nth 4 old)))
    (append elem (list (or found (incf csid-sequence))))))

(defun csid-parse-source (url function data-type)
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
		    "utf-8"))))
	     (shr-base (shr-parse-base url)))
	(decode-coding-region (point) (point-max) charset)
	(funcall function
		 (cond
		  ((eq data-type :json)
		   (json-read))
		  (t
		   (shr-transform-dom 
		    (libxml-parse-html-region (point) (point-max))))))))))

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

;; "fredag, august 8, 2014"
(defun csid-parse-month-date-with-year (string)
  (setq string (downcase string))
  (when (string-match (format "\\(%s\\).*?\\([0-9]+\\).*?\\([0-9]+\\)"
			      (mapconcat 'identity csid-months "\\|"))
		      string)
    (format "%04d-%02d-%02d"
	    (string-to-number (match-string 3 string))
	    (1+ (position (match-string 1 string) csid-months
			  :test 'equalp))
	    (string-to-number (match-string 2 string)))))

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

(defun csid-parse-rfc2822 (string)
  (format-time-string "%Y-%m-%d"
		      (apply 'encode-time
			     (mapcar
			      (lambda (number)
				(or number 0))
			      (parse-time-string string)))))

;; "06. aug 2013"
(defun csid-parse-short-month (string)
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\) \\([0-9]+\\)"
			    (mapconcat
			     (lambda (month)
			       (substring month 0 3))
			     csid-months "\\|"))
		    string)
      (format "%s-%02d-%02d"
	      (match-string 3 string)
	      (1+ (position (match-string 2 string)
			    (mapcar
			     (lambda (month)
			       (substring month 0 3))
			     csid-months)
			    :test 'equalp))
	      (string-to-number (match-string 1 string)))
    (csid-parse-month-date string)))

;; "06. sept 2013"
(defun csid-parse-shortish-month (string)
  (when (string-match (format "\\([0-9]+\\).*\\(%s\\).*?\\([0-9]+\\)"
			      (mapconcat
			       (lambda (month)
				 (substring month 0 3))
			       csid-months "\\|"))
		      string)
    (format "%s-%02d-%02d"
	    (match-string 3 string)
	    (1+ (position (match-string 2 string)
			  (mapcar
			   (lambda (month)
			     (substring month 0
					(length (match-string 2 string))))
			   csid-months)
			  :test 'equalp))
	    (string-to-number (match-string 1 string)))))

;; "Ma. 23. sep. "
(defun csid-parse-short-yearless-month (string &optional englishp)
  (when (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			      (mapconcat
			       (lambda (month)
				 (substring month 0 3))
			       (if englishp
				   csid-english-months
				 csid-months)
			       "\\|"))
		      string)
    (csid-expand-date
     (1+ (position (match-string 2 string)
		   (mapcar
		    (lambda (month)
		      (substring month 0 3))
		    (if englishp
			csid-english-months
		      csid-months))
		   :test 'equalp))
     (string-to-number (match-string 1 string)))))

;; "aug 23"
(defun csid-parse-short-reverse-yearless-month (string)
  (csid-parse-short-yearless-month (mapconcat 'identity
					      (reverse (split-string string))
					      " ")))


;; "2014-03-20T21:00:00+01:00"
(defun csid-parse-iso8601 (string)
  (substring string 0 10))

;; "20140916T210000"
(defun csid-parse-compact-iso8601 (string)
  (concat (substring string 0 4) "-"
	  (substring string 4 6) "-"
	  (substring string 6 8)))

;; 23.09
(defun csid-parse-numeric-date (string)
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\)" string)
      (csid-expand-date (string-to-number (match-string 2 string))
			(string-to-number (match-string 1 string)))
    string))

;; "22.09.13" or "22.09.2013"
(defun csid-parse-full-numeric-date (string)
  (if (string-match "\\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\)" string)
      (let ((year (string-to-number (match-string 3 string))))
	(format "%04d-%02d-%02d"
		(+ (if (< year 100)
		       2000
		     0)
		   year)
		(string-to-number (match-string 2 string))
		(string-to-number (match-string 1 string))))
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
  (loop for elem in (dom-by-class dom "post")
	for link = (car (dom-by-name elem 'a))
	when link
	collect (list (csid-parse-month-date-with-year
		       (dom-text (car (dom-by-class elem "entry-date"))))
		      (dom-attr link :href)
		      (dom-text link))))

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
		       (dom-texts link))
		      (csid-parse-full-numeric-date (cdar (last (nth 1 tds))))
		      (shr-expand-url (dom-attr link :href))
		      (dom-texts link))))

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
  (loop for elem in (dom-by-class dom "concert-item")
	collect (list (csid-parse-month-date
		       (dom-texts (car (dom-by-class elem "concert-date"))))
		      (dom-attr (car (dom-by-name elem 'a)) :href)
		      (dom-texts (car (dom-by-name elem 'h2))))))

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
				      'csid-parse-bidrobon-1
				      :html)))))

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
  (replace-regexp-in-string "^[\r\n\t ]\\|[\r\n\t ]$" "" 
			    (replace-regexp-in-string "[\r\n\t ]+" " " string)))

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

(defun csid-parse-jakob (dom)
  (loop for elem in (dom-by-class dom "eventItem")
	for day = (csid-clean-string
		   (dom-texts (car (dom-by-class elem "day"))))
	for month = (csid-clean-string
		     (dom-texts (car (dom-by-class elem "month"))))
	collect (list (csid-parse-short-yearless-month
		       (format "%s %s" day month))
		      (shr-expand-url (dom-attr (car (dom-by-class elem "more"))
						:href))
		      (csid-clean-string
		       (dom-texts (car (dom-by-name elem 'h2)))))))

(defun csid-parse-vanguard (dom)
  (loop for elem in (dom-by-style dom "clear:both")
	for a = (car (dom-by-name elem 'a))
	when (plusp (length (dom-texts a)))
	collect (list (csid-parse-rfc2822
		       (dom-texts (car (dom-by-class elem "smalltext"))))
		      (dom-attr a :href)
		      (dom-texts a))))

(defun csid-parse-ultima (dom)
  (loop for link in (dom-by-name dom 'a)
	for href = (dom-attr link :href)
	when (and href
		  (string-match "\\`webcal:\\(.*\\)" href))
	return (csid-parse-vcalendar (concat "http:" (match-string 1 href)))))

(defun csid-parse-vcalendar (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (when (search-forward "\n\n")
      (loop for event in (dom-by-name
			  (vcalendar-parse-region (point) (point-max))
			  'vevent)
	    collect
	    (list (csid-parse-compact-iso8601
		   (dom-attr (car (dom-by-name event 'dtstart)) :value))
		  (dom-attr (car (dom-by-name event 'url)) :value)
		  (dom-attr (car (dom-by-name event 'summary)) :value))))))

(defun csid-parse-blitz (dom)
  (loop for elem in (dom-by-class dom "views-row")
	for month = (dom-text (car (dom-by-class elem "calendar-date-month")))
	for day = (dom-text (car (dom-by-class elem "calendar-date-day")))
	for link = (car (dom-by-name elem 'a))
	when (plusp (length month))
	collect (list (csid-parse-short-yearless-month
		       (format "%s %s" day month)
		       t)
		      (shr-expand-url (dom-attr link :href))
		      (dom-text link))))

(defun csid-parse-magneten (data)
  (let ((html
	 (cdr (cadr
	       (assq 'content (aref (cdr
				     (assq 'entry (cdr (assq 'feed data))))
				    0))))))
    (with-temp-buffer
      (insert html)
      (csid-parse-magneten-html
       (shr-transform-dom 
	(libxml-parse-html-region (point-min) (point-max)))))))

;; Magneten's list is apparently hand-written, but this seems to do
;; the trick.
(defun csid-parse-magneten-html (dom)
  (let (result date)
    (with-temp-buffer
      (pp dom (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward ":style . \"font-size: x-small;\".*\n.*text . \"\\([^\"]+\\)" nil t)
	(setq date (ignore-errors
		     (csid-parse-month-date (match-string 1))))
	(when (re-search-forward ":style . \"font-size: large;\".*\n.*text . \"\\([^\"]+\\)" nil t)
	  (when (and date
		     (= (length date) 10))
	    (push (list date
			"http://magnetenpub.blogspot.com/p/konsertkalender.html"
			(match-string 1))
		  result)))))
    (nreverse result)))

(defun csid-parse-nilsen (dom)
  (loop for row in (dom-by-class dom "program3sp")
	append (loop with date
		     for elem in (cdr row)
		     when (and (eq (car elem) 'p)
			       (equal (dom-attr elem :class) "arrdato"))
		     do (setq date (dom-text elem))
		     when (and (eq (car elem) 'p)
			       (equal (dom-attr elem :class) "arrheading"))
		     collect (list
			      (csid-parse-full-numeric-date date)
			      (shr-expand-url
			       (dom-attr (car (dom-by-name elem 'a)) :href))
			      (dom-text (car (dom-by-name elem 'a)))))))

(defun csid-parse-spektrum (dom)
  (loop for elem in (dom-by-name dom 'li)
	for a = (car (dom-by-name elem 'a))
	collect (list (csid-parse-full-numeric-date
		       (dom-text (car (dom-by-class elem "date"))))
		      (shr-expand-url (dom-attr a :href))
		      (dom-text a))))

(defun csid-parse-nymusikk (dom)
  (loop for elem in (dom-by-class dom "^tweet$")
	for a = (car (dom-by-name elem 'a))
	collect (list (csid-parse-full-numeric-date
		       (dom-text (car (dom-by-class elem "date"))))
		      (dom-attr a :href)
		      (dom-text a))))

(defun csid-parse-konserthuset (dom)
  (append
   (csid-parse-konserthuset-1 dom)
   (loop with time = (decode-time (current-time))
	 repeat 10
	 append (progn
		  (setcar (nthcdr 4 time) (1+ (nth 4 time)))
		  (when (> (nth 4 time) 12)
		    (setcar (nthcdr 4 time) 1)
		    (setcar (nthcdr 5 time) (1+ (nth 5 time))))
		  (csid-parse-source
		   (format-time-string
		    "http://oslokonserthus.no/public/eventschedule.jsp?month=%m&year=%Y"
		    (apply 'encode-time time))
		   'csid-parse-konserthuset-1 :html)))))

(defun csid-parse-konserthuset-1 (dom)
  (loop for row in (dom-by-name
		    (dom-parent
		     dom (dom-parent
			  dom (car (dom-by-class dom "scheduleHeadingTD"))))
		    'tr)
	for tds = (dom-by-name row 'td)
	when (not (string-match "scheduleHeading" (dom-attr (car tds) :class)))
	collect (list (csid-parse-numeric-date (dom-text (car tds)))
		      (shr-expand-url (dom-attr (car (dom-by-name row 'a))
						:href))
		      (dom-text (nth 2 tds)))))

(defun csid-parse-riksscenen (dom)
  (loop for date in (dom-by-class dom "event-date")
	for elem = (dom-parent dom date)
	for a = (car (dom-by-name elem 'a))
	when a
	collect (list (csid-parse-month-date (dom-text date))
		      (shr-expand-url (dom-attr a :href))
		      (dom-text a))))

(defun csid-parse-olsen (dom)
  (loop for elem in (dom-by-class dom "^ai1ec-event$")
	collect (list (csid-parse-short-reverse-yearless-month
		       (dom-texts (car (dom-by-class elem "ai1ec-event-time"))))
		      (dom-attr (car (dom-by-name elem 'a)) :href)
		      (csid-clean-string
		       (dom-texts
			(car (dom-by-class elem "ai1ec-event-title")))))))

(defun csid-parse-verkstedet (dom)
  (loop for elem in (dom-by-class dom "^event$")
	collect (list
		 (csid-parse-shortish-month
		  (format "%s %s %s"
			  (dom-text (car (dom-by-class elem "^day$")))
			  (dom-text (car (dom-by-class elem "^month$")))
			  (dom-text (car (dom-by-class elem "^year$")))))
		 "http://www.verkstedetbar.no/program/"
		 (dom-text (car (dom-by-name elem 'h3))))))

(defun csid-parse-gamla (dom)
  (loop for elem in (dom-by-class dom "event-small")
	for link = (car (dom-by-name elem 'a))
	collect (list
		 (csid-parse-shortish-month
		  (dom-text (car (dom-by-name elem 'h4))))
		 (dom-attr link :href)
		 (dom-attr link :title))))

(defun csid-parse-new (dom)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (pp dom (current-buffer))
  (goto-char (point-min)))

(defun csid-generate-html (&optional file)
  (let ((data
	 ;; Sort by dates, and then names.
	 (sort (sort (copy-list csid-database)
		     (lambda (e1 e2)
		       (string< (car e1) (car e2))))
	       (lambda (e1 e2)
		 (string< (cadr e1) (cadr e2)))))
	(coding-system-for-write 'utf-8)
	(now (format-time-string "%Y-%m-%d"))
	prev-date start)
    (with-temp-file (or file "/tmp/csid.html")
      (insert "<head><title>Crowdsourcing Is Dead</title><meta charset='utf-8'><link href='csid.css' rel='stylesheet' type='text/css'><body><img src='csid.png'><p>(Also known as <a href='http://lars.ingebrigtsen.no/2013/09/crowdsourcing-is-dead.html'>'Concerts In Oslo' or 'Konserter i Oslo'</a>.)</p><div id='selector'></div>")
      (insert "<table>")
      (setq start (point))
      (loop for (venue date url name id) in data
	    unless (string< date now)
	    do (insert (format "<tr class='%s date'><td colspan=3>%s</tr><tr name='%s' id='event-%s'><td><a href='%s'>%s<td>%s</tr>"
			       (if (equal prev-date date)
				   "invisible"
				 "visible")
			       (csid-add-weekday date)
			       (replace-regexp-in-string " " "_" venue)
			       id
			       url
			       (url-insert-entities-in-string
				(if (> (length name) 1000)
				    (substring name 0 1000)
				  name))
			       venue))
	    (setq prev-date date))
      (insert "</table>")
      (write-region start (point) "/tmp/csid-table.html")
      (insert "<script type='text/javascript' src='jquery-1.10.2.min.js'></script><script type='text/javascript' src='jquery.cookie.js'></script><script type='text/javascript' src='csid.js'></script>"))))

(defun csid-add-weekday (date)
  (let ((time (encode-time 0 0 0
			   (string-to-number (substring date 8))
			   (string-to-number (substring date 5 7))
			   (string-to-number (substring date 0 4)))))
    (format-time-string "%a %b %d" time)))

(defun csid-update-html (file &optional inhibit-fetch)
  (csid-read-database)
  (unless inhibit-fetch
    (csid-parse-sources))
  (csid-generate-html file))

(provide 'csid)

;;; csid.el ends here
