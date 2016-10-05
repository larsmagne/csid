;;; csid.el --- Generate Concert Listings
;; Copyright (C) 2013-2014 Lars Magne Ingebrigtsen

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
(require 'atom)

(defvar csid-database-file-name "~/.emacs.d/csid.data")

(defvar csid-sequence 0)

(defvar csid-sources
  '(("Revolver" "https://revolveroslo.ticketco.no/" ticketco)
    ("Blå" "http://www.blaaoslo.no/" blaa)
    ("Mir" "http://www.lufthavna.no/" mir)
    ;;("Crossroads" "https://www.facebook.com/thecrossroadclub/events?key=events" facebook)
    ("Victoria" "http://nasjonaljazzscene.no/arrangement/" victoria)
    ("Rockefeller" "http://rockefeller.no/index.html" rockefeller :multi)
    ("Mono" "http://www.cafemono.no/program/" mono)
    ("Parkteateret" "http://parkteatret.no/program/" parkteateret)
    ("Konsertforeninga" "http://www.konsertforeninga.no/konserter"
     konsertforeninga)
    ("Maksitaksi" "https://www.facebook.com/maksitaksii/events?ref=page_internal" facebook)
    ("Betong" "https://www.facebook.com/betongoslo/events" facebook)
    ("Bidrobon" "http://bidrobon.weebly.com/program-varingren-2016.html" bidrobon)
    ("Cosmopolite" "http://cosmopolite.no/program/cosmopolite" cosmopolite)
    ("Belleville" "http://cosmopolite.no/program/belleville" cosmopolite)
    ("Vulkan" "http://vulkanarena.no/shows" vulkan)
    ("Jakob" "http://www.jakob.no/program/" jakob)
    ("Vanguard" "http://www.fanrx.com/facebook/events.php?theme=custom&page=vanguardoslo&bgcolor=ffffff&textcolor=000000&linkcolor=555555&max=15" vanguard)
    ("Ultima" "http://ultima.no/program" ultima)
    ("Blitz" "http://www.blitz.no/kalender" blitz)
    ("Magneten" "http://magnetenpub.blogspot.no//feeds/pages/default?alt=json&v=2&dynamicviews=1"
     magneten :json :date)
    ("Herr Nilsen" "http://www.herrnilsen.no/program2009.html" nilsen)
    ("Spektrum" "http://www.oslospektrum.no/" spektrum)
    ("Ny Musikk" "http://nymusikk.no/no/hva-skjer/" nymusikk)
    ("Konserthuset" "http://www.oslokonserthus.no/program/json/all"
     konserthuset :json)
    ("Riksscenen" "http://www.riksscenen.no/program.95415.no.html" riksscenen)
    ("Olsen" "http://shop.olsenbar.no/program/" olsen)
    ;;("Verkstedet" "http://www.verkstedetbar.no/program/" verkstedet :date)
    ("Gamla" "http://www.gamla.no/" gamla)
    ;;("Sawol" "http://www.sawol.no/category/program/" sawol)
    ("Buckleys" "http://www.buckleys.no/konserter.html" buckleys :date)
    ;;("New Orleans" "http://www.neworleansworkshop.com/program" neworleans :date)
    ;;("NB" "http://www.nb.no/Hva-skjer/Arrangementer/Konserter" nasjonalbiblioteket)
    ("Uhørt" "https://www.facebook.com/uhortistroget/events" facebook)
    ("Kulturhuset" "https://www.facebook.com/kulturhusetioslo/events" facebook)
    ("Kampenjazz" "http://oysteineide.wix.com/kampenjazz#!konserter/cb30" kampenjazz :date)
    ("Cafeteatret" "http://nordicblacktheatre.no/wp-admin/admin-ajax.php?action=wpcal-getevents&end=1444600800&start=1440972000" cafeteatret :json)
    ("Telenor Arena" "http://telenorarena.no/en/events/" telenor)
    ("Postkontoret" "https://www.facebook.com/toyenpostkontor/events?key=events"
     facebook)
    ("Per på hjørnet" "http://www.pph.oslo.no/" pph :date)
    ("The Villa" "http://www.thevilla.no/program/" villa)
    ("Dattera" "http://www.dattera.no/nb/pages/6-Kalender" dattera)
    ("Internasjonalen" "http://www.internasjonalen.no/program/" internasjonalen)
    ("Jæger" "http://jaegeroslo.no/program/" jaeger)
    ("Union" "http://unionscene.no/program/" union)
    ("Musikkflekken" "https://www.facebook.com/Musikkflekken/events" facebook)
    ("Smelteverket" "https://www.facebook.com/Smelteverket/events" facebook)
    ("Skuret" "https://www.facebook.com/skuret/events" facebook)
    ("Pizdets" "https://www.facebook.com/pissjets/events" facebook)
    ("Fisk og Vilt" "https://www.facebook.com/fiskogviltoslo/events" facebook)
    ("Hvaskjer" "https://www.facebook.com/hvaskjertorshov/events" facebook)
    ("UiO" "http://www.uio.no/om/aktuelt/arrangementer/konserter/" uio)
    ;;("Mr Pizza" "http://www.mrpizza.no/" pizza :date)
    ("Sub Scene" "http://www.subscene.no/" subscene)
    ("Vigeland" "https://www.facebook.com/emanuelvigeland/events" facebook)
    ("Josefine" "http://josefinevise.no/" josefine)
    ("Izakaya" "https://www.facebook.com/Izakaya-343430575679537/events?ref=page_internal" facebook)
    ("Sentralen" "http://www.sentralen.no/arrangementer" sentralen)
    ("Ingensteds" "https://ingensteds.ticketco.no/" ticketco)
    ("Enga" "https://www.facebook.com/Enga-419256284860268/events?ref=page_internal" facebook)
    ("Big Dipper" "https://www.facebook.com/Platebutikken-Big-Dipper-428305525645/events?ref=page_internal" facebook)
    ("Krøsset" "https://www.facebook.com/krxsset/events?ref=page_internal" facebook)
    ))

(defvar csid-database nil)

(defun csid-write-database (data)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file csid-database-file-name
      (pp data (current-buffer))))
  nil)

(defun csid-update-database (data)
  (dolist (elem data)
    (if (not (and (nth 1 elem)
		  (csid-valid-date-p (nth 1 elem))))
	(message "Invalid date for %S" elem)
      (setcar (nthcdr 3 elem) (csid-clean-string (nth 3 elem)))
      (let ((old
	     (cl-member (nth 4 elem) csid-database
			:key (lambda (event)
			       (nth 4 event)))))
	(if (not old)
	    ;; Facebook sometimes seem to return the wrong data?
	    ;; Don't add a new show if we already have the URL.
	    (unless (cl-member #'caddr csid-database
			       :key #'caddr)
	      (push elem csid-database))
	  ;; Don't update anything if we're using the date as the key,
	  ;; because then multiple events on the same date will be
	  ;; overwritten.
	  (unless (memq :date (assoc (car elem) csid-sources))
	    ;; Update the title.
	    (when (plusp (length (nth 3 elem)))
	      (setcar (nthcdr 3 (car old)) (nth 3 elem)))
	    ;; Update the date.
	    (when (plusp (length (nth 1 elem)))
	      (setcar (nthcdr 1 (car old)) (nth 1 elem))))))))
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
  ;; When calling interactively, clear out the list for easier debugging.
  (when type
    (setq csid-database nil))
  (csid-write-database
   (csid-update-database
    (loop for source in csid-sources
	  for (name url function) = source
	  for function = (intern (format "csid-parse-%s" function) obarray)
	  do (message "%s" name)
	  when (or (not type)
		   (string= type name))
	  append (let* ((max-specpdl-size 6000)
			(max-lisp-eval-depth 6000)
			(results
			 (if type
			     (csid-parse-source
			      url
			      (if (fboundp function)
				  function
				'csid-parse-new)
			      (if (memq :json source)
				  :json
				:html))
			   (progn
			     (csid-parse-source
			      url
			      (if (fboundp function)
				  function
				'csid-parse-new)
			      (if (memq :json source)
				  :json
				:html))))))
		   (unless results
		     (message "No results for type %s" name))
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
    (append elem (list (or found (incf csid-sequence))
		       (format-time-string "%FT%T")))))

(defun csid-retrieve-synchronously (url &optional silent inhibit-cookies)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL."
  (url-do-setup)

  (let ((retrieval-done nil)
	(start-time (current-time))
        (asynch-buffer nil))
    (setq asynch-buffer
	  (url-retrieve url (lambda (&rest ignored)
			      (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
			      (setq retrieval-done t
				    asynch-buffer (current-buffer)))
			nil silent inhibit-cookies))
    (if (null asynch-buffer)
        ;; We do not need to do anything, it was a mailto or something
        ;; similar that takes processing completely outside of the URL
        ;; package.
        nil
      (let ((proc (get-buffer-process asynch-buffer)))
	;; If the access method was synchronous, `retrieval-done' should
	;; hopefully already be set to t.  If it is nil, and `proc' is also
	;; nil, it implies that the async process is not running in
	;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
	;; url-file.el should probably set something like a `url-process'
	;; buffer-local variable so we can find the exact process that we
	;; should be waiting for.  In the mean time, we'll just wait for any
	;; process output.
	(while (and (not retrieval-done)
		    (< (float-time (time-subtract (current-time) start-time))
		       20))
	  (url-debug 'retrieval
		     "Spinning in url-retrieve-synchronously: %S (%S)"
		     retrieval-done asynch-buffer)
          (if (buffer-local-value 'url-redirect-buffer asynch-buffer)
              (setq proc (get-buffer-process
                          (setq asynch-buffer
                                (buffer-local-value 'url-redirect-buffer
                                                    asynch-buffer))))
            (if (and proc (memq (process-status proc)
                                '(closed exit signal failed))
                     ;; Make sure another process hasn't been started.
                     (eq proc (or (get-buffer-process asynch-buffer) proc)))
                ;; FIXME: It's not clear whether url-retrieve's callback is
                ;; guaranteed to be called or not.  It seems that url-http
                ;; decides sometimes consciously not to call it, so it's not
                ;; clear that it's a bug, but even then we need to decide how
                ;; url-http can then warn us that the download has completed.
                ;; In the mean time, we use this here workaround.
		;; XXX: The callback must always be called.  Any
		;; exception is a bug that should be fixed, not worked
		;; around.
		(progn ;; Call delete-process so we run any sentinel now.
		  (delete-process proc)
		  (setq retrieval-done t)))
            ;; We used to use `sit-for' here, but in some cases it wouldn't
            ;; work because apparently pending keyboard input would always
            ;; interrupt it before it got a chance to handle process input.
            ;; `sleep-for' was tried but it lead to other forms of
            ;; hanging.  --Stef
            (unless (or (with-local-quit
			  (accept-process-output proc 1))
			(null proc))
              ;; accept-process-output returned nil, maybe because the process
              ;; exited (and may have been replaced with another).  If we got
	      ;; a quit, just stop.
	      (when quit-flag
		(delete-process proc))
              (setq proc (and (not quit-flag)
			      (get-buffer-process asynch-buffer)))))))
      asynch-buffer)))

(defun csid-parse-source (url function data-type)
  (with-current-buffer (csid-retrieve-synchronously url t t)
    (goto-char (point-min))
    (prog1
	(when (search-forward "\n\n" nil t)
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
		       (ignore-errors (json-read)))
		      (t
		       (libxml-parse-html-region (point) (point-max)))))))
      (kill-buffer (current-buffer)))))

(defun csid-parse-revolver (dom)
  (loop for elem in (dom-by-class dom "views-table")
	for date = (dom-attr (dom-by-class elem "date-display-single") 'content)
	for link = (dom-by-tag elem 'a)
	collect (list (substring date 0 10)
		      (shr-expand-url (dom-attr link 'href))
		      (dom-text link))))

(defun csid-parse-blaa (dom)
  (loop for month in (dom-by-class dom "^month$")
	for month-name = (dom-texts (dom-by-tag month 'h1))
	append (loop for day in (dom-by-class month "^day$")
		     append (loop for elem in (dom-by-tag day 'article)
		     for h1 = (dom-by-tag elem 'h1)
		     when elem
		     collect (list
			      (csid-parse-month-date
			       (format "%s %s"
				       (dom-text (dom-by-class day "^number$"))
				       month-name))
			      (dom-attr (dom-by-tag h1 'a) 'href)
			      (dom-texts h1))))))

;; Date parsers.

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

;; "Fredag 27. september" but in a six month window
(defun csid-parse-month-date-window (string)
  (setq string (downcase string))
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\)"
			    (mapconcat 'identity csid-months "\\|"))
		    string)
      (csid-expand-date-window
       (1+ (position (match-string 2 string) csid-months
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

;; "onsdag 19. november 2014"
(defun csid-parse-norwegian-month-date-with-year (string)
  (setq string (downcase string))
  (when (string-match (format "\\([0-9]+\\).*?\\(%s\\).*?\\([0-9]+\\)"
			      (mapconcat 'identity csid-months "\\|"))
		      string)
    (format "%04d-%02d-%02d"
	    (string-to-number (match-string 3 string))
	    (1+ (position (match-string 2 string) csid-months
			  :test 'equalp))
	    (string-to-number (match-string 1 string)))))

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
(defun csid-parse-short-month (string &optional englishp)
  (if (string-match (format "\\([0-9]+\\).*\\(%s\\) +\\([0-9]+\\)"
			    (mapconcat
			     (lambda (month)
			       (substring month 0 3))
			     (if englishp
				 csid-english-months
			       csid-months)
			     "\\|"))
		    string)
      (format "%s-%02d-%02d"
	      (match-string 3 string)
	      (1+ (position (match-string 2 string)
			    (mapcar
			     (lambda (month)
			       (substring month 0 3))
			     (if englishp
				 csid-english-months
			       csid-months))
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
  (when (string-match (format "\\([0-9]+\\)[^0-9]+\\(%s\\)"
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
(defun csid-parse-short-reverse-yearless-month (string &optional englishp)
  (setq string (replace-regexp-in-string "\n" " " string))
  (when (string-match (format "\\(%s\\).*?\\([0-9]+\\)"
			      (mapconcat
			       (lambda (month)
				 (substring month 0 3))
			       (if englishp
				   csid-english-months
				 csid-months)
			       "\\|"))
		      string)
    (csid-expand-date
     (1+ (position (match-string 1 string)
		   (mapcar
		    (lambda (month)
		      (substring month 0 3))
		    (if englishp
			csid-english-months
		      csid-months))
		   :test 'equalp))
     (string-to-number (match-string 2 string)))))


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

;; 23/09
(defun csid-parse-slashed-date (string)
  (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)" string)
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

(defun csid-expand-date (month day &optional this-year-only)
  (let ((this-year (nth 5 (decode-time)))
	(this-month (nth 4 (decode-time))))
    (when (and (not this-year-only)
	       (< month this-month))
      (incf this-year))
    (format "%s-%02d-%02d" this-year month day)))

(defun csid-expand-date-window (month day &optional this-year-only)
  (let* ((this-year (nth 5 (decode-time)))
	 (year
	  (cadar
	   (sort
	    (loop for year in (list (1- this-year) this-year (1+ this-year))
		  collect (list (abs (- (float-time
					 (parse-iso8601-time-string
					  (format "%s-%02d-%02dT05:05:05"
						  year month day)))
					(float-time)))
				year))
	    (lambda (a1 a2)
	      (< (car a1) (car a2)))))))
    (if this-year-only
	(format "%s-%02d-%02d" this-year month day)
      (format "%s-%02d-%02d" year month day))))

(defun csid-parse-mir (dom)
  (loop for elem in (dom-by-id dom "program")
	for link = (dom-by-tag (dom-by-class elem "programtittel") 'a)
	collect (list (csid-parse-month-date
		       (dom-text (dom-by-class elem "programtid")))
		      (dom-attr link 'href)
		      (dom-attr link 'title))))

(defun csid-parse-facebook (dom)
  (let ((id (loop for meta in (dom-by-tag dom 'meta)
		  for content = (dom-attr meta 'content)
		  when (and content
			    (string-match "fb.*/\\([0-9]+\\)$" content))
		  return (match-string 1 content))))
    (when id
      (csid-parse-facebook-1 (csid-facebook id)))))

(defun csid-parse-facebook-1 (json)
  (loop with edges
	for entry in (cadr (assq 'response (car json)))
	when (and (consp entry)
		  (consp (cdr entry))
		  (setq edges (assq 'edges entry)))
	append (loop for e across (cdr edges)
		     for event = (cdr (assq 'node e))
		     collect
		     (list
		      (csid-parse-iso8601
		       (cdr (car (cdr (assq 'time_range event)))))
		      (cdr (assq 'url event))
		      (cdr (assq 'name event))))))

(defun csid-parse-victoria (dom)
  (loop for elem in (dom-by-class dom "event-entry")
	for date = (dom-by-class elem "show-for-small")
	collect (list (csid-parse-numeric-date
		       (dom-text (dom-by-tag date 'p)))
		      (dom-attr (dom-by-tag elem 'a) 'href)
		      (dom-text (dom-by-tag elem 'h2)))))

(defun csid-parse-rockefeller (dom)
  (loop for elem in (dom-by-class dom "^show bkg")
	collect (list (csid-parse-rockefeller-stage
		       (dom-attr (dom-by-class elem "^sknapp ") 'class))
		      (csid-parse-full-numeric-date
		       (dom-texts (dom-by-class elem "datofelt")))
		      (shr-expand-url (dom-attr (dom-by-tag elem 'a) 'href))
		      (csid-clean-string
		       (dom-texts (dom-by-class elem "showtitle"))))))

(defun csid-parse-rockefeller-stage (class)
  (cond
   ((string-match "sc_P" class)
    "Bushwick")
   ((string-match "sc_L" class)
    "Leiligheten")
   ((string-match "sc_R" class)
    "Rockefeller")
   ((string-match "sc_J" class)
    "John Dee")
   ((string-match "sc_S" class)
    "Sentrum")
   (t
    "Rockefeller")))

(defun csid-parse-mono (dom)
  (loop for event in (dom-by-class
		      (dom-by-id dom "^prikketabell_program$")
		      "^event_wrapper$")
	collect (list (csid-parse-numeric-date
		       (dom-text (dom-by-class event "event_date")))
		      (shr-expand-url (dom-attr (dom-by-tag event 'a) 'href))
		      (csid-clean-string
		       (dom-text (dom-by-class event "event_title"))))))

(defun csid-parse-parkteateret (dom)
  (loop for elem in (dom-by-class dom "concert-item")
	collect (list (csid-parse-month-date
		       (dom-texts (dom-by-class elem "concert-date")))
		      (dom-attr (dom-by-tag elem 'a) 'href)
		      (dom-texts (dom-by-tag elem 'h2)))))

(defun csid-parse-konsertforeninga (dom)
  (loop for elem in (dom-by-tag (dom-by-tag dom 'table) 'tr)
	for tds = (dom-by-tag elem 'td)
	for link = (dom-by-tag (nth 2 tds) 'a)
	collect (list (csid-parse-month-date (car (last (nth 0 tds))))
		      (shr-expand-url (dom-attr link 'href))
		      (dom-text link))))

(defun csid-parse-betong (dom)
  (loop for elem in (dom-by-tag
		     (car (dom-by-class dom "^table$"))
		     'tr)
	for tds = (dom-by-tag elem 'td)
	for link = (dom-by-tag (nth 1 tds) 'a)
	when (and link
		  (string-match "konsert" (dom-text (nth 3 tds))))
	collect (list (csid-parse-numeric-date (dom-text (nth 0 tds)))
		      (dom-attr link 'href)
		      (dom-text link))))

(defun csid-parse-bidrobon (dom)
  (loop for event in (dom-by-tag (dom-by-class dom "wsite-content") 'a)
	for date = (csid-parse-short-yearless-month (dom-texts event))
	when (and date
		  (csid-date-likely-p date))
	collect (list date
		      (shr-expand-url (dom-attr event 'href))
		      (dom-texts event))))

(defun csid-clean-string (string)
  (replace-regexp-in-string "^[\r\n\t ]\\|[\r\n\t ]$" "" 
			    (replace-regexp-in-string "[\r\n\t ]+" " " string)))

(defun csid-parse-cosmopolite (dom)
  (loop for elem in (dom-by-class dom "concert-item")
	for day = (dom-text (dom-by-class elem "day-of-the-month"))
	for month = (dom-text (dom-by-class elem "^month$"))
	collect (list (csid-parse-short-yearless-month
		       (format "%s %s" day month))
		      (shr-expand-url (dom-attr (dom-by-tag elem 'a) 'href))
		      (csid-clean-string (dom-texts (dom-by-tag elem 'h2))))))

(defun csid-parse-vulkan (dom)
  (loop for elem in (dom-by-tag dom 'article)
	for date = (dom-attr elem 'data-starts-at)
	when date
	collect (list (csid-parse-iso8601 date)
		      (shr-expand-url (dom-attr (dom-by-tag elem 'a) 'href))
		      (dom-attr elem 'data-title))))

(defun csid-parse-jakob (dom)
  (loop for elem in (dom-by-class dom "eventItem")
	for day = (csid-clean-string
		   (dom-texts (dom-by-class elem "day")))
	for month = (csid-clean-string
		     (dom-texts (dom-by-class elem "month")))
	collect (list (csid-parse-short-yearless-month
		       (format "%s %s" day month))
		      (shr-expand-url
		       (dom-attr (dom-by-class elem "more") 'href))
		      (csid-clean-string
		       (dom-texts (dom-by-tag elem 'h2))))))

(defun csid-parse-vanguard (dom)
  (loop for elem in (dom-by-style dom "clear:both")
	for a = (dom-by-tag elem 'a)
	when (plusp (length (dom-texts a)))
	collect (list (csid-parse-rfc2822
		       (dom-texts (dom-by-class elem "smalltext")))
		      (dom-attr a 'href)
		      (dom-texts a))))

(defun csid-parse-ultima (dom)
  (loop for elem in (dom-by-class dom "program_list_title")
	for event = (dom-parent dom (dom-parent dom elem))
	for link = (dom-attr (dom-by-tag event 'a) 'href)
	when link
	collect (list (csid-parse-full-numeric-date (dom-attr event 'data-day))
		      link
		      (dom-attr event 'data-title))))
   
(defun csid-parse-vcalendar (url)
  (with-current-buffer (csid-retrieve-synchronously url t t)
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match "" t t))
    (goto-char (point-min))
    (when (search-forward "\n\n")
      (loop for event in (dom-by-tag
			  (vcalendar-parse-region (point) (point-max))
			  'vevent)
	    collect
	    (list (csid-parse-compact-iso8601
		   (dom-attr (dom-by-tag event 'dtstart) 'value))
		  (dom-attr (dom-by-tag event 'url) 'value)
		  (dom-attr (dom-by-tag event 'summary) 'value))))))

(defun csid-parse-blitz (dom)
  (loop for elem in (dom-by-class dom "views-row")
	for month = (dom-text (dom-by-class elem "calendar-date-month"))
	for day = (dom-text (dom-by-class elem "calendar-date-day"))
	for link = (dom-by-tag elem 'a)
	when (plusp (length month))
	collect (list (csid-parse-short-yearless-month
		       (format "%s %s" day month)
		       t)
		      (shr-expand-url (dom-attr link 'href))
		      (dom-text link))))

(defun csid-parse-magneten (data)
  (let ((html
	 (cdr (assq '$t
		    (cdr
		     (assq 'content (aref (cdr (assq 'entry (cdr (assq 'feed data))))
					  0)))))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert html)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (set-buffer-multibyte t)
      (csid-parse-magneten-html
       (libxml-parse-html-region (point-min) (point-max))))))

;; Magneten's list is apparently hand-written, but this seems to do
;; the trick.
(defun csid-parse-magneten-html (dom)
  (let (result date)
    (with-temp-buffer
      (pp dom (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward "style . \"font-size: x-small;\".*\n.*?\"\\([^\"]+\\)" nil t)
	(setq date (ignore-errors
		     (csid-parse-month-date (match-string 1))))
	(when (re-search-forward "style . \"font-size: large;\".*\n.*?\"\\([^\"]+\\)" nil t)
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
		     for elem in (dom-non-text-children row)
		     when (and (eq (car elem) 'p)
			       (equal (dom-attr elem 'class) "arrdato"))
		     do (setq date
			      (csid-parse-full-numeric-date (dom-text elem)))
		     when (and (eq (car elem) 'p)
			       (equal (dom-attr elem 'class) "arrheading")
			       (csid-date-likely-p date))
		     collect (list
			      date
			      (shr-expand-url
			       (dom-attr (dom-by-tag elem 'a) 'href))
			      (dom-text (dom-by-tag elem 'a))))))

(defun csid-date-likely-p (date)
  (time-less-p
   (apply 'encode-time (mapcar
			(lambda (elem)
			  (or elem 0))
			(parse-time-string date)))
   ;; Stuff that's more than four months in the future is unlikely
   ;; for some venues.
   (time-add (current-time) (* 4 30 24 60 60))))

(defun csid-parse-spektrum (dom)
  (loop for elem in (dom-by-tag dom 'li)
	for a = (dom-by-tag elem 'a)
	for date = (csid-parse-full-numeric-date
		    (dom-text (dom-by-class elem "date")))
	when (csid-valid-date-p date)
	collect (list date
		      (shr-expand-url (dom-attr a 'href))
		      (dom-text a))))

(defun csid-parse-nymusikk (dom)
  (loop for elem in (dom-by-class dom "^tweet$")
	for a = (dom-by-tag elem 'a)
	collect (list (csid-parse-full-numeric-date
		       (dom-text (dom-by-class elem "date")))
		      (dom-attr a 'href)
		      (dom-text a))))

(defun csid-parse-konserthuset (json)
  (loop for event across json
	collect (list (csid-parse-iso8601
		       (cdr (assq 'datetime (assq 'dato event))))
		      (cdr (assq 'url event))
		      (cdr (assq 'title event)))))

(defun csid-parse-riksscenen (dom)
  (loop for date in (dom-by-class dom "event-date")
	for elem = (dom-parent dom date)
	for a = (dom-by-tag elem 'a)
	when a
	collect (list (csid-parse-month-date (dom-text date))
		      (shr-expand-url (dom-attr a 'href))
		      (dom-text a))))

(defun csid-parse-olsen (dom)
  (loop for event in (dom-by-class dom "m-calender-event")
	for link = (dom-by-tag event 'a)
	collect (list (csid-parse-full-numeric-date
		       (dom-texts (dom-by-class event "from-date")))
		      (shr-expand-url (dom-attr link 'href))
		      (dom-attr link 'title))))

(defun csid-parse-verkstedet (dom)
  (loop for elem in (dom-by-class dom "^event$")
	collect (list
		 (csid-parse-shortish-month
		  (format "%s %s %s"
			  (dom-text (dom-by-class elem "^day$"))
			  (dom-text (dom-by-class elem "^month$"))
			  (dom-text (dom-by-class elem "^year$"))))
		 "http://www.verkstedetbar.no/program/"
		 (dom-text (dom-by-tag elem 'h3)))))

(defun csid-parse-gamla (dom)
  (loop for elem in (dom-by-class dom "event-small")
	for link = (dom-by-tag elem 'a)
	collect (list
		 (csid-parse-shortish-month (dom-text (dom-by-tag elem 'h4)))
		 (dom-attr link 'href)
		 (dom-attr link 'title))))

(defun csid-parse-sawol (dom)
  (append
   (loop for elem in (dom-by-class dom "category-program")
	 for link = (dom-by-tag elem 'a)
	 when (dom-attr link 'title)
	 collect (list (csid-parse-short-month
			(format "%s %s"
				(dom-text (dom-by-class elem "dayInfo"))
				(dom-text (dom-by-class elem "monthInfo"))))
		       (dom-attr link 'href)
		       (dom-attr link 'title)))
   (let* ((next (dom-by-id dom "^nextpage$"))
	  (link (and next (dom-attr (dom-by-tag next 'a) 'href))))
     (when link
       (csid-parse-source link 'csid-parse-sawol :html)))))

(defun csid-parse-buckleys (dom)
  (loop for elem in (dom-by-tag dom 'h2)
	for event = (dom-parent dom (car (dom-by-tag elem 'br)))
	for name = (cadr (dom-strings event))
	for date = (csid-parse-short-yearless-month (dom-texts elem))
	when (and name
		  (csid-valid-date-p date)
		  (csid-date-likely-p date))
	collect (list date
		      "http://www.buckleys.no/kommende-konserter.html"
		      (csid-clean-string name))))

(defun csid-parse-neworleans (dom)
  (loop with year = (let ((year (dom-text (dom-by-tag dom 'title))))
		      (and (string-match "[0-9][0-9][0-9][0-9]" year)
			   (match-string 0 year)))
	for title in (dom-by-class dom "views-field-title")
	for elem = (dom-parent dom (dom-parent dom title))
	for month = (dom-by-class elem "views-field-field_dato_1")
	when (and (eq (car elem) 'div)
		  month
		  year)
	collect (list (csid-parse-month-date-with-year
		       (format "%s %s %s"
			       (dom-texts month)
			       (dom-texts
				(dom-by-class elem "views-field-field_dato$"))
			       year))
		      "http://www.neworleansworkshop.com/program"
		      (dom-texts title))))

(defun csid-parse-nasjonalbiblioteket (dom)
  (loop for elem in (dom-by-class dom "kalendar-item")
	for link = (dom-by-tag elem 'a)
	collect (list (csid-parse-short-yearless-month
		       (csid-clean-string
			(dom-texts (dom-by-class elem "item-date"))))
		      (shr-expand-url (dom-attr link 'href))
		      (csid-clean-string (dom-texts link)))))

(defun csid-parse-kampenjazz (dom)
  (loop for elem in (dom-by-tag dom 'script)
	for script = (dom-texts elem)
	when (string-match "KONSERTER.*?\\(http:[^\"]+\\)" script)
	return (csid-parse-source (replace-regexp-in-string
				   "\\\\" "" (match-string 1 script))
				  'csid-parse-kampenjazz-1
				  'html)))

(defun csid-ensure-date (string)
  (and (string-match "^[-0-9]+$" string)
       (string-match "[0-9]" string)
       string))

(defun csid-parse-kampenjazz-1 (dom)
  (loop for event in (append (dom-by-class dom "backcolor_5")
			     (dom-by-class dom "backcolor_18"))
	for date = (or (csid-parent-date dom event 'dom-previous-sibling)
		       (csid-parent-date dom event 'dom-parent))
	when (and date
		  (csid-date-likely-p date))
	collect (list date
		      "http://oysteineide.wix.com/kampenjazz#!konserter/cb30"
		      (dom-texts event))))

(defun csid-parent-date (dom node func)
  (loop with date
	while (and node
		   (not date))
	do (setq date (csid-ensure-date
		       (csid-parse-month-date (dom-texts node))))
	when date
	return date
	do (setq node (funcall func dom node))))

(defun csid-parse-cafeteatret (json)
  (loop for event across json
	for title = (cdr (assq 'title event))
	for url = (cdr (assq 'post_url event))
	when (and (string-match "Kampenjazz" title)
		  (> (length url) 0))
	collect (list (csid-parse-iso8601 (cdr (assq 'start event)))
		      url
		      (replace-regexp-in-string
		       "Kampenjazz presenterer:? +" "" title))))

(defun csid-parse-telenor (dom)
  (loop for event in (dom-by-class dom "^module-content$")
	collect (list (csid-parse-short-month
		       (dom-texts (dom-by-class event "event-date"))
		       t)
		      (dom-attr
		       (dom-by-tag (dom-parent dom event) 'a)
		       'href)
		      (csid-clean-string
		       (dom-texts (dom-by-class event "event-title"))))))

(defun csid-parse-pph (dom)
  (loop with date
	for event in (dom-by-tag (dom-by-class dom "programbg") 'p)
	for texts = (loop for child in (dom-children event)
			  when (stringp child)
			  collect child
			  when (and (not (stringp child))
				    (plusp (length (dom-text child))))
			  collect (dom-text child))
	when (and (= (length texts) 2)
		  (setq date (csid-parse-numeric-date (car texts)))
		  (csid-valid-date-p date)
		  (csid-date-likely-p date))
	collect (list date
		      "http://www.pph.oslo.no/"
		      (csid-clean-string (cadr texts)))))

(defun csid-parse-villa (dom)
  (loop for event in (dom-by-class dom "^group$")
	for link = (dom-by-tag (dom-by-class event "sub-head") 'a)
	when (dom-attr link 'href)
	collect (list (csid-parse-short-month
		       (dom-texts (dom-by-tag event 'p)) t)
		      (dom-attr link 'href)
		      (dom-texts link))))

(defun csid-parse-dattera (dom)
  (loop for day in (dom-by-class dom "^date$")
	append (loop for event in (dom-by-tag day 'h3)
		     for text = (dom-texts event)
		     collect (list (csid-parse-month-date
				    (dom-text (dom-by-tag day 'h4)))
				   (shr-expand-url
				    (dom-attr (dom-by-tag event 'a) 'href))
				   text))))

(defun csid-parse-internasjonalen (dom)
  (loop for event in (dom-by-class dom "^event$")
	for date-node = (loop with prev = event
			      do (setq prev (dom-previous-sibling dom prev))
			      while (or (stringp prev)
					(not (eq (dom-tag prev) 'h3)))
			      finally (return prev))
	for link = (dom-by-tag (dom-by-tag event 'h2) 'a)
	for date = (csid-parse-month-date-window (dom-texts date-node))
	while (string> date (format-time-string "%Y-%m-%d"))
	when (csid-date-likely-p date)
	collect (list date
		      (dom-attr link 'href)
		      (dom-text link))))

(defun csid-parse-jaeger (dom)
  (loop for event in (dom-by-class dom "program_right")
	for link = (dom-by-tag (dom-by-tag event 'h1) 'a)
	for date = (csid-parse-month-date (dom-texts (dom-by-tag event 'h7)))
	when (and (csid-valid-date-p date)
		  (csid-date-likely-p date))
	collect (list date
		      (dom-attr link 'href)
		      (dom-text link))))

(defun csid-parse-union (dom)
  (append
   (csid-parse-union-1 dom)
   (loop for i from 15 upto 100 by 15
	 append (csid-parse-source
		 (format "http://unionscene.no/program/P%d" i)
		 'csid-parse-union-1
		 'html))))

(defun csid-parse-union-1 (dom)
  (loop for event in (dom-by-class dom "listitem")
	for link = (dom-by-tag (dom-by-tag event 'h2) 'a)
	for date = (csid-parse-month-date
		    (dom-text (dom-by-class event "date")))
	when (and (csid-valid-date-p date)
		  (csid-date-likely-p date))
	collect (list date
		      (dom-attr link 'href)
		      (dom-text link))))

(defun csid-parse-uio (dom)
  (loop for event in (dom-by-class dom "vevent")
	for title = (dom-by-class event "vrtx-title summary")
	collect (list (csid-parse-iso8601
		       (dom-attr (dom-by-class event "dtstart") 'title))
		      (dom-attr title 'href)
		      (dom-texts title))))

(defun csid-parse-pizza (dom)
  (loop with date
	for event in (dom-by-tag dom 'p)
	for text = (dom-text event)
	when (and text
		  (string-match
		   (concat "^\\(" (mapconcat 'identity csid-weekdays "\\|")
			   "\\)")
		   text)
		  (setq date (csid-parse-month-date text))
		  (csid-valid-date-p date)
		  (csid-date-likely-p date))
	collect (list date
		      "http://www.mrpizza.no/"
		      (replace-regexp-in-string " " " " text))))

(defun csid-parse-subscene (dom)
  (loop for event in (dom-by-class dom "^post-")
	for title = (dom-by-tag (dom-by-tag event 'h1) 'a)
	for date = (csid-parse-month-date (dom-texts (dom-by-tag event 'h3)))
	for class = (dom-texts (dom-by-class event "cat-links"))
	when (and class
		  (string-match "Arrangementer" class)
		  (csid-valid-date-p date))
	collect (list date
		      (dom-attr title 'href)
		      (dom-texts title))))

(defun csid-parse-josefine (dom)
  (loop for event in (dom-by-tag (dom-by-id dom "^northsidebar$") 'a)
	for text = (dom-texts event)
	for date = (csid-parse-month-date text)
	when (csid-valid-date-p date)
	collect (list date
		      (dom-attr event 'href)
		      (replace-regexp-in-string "^[^:]+: +" "" text))))

(defun csid-parse-ticketco (dom)
  (loop for event in (dom-by-class dom "^tc-events-list--item$")
	for elem = (dom-by-tag (dom-by-tag event 'h3) 'a)
	for date = (csid-parse-full-numeric-date
		    (dom-texts (dom-by-class
				event "^tc-events-list--place-time$")))
	when (csid-valid-date-p date)
	collect (list date
		      (dom-attr elem 'href)
		      (dom-texts elem))))

(defun csid-parse-sentralen (dom)
  (append
   (csid-parse-sentralen-1 dom)
   (let ((next (dom-by-class dom "^event-list__more-wrapper$")))
     (when next
       (csid-parse-source (dom-attr (dom-by-tag next 'a) 'href)
			  'csid-parse-sentralen
			  'html)))))

(defun csid-parse-sentralen-1 (dom)
  (loop for event in (dom-by-class dom "^event-item$")
	for date = (csid-parse-numeric-date
		    (dom-texts (dom-by-class event "^event-item__date$")))
	for url = (dom-attr (dom-by-tag event 'a) 'href)
	when (and (csid-valid-date-p date)
		  (csid-sentralen-wanted-p url))
	collect (list date url
		      (dom-texts (dom-by-tag event 'h3)))))

(defun csid-sentralen-wanted-p (url)
  (let* ((file "~/.emacs.d/sentralen.data")
	 (data (and (file-exists-p file)
		    (with-temp-buffer
		      (insert-file-contents file)
		      (read (current-buffer)))))
	 (elem (assoc url data)))
    (if elem
	(cdr elem)
      (let ((wantedp (csid-sentralen-wanted-p-1 url)))
	(push (cons url wantedp) data)
	(with-temp-buffer
	  (pp data (current-buffer))
	  (write-region (point-min) (point-max) file nil 'silent))
	wantedp))))

(defun csid-sentralen-wanted-p-1 (url)
  (with-current-buffer (csid-retrieve-synchronously url)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	   (time (dom-texts (dom-by-class
			     dom "^article--event__details__time$"))))
      (kill-buffer (current-buffer))
      (and time
	   (>= (csid-clock-to-seconds time)
	       (csid-clock-to-seconds "19.00"))))))

(defun csid-clock-to-seconds (string)
  (if (string-match "\\([0-9][0-9]\\)[^0-9]\\([0-9][0-9]\\)" string)
      (+ (* (string-to-number (match-string 1 string)) 60 60)
	 (* (string-to-number (match-string 1 string)) 60))
    0))

(defun csid-valid-date-p (date)
  (and (= (length date) 10)
       (string-match "^[-0-9]+$" date)))

(defun csid-parse-new (dom)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (dom-pp dom t)
  (goto-char (point-min))
  (error "Parsed"))

(defun csid-number-database ()
  (loop for elem in (copy-list csid-database)
	for i from 1
	collect (append elem (list i))))

(defun csid-generate-html (&optional file)
  (let ((data
	 ;; Sort by dates, and then names.
	 (sort (sort (csid-number-database)
		     (lambda (e1 e2)
		       (string< (car e1) (car e2))))
	       (lambda (e1 e2)
		 (string< (cadr e1) (cadr e2)))))
	(coding-system-for-write 'utf-8)
	(now (format-time-string "%Y-%m-%d"))
	prev-date start)
    (with-temp-file (or file "/tmp/csid.html")
      (insert "<head><title>Crowdsourcing Is Dead</title><meta charset='utf-8'><link href='csid.css' rel='stylesheet' type='text/css'><meta name='viewport' content='width=device-width, initial-scale=1'><link href='pikaday.css' rel='stylesheet' type='text/css'><link rel='icon' href='http://csid.no/favicon.ico'><body><div id='body-container'><div id='large-heading'><img src='csid.png' id='logo'><p>(Also known as <a href='http://lars.ingebrigtsen.no/2013/09/crowdsourcing-is-dead.html'>'Concerts In Oslo' or 'Konserter i Oslo'</a>.)</p></div><div id='small-heading'><img src='menu.png' id='small-menu'>Crowdsourcing Is Dead</div>")
      (insert "<table class='events'><colgroup><col class='band'><col class='venue'><col class='button'></colgroup>")
      (setq start (point))
      (loop with prev-date
	    for (venue date url name id fetch-date rank) in data
	    unless (string< date now)
	    do (progn
		 (unless (equal date prev-date)
		   (insert (format "<tr class='%s date'><td colspan=3>%s</tr>"
				   (if (equal prev-date date)
				       "invisible"
				     "visible")
				   (csid-add-weekday date))))
		 (setq prev-date date)
		 (insert (format "<tr name='%s' id='event-%s' data=%s date='%s' time='%s'><td><a href='%s'>%s<td>%s</tr>"
				 (replace-regexp-in-string " " "_" venue)
				 id
				 rank date fetch-date
				 url
				 (csid-make-text-breakable
				  (if (> (length name) 1000)
				      (substring name 0 1000)
				    name))
				 venue)))
	    (setq prev-date date))
      (insert "</table><div id='selector'></div>")
      (write-region start (point) "/tmp/csid-table.html")
      (insert "<script type='text/javascript' src='jquery-1.10.2.min.js'></script><script type='text/javascript' src='jquery.cookie.js'></script><script type='text/javascript' src='jquery.colorbox-min.js'></script><script type='text/javascript' src='FileSaver.min.js'></script><script type='text/javascript' src='csid.js'></script><script type='text/javascript' src='pikaday.js'></script><script type='text/javascript'>addNavigation();</script>"))))

(defun csid-make-text-breakable (string)
  (mapconcat
   'identity
   (loop for word in (split-string string)
	 collect (if (> (length word) 30)
		     (mapconcat
		      'identity
		      (loop for i from 0 upto (/ (length word) 30)
			    collect
			    (url-insert-entities-in-string
			     (substring word (* i 30)
					(min (* (1+ i) 30)
					     (length word)))))
		      "<wbr>")
		   (url-insert-entities-in-string word)))
   " "))

(defun csid-add-weekday (date)
  (let* ((day (string-to-number (substring date 8)))
	 (time (encode-time 0 0 0
			    day
			    (string-to-number (substring date 5 7))
			    (string-to-number (substring date 0 4)))))
    (concat (format-time-string "%A, %B %e" time)
	    (cond
	     ((<= 10 day 20) "th")
	     ((= (mod day 10) 1) "st")
	     ((= (mod day 10) 2) "nd")
	     ((= (mod day 10) 3) "rd")
	     (t "th")))))

(defun csid-update-html (file &optional inhibit-fetch)
  (csid-read-database)
  (unless inhibit-fetch
    (csid-parse-sources))
  (csid-generate-html file))

(defun csid-write-atom (file)
  (csid-read-database)
  (let ((feed (atom-create "Concerts in Oslo" "http://csid.no/"))
	(database (sort (copy-sequence csid-database)
			(lambda (e1 e2)
			  (string< (nth 1 e1)
				   (nth 1 e2)))))
	(time (current-time)))
    (loop repeat 18
	  for this-date = (format-time-string "%Y-%m-%d" time)
	  for events =
	  (loop for (venue date url name id scan-time) in database
		when (and scan-time
			  (string-match this-date scan-time))
		collect (format "%s: <a href=\"%s\">%s</a> at %s"
				date
				(format "http://csid.no/?goto=%s" id)
				name
				venue))
	  when events
	  do (atom-add-html-entry
	      feed
	      (format "Concerts Registered on %s" this-date)
	      (format "http://csid.no/scan-date=%s" this-date)
	      (mapconcat 'identity events
			 "<br />"))
	  do (setq time (time-subtract time (list 0 (* 25 60 60)))))
    (with-temp-buffer
      (atom-print feed)
      (write-region (point-min) (point-max) file))))

(defun csid-facebook (id)
  (let* ((url-request-method "POST")
	 (boundary (mml-compute-boundary '()))
	 (url-request-data
	  (mm-url-encode-www-form-urlencoded
	   `(("__a" . "1")
	     ("__dyn" . "7xe7oS11Axq2qEcEyEhy8jXWo466E4a68K5U4e1FxZ3ocUO6EKaxe4Ea8uwh8eUny8lwIwhEiw")
	     ("__req" . "1")
	     ("__rev" . "2215361")
	     ("__user" . "0")
	     ("batch_name" . "TimelineEventsCalendarRoute")
	     ("lsd" . "AVqLOmAW")
	     ("method" . "GET")
	     ("queries" . ,(format "{\"q0\":{\"priority\":0,\"q\":\"Query TimelineEventsCalendarRoute{node(%s){id,__typename,@F5}} QueryFragment F0 : Page{id,name,url,is_owned} QueryFragment F1 : Node{id,__typename} QueryFragment F2 : Event{id,name,url} QueryFragment F3 : Event{id,time_range{start},is_event_draft,scheduled_publish_timestamp,start_time_sentence.format(SHORT_TIME_LABEL) as _start_time_sentenceiczKr,suggested_event_context_sentence.show_category(false) as _suggested_event_context_sentence2eg3eg{text},event_place{contextual_name,city{contextual_name,id},__typename,@F0,@F1},event_viewer_capability{is_viewer_admin},is_canceled,@F2} QueryFragment F4 : Page{id,viewer_profile_permissions} QueryFragment F5 : Page{id,name,viewer_profile_permissions,all_owned_events.upcoming(true).allowed_states(CANCELED,DRAFT,SCHEDULED_DRAFT_FOR_PUBLICATION,PUBLISHED).filter_out_canceled_events_if_not_connected(true).first(10) as _all_owned_events2E5FjY{edges{is_hidden_on_profile_calendar,is_added_to_profile_calendar,node{id,time_range{start},@F3},cursor},page_info{has_next_page,has_previous_page}},all_owned_events.past(true).allowed_states(CANCELED,DRAFT,SCHEDULED_DRAFT_FOR_PUBLICATION,PUBLISHED).filter_out_canceled_events_if_not_connected(true).first(10) as _all_owned_events3oqq9Y{edges{is_hidden_on_profile_calendar,is_added_to_profile_calendar,node{id,time_range{start},@F3},cursor},page_info{has_next_page,has_previous_page}},@F4}\",\"query_params\":{}}}"
				   id))
	     ("scheduler" . "phased)")))))
    (with-current-buffer (csid-retrieve-synchronously
			  "https://www.facebook.com/api/graphqlbatch/")
      (goto-char (point-min))
      (when (re-search-forward "^\r?\n" nil t)
	(prog1
	    (ignore-errors (json-read))
	  (kill-buffer (current-buffer)))))))


(provide 'csid)

;;; csid.el ends here
