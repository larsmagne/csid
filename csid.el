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
(require 'url-http)

(defvar csid-database-file-name "~/.emacs.d/csid.data")

(defvar csid-summary-directory "~/src/csid/summaries")

(defvar csid-sequence 0)

(defvar csid-sources
  '(("Revolver" "https://revolveroslo.ticketco.no/" ticketco (59.917146 10.749779))
    ("Blå" "http://www.blaaoslo.no/" blaa (59.920284 10.752836))
    ("Mir" "http://www.lufthavna.no/" mir (59.921667 10.761053))
    ("Victoria" "http://nasjonaljazzscene.no/arrangement/" victoria (59.915514 10.737868))
    ("Rockefeller" "http://rockefeller.no/index.html" rockefeller :multi (59.916125 10.750050))
    ("Mono" "http://www.cafemono.no/program/" mono (59.913942 10.749326))
    ("Parkteateret" "http://parkteatret.no/program/" parkteateret (59.923515 10.758537))
    ("Konsertforeninga" "http://www.konsertforeninga.no/event-directory/" konsertforeninga)
    ;;("Maksitaksi" "https://www.facebook.com/maksitaksii/events?ref=page_internal" facebook (59.918278 10.737577))
    ("Betong" "https://www.facebook.com/betongoslo/events" facebook (59.932264 10.712854))
    ("Bidrobon" "https://nb-no.facebook.com/pg/Drivhuset-musikkverksted-bidrobon-Biermannsg%C3%A5rden-202355836444/events/?ref=page_internal" facebook (59.931460 10.755416))
    ("Cosmopolite" "http://cosmopolite.no/program/cosmopolite" cosmopolite (59.936133 10.765991))
    ("Belleville" "http://cosmopolite.no/program/belleville" cosmopolite (59.936133 10.765991))
    ;;("Vulkan" "https://vulkanarena.no/" vulkan (59.922435 10.751270))
    ("Jakob" "http://jakob.no/program/" jakob (59.918090 10.754294))
    ("Ultima" "http://ultima.no/program" ultima)
    ("Blitz" "http://www.blitz.no/kalender" blitz (59.918438 10.737446))
    ("Magneten" "http://magnetenpub.blogspot.no//feeds/pages/default?alt=json&v=2&dynamicviews=1"
     magneten :json :date (59.936159 10.765462))
    ("Herr Nilsen" "http://www.herrnilsen.no/program2009.html" nilsen (59.915406 10.740988))
    ("Spektrum" "http://www.oslospektrum.no/" spektrum (59.913001 10.753873))
    ("Ny Musikk" "http://nymusikk.no/no/hva-skjer/" nymusikk (59.911173 10.765284))
    ("Konserthuset" "http://www.oslokonserthus.no/program/json/all" konserthuset :json (59.913270 10.729669))
    ("Riksscenen" "http://www.riksscenen.no/program.95415.no.html" riksscenen (59.919877 10.761074))
    ("Olsen" "http://shop.olsenbar.no/program/" olsen (59.907644 10.818268))
    ;;("Verkstedet" "http://www.verkstedetbar.no/program/" verkstedet :date)
    ("Gamla" "https://www.gamla.no/" gamla (59.913654 10.745297))
    ;;("Sawol" "http://www.sawol.no/category/program/" sawol)
    ;;("Buckleys" "http://www.buckleys.no/konserter.html" buckleys :date)
    ;;("New Orleans" "http://www.neworleansworkshop.com/program" neworleans :date)
    ;;("NB" "http://www.nb.no/Hva-skjer/Arrangementer/Konserter" nasjonalbiblioteket)
    ("Uhørt" "https://www.facebook.com/uhortistroget/events" facebook (59.914105 10.748769))
    ("Kulturhuset" "https://www.facebook.com/kulturhusetioslo/events" facebook (59.914646 10.750909))
    ;;("Kampenjazz" "http://oysteineide.wix.com/kampenjazz#!konserter/cb30" kampenjazz :date)
    ("Cafeteatret" "http://nordicblacktheatre.no/wp-admin/admin-ajax.php?action=wpcal-getevents&end=1444600800&start=1440972000" cafeteatret :json (59.910344 10.767058))
    ("Telenor Arena" "http://telenorarena.no/en/calendar" telenor (59.903079 10.624335))
    ("Postkontoret" "https://www.facebook.com/toyenpostkontor/events?key=events" facebook (59.914083 10.775254))
    ;;("Per på hjørnet" "http://www.pph.oslo.no/" pph :date)
    ("The Villa" "http://www.thevilla.no/program/" villa (59.915832 10.748751))
    ("Dattera" "http://www.dattera.no/nb/pages/6-Kalender" dattera (59.913291 10.760122))
    ("Internasjonalen" "http://www.internasjonalen.no/program/" internasjonalen (59.914558 10.749595))
    ("Jæger" "http://jaegeroslo.no/program/" jaeger (59.913957 10.743499))
    ("Union" "http://unionscene.no/program/" union (59.743974 10.192263) :nobound)
    ("Musikkflekken" "https://www.facebook.com/Musikkflekken/events" facebook (59.890424 10.524722) :nobound)
    ("Smelteverket" "https://www.facebook.com/Smelteverket/events" facebook (59.921999 10.752267))
    ;;("Skuret" "https://www.facebook.com/skuret/events" facebook)
    ("Pizdets" "https://www.facebook.com/pissjets/events" facebook (59.916321 10.748236))
    ("Hvaskjer" "https://www.facebook.com/hvaskjertorshov/events" facebook (59.936192 10.765818))
    ("UiO" "http://www.uio.no/om/aktuelt/arrangementer/konserter/" uio (59.940768 10.723079))
    ;;("Mr Pizza" "http://www.mrpizza.no/" pizza :date)
    ("Sub Scene" "http://www.subscene.no/" subscene (59.912176 10.736554))
    ("Vigeland" "https://www.facebook.com/emanuelvigeland/events" facebook (59.947077 10.692663))
    ("Josefine" "http://josefinevise.no/" josefine (59.923494 10.727687))
    ("Izakaya" "https://www.facebook.com/Izakaya-343430575679537/events?ref=page_internal" facebook (59.918220 10.741845))
    ("Sentralen" "http://www.sentralen.no/arrangementer" sentralen (59.911146 10.740328))
    ("Ingensteds" "https://www.facebook.com/pg/ingenstedsoslo/events/?ref=page_internal" facebook (59.919991 10.752761))
    ("Enga" "https://www.facebook.com/Enga-419256284860268/events?ref=page_internal" facebook (59.907470 10.783760))
    ("Big Dipper" "https://www.facebook.com/Platebutikken-Big-Dipper-428305525645/events?ref=page_internal" facebook (59.913707 10.745384))
    ("Krøsset" "https://www.facebook.com/krxsset/events?ref=page_internal" facebook (59.921141 10.751589))
    ("Kafé hærverk" "https://www.facebook.com/pg/kafehaerverk/events/?ref=page_internal" facebook (59.919202 10.751920))
    ("Cappelens forslag" "https://www.facebook.com/pg/CappelensForslag/events/?ref=page_internal" facebook (59.915077 10.753527))
    ;;("Barrikaden" "http://vestbredden.net/barrikaden/" barrikaden :date)
    ("Henie Onstad" "http://hok.no/kalender" henie-onstad :date (59.888617 10.553501) :nobound)
    ("Ila fysikalske" "https://nb-no.facebook.com/pg/ilafysikalske/events/" facebook (59.930864 10.753765))
    ("Khartoum" "https://nb-no.facebook.com/pg/khartoumcontemporary/events/?ref=page_internal" facebook (59.917118 10.750163))
    ("Salt" "http://salted.no/events/list/?tribe_event_display=list&ical=1&tribe_display=list" salt :vcalendar (59.907498 10.747032))
    ("Røverstaden" "https://www.facebook.com/pg/roverstaden/events/?ref=page_internal" facebook (59.913562 10.729115))
    ("No 53" "https://www.facebook.com/pg/No-53-139168752824276/events/?ref=page_internal" facebook (59.908699 10.767789))
    ("LilleKampen" "https://www.facebook.com/pg/lillekampen/events/?ref=page_internal" facebook (59.913884 10.781574))
    ("Goon Bar" "https://www.facebook.com/pg/WeAreAllGoons/events/?ref=page_internal" facebook (59.917146 10.749779))
    ("Vaterland" "https://www.facebook.com/pg/vaterlandoslo/events/?ref=page_internal" facebook (59.913885 10.756072))
    ("Rommen scene" "https://www.rommenscene.no/program/" rommen (59.967347 10.914572))
    ("Skippergata" "https://www.facebook.com/pg/Skippergata.oslo/events/?ref=page_internal" facebook (59.910627 10.747597))
    ))

(defvar csid-database nil)

(defun csid-write-database (data)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file csid-database-file-name
      (pp data (current-buffer))))
  nil)

(defun csid-update-database (data)
  (dolist (elem data)
    (if (or (not (and (nth 1 elem)
		      (csid-valid-date-p (nth 1 elem))))
	    (null (nth 3 elem)))
	(message "Invalid data for %S" elem)
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

(defun csid-source-type (source)
  (cond
   ((memq :json source)
    :json)
   ((memq :vcalendar source)
    :vcalendar)
   (t
    :html)))

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
			      (csid-source-type source))
			   (progn
			     (csid-parse-source
			      url
			      (if (fboundp function)
				  function
				'csid-parse-new)
			      (csid-source-type source))))))
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

(defun csid-close ()
  (maphash
   (lambda (key val)
     (dolist (proc val)
       (delete-process proc)))
   url-http-open-connections)
  (setq url-http-open-connections (make-hash-table :test 'equal
						   :size 17)))

(defun csid-retrieve-synchronously (url &optional silent inhibit-cookies)
  (condition-case var
      (csid-retrieve-synchronously-1 url silent inhibit-cookies)
    (error
     (generate-new-buffer "*csid error*"))))

(defun csid-retrieve-synchronously-1 (url &optional silent inhibit-cookies)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL."
  ;; Never reuse anything, because perhaps that creates problems?
  (csid-close)
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
	(set-process-query-on-exit-flag proc nil)
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
		 (shr-base (shr-parse-base url))
		 (start (point))
		 (end (point-max))
		 (buf (current-buffer)))
	    (with-temp-buffer
	      (insert-buffer-substring buf start end)
	      (goto-char (point-min))
	      (decode-coding-region (point) (point-max) charset)
	      (funcall function
		       (cond
			((eq data-type :json)
			 (ignore-errors (json-read)))
			((eq data-type :vcalendar)
			 (ignore-errors
			   (vcalendar-parse-region (point) (point-max))))
			(t
			 (libxml-parse-html-region (point) (point-max))))))))
      (kill-buffer (current-buffer)))))

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

(defun csid-parse-sloppy-iso8601 (string)
  (and (string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" string)
       (format "%04d-%02d-%02d"
	       (string-to-number (match-string 1 string))
	       (string-to-number (match-string 2 string))
	       (string-to-number (match-string 3 string)))))

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
		      (dom-texts link))))

(defun csid-parse-facebook (dom)
  (let ((id (loop for meta in (dom-by-tag dom 'meta)
		  for content = (dom-attr meta 'content)
		  when (and content
			    (string-match "fb://page/\\([0-9]+\\)" content))
		  return (match-string 1 content)))
	(cursor nil))
    (when id
      (csid-parse-facebook-public-1
       (csid-get-facebook-events-public id cursor)))))

(defun csid-parse-facebook-1 (json)
  (loop for event across (cdr (assq 'data json))
	collect (list (csid-parse-iso8601 (cdr (assq 'start_time event)))
		      (format "https://www.facebook.com/events/%s/"
			      (cdr (assq 'id event)))
		      (cdr (assq 'name event)))))

(defun csid-parse-facebook-public-1 (json)
  (loop for event across (cdr (assq 'edges (cdr (assq 'upcoming_events (cdr (assq 'page (cdr (assq 'data json))))))))
	for elem = (cdr (assq 'node event))
	collect (list (csid-parse-iso8601 (cdar (cdr (assq 'time_range elem))))
		      (format "https://www.facebook.com/events/%s/"
			      (cdr (assq 'id elem)))
		      (cdr (assq 'name elem)))))

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
  (loop for elem in (dom-by-class dom "eventon_list_event")
	for time = (loop for time in (dom-by-tag elem 'time)
			 when (equal (dom-attr time 'itemprop) "startDate")
			 return (dom-attr time 'datetime))
	for link = (dom-by-tag elem 'a)
	for title = (dom-texts (dom-by-class elem "evcal_event_title"))
	when time
	collect (list (csid-parse-sloppy-iso8601 time)
		      (dom-attr link 'href)
		      title)))

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
  (loop for event in (dom-by-class dom "^wsite-multicol$")
	for link = (shr-expand-url
		    (dom-attr
		     (dom-by-tag (dom-by-class event "paragraph") 'a)
		     'href))
	for image = (shr-expand-url (dom-attr (dom-by-tag event 'img) 'src))
	for date = (csid-parse-slashed-date
		    (dom-texts (dom-by-class event "paragraph")))
	when (and date
		  (csid-date-likely-p date))
	collect (list date
		      (or link image)
		      (dom-texts (dom-by-class event "paragraph")))))

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
  (loop for elem in (dom-by-class dom "event_container")
	collect (list (csid-parse-short-yearless-month
			(dom-texts (dom-by-class elem "^date$")))
		      (dom-attr (dom-by-tag elem 'a) 'href)
		      (dom-texts (dom-by-class elem "event_title")))))

(defun csid-parse-jakob (dom)
  (loop for event in (dom-by-tag dom 'article)
	collect (list (csid-parse-norwegian-month-date-with-year
		       (dom-texts (dom-by-class event "^tease-meta$")))
		      (dom-attr (dom-by-tag event 'a) 'href)
		      (dom-texts (dom-by-tag event 'h2)))))
  
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
   
(defun csid-parse-salt (dom)
  (loop with regexp = "concert\\|konsert"
	for elem in (dom-by-tag dom 'vevent)
	for subject = (dom-text (dom-by-tag elem 'summary))
	when (or (string-match regexp subject)
		 (string-match
		  regexp (dom-text (dom-by-tag elem 'description))))
	collect (list (csid-parse-compact-iso8601
		       (dom-text (dom-by-tag elem 'dtstart)))
		      (dom-text (dom-by-tag elem 'url))
		      subject)))

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
  (loop for elem in (dom-by-tag (dom-by-class dom "av-upcoming-events") 'a)
	collect (list
		 (csid-parse-month-date
		  (dom-texts (dom-by-class elem "tribe-event-date-start")))
		 (dom-attr elem 'href)
		 (dom-texts (dom-by-tag elem 'h4)))))

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
	for name = (or
		    (loop for string in (cdr (dom-strings event))
			  when (and (not (member string csid-months))
				    (not (string-match "[0-9][0-9]:[0-9][0-9]"
						       string))
				    (not (string-match "presenterer" string)))
			  return string)
		    (cadr (dom-strings event)))
	for date = (csid-parse-short-yearless-month (dom-texts elem))
	when (and name
		  (csid-valid-date-p date)
		  (csid-date-likely-p date))
	collect (list date
		      "http://www.buckleys.no/konserter.html"
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
  (loop for event in (dom-by-class dom "^item event$")
	for link = (dom-attr (dom-by-tag event 'a) 'href)
	when link
	collect (list (csid-parse-short-month
		       (dom-texts (dom-by-class event "date"))
		       t)
		      (shr-expand-url link)
		      (csid-clean-string
		       (dom-texts (dom-by-tag event 'h3))))))

(defun csid-parse-pph (dom)
  (let* ((box (dom-by-class dom "programbg"))
	 (texts (delete "" (split-string (dom-texts box "|") "|"))))
    (loop while texts
	  for date = (csid-parse-numeric-date (car texts))
	  if (and date
		  (csid-valid-date-p date)
		  (csid-date-likely-p date))
	  collect (prog1
		      (list date
			    "http://www.pph.oslo.no/"
			    (cadr texts))
		    (setq texts (cddr texts)))
	  else
	  do (setq texts (cdr texts)))))

(defun csid-parse-villa (dom)
  (loop for event in (dom-by-class dom "edgtf-event-content")
	for link = (dom-by-tag event 'a)
	collect (list (csid-parse-short-yearless-month
		       (dom-texts
			(dom-by-class event "edgtf-event-date-holder")))
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
	while (not (string< date (format-time-string "%Y-%m-%d")))
	when (csid-date-likely-p date)
	collect (list date
		      (dom-attr link 'href)
		      (dom-text link))))

(defun csid-parse-jaeger (dom)
  (loop for event in (dom-by-class dom "program_right")
	for link = (dom-by-tag (dom-by-tag event 'h1) 'a)
	for date = (csid-parse-english-month-date
		    (dom-texts (dom-by-tag event 'h7)))
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
  (loop for event in (dom-by-class dom "entry-content")
	for link = (dom-by-tag event 'a)
	when (and link
		  (string-match "http" (dom-attr link 'href)))
	collect (list (csid-parse-month-date (dom-texts (dom-by-tag event 'p)))
		      (dom-attr link 'href)
		      (dom-texts (dom-by-tag event 'h1)))))

(defun csid-parse-josefine (dom)
  (loop for event in (dom-by-tag (dom-by-id dom "^northsidebar$") 'a)
	for text = (dom-texts event)
	for date = (csid-parse-month-date text)
	when (csid-valid-date-p date)
	collect (list date
		      (dom-attr event 'href)
		      (replace-regexp-in-string "^[^:]+: +" "" text))))

(defun csid-parse-ticketco (dom)
  (loop for event in (dom-by-class dom "tc-events-list--details")
	for a = (dom-by-tag event 'a)
	for date = (csid-parse-full-numeric-date
		    (dom-texts (dom-by-class
				event "^tc-events-list--place-time$")))
	when (csid-valid-date-p date)
	collect (list date
		      (dom-attr a 'href)
		      (dom-texts a))))

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

(defun csid-parse-barrikaden (dom)
  (loop for section in (dom-by-tag (dom-by-class dom "entry-content") 'p)
	for elem = (dom-texts section)
	for date = (csid-parse-full-numeric-date elem)
	for text = (replace-regexp-in-string ".*[0-9]+.[0-9]+.[0-9]+.[: ]*" "" elem)
	when (and date
		  (plusp (length text)))
	collect (list date
		      "http://vestbredden.net/barrikaden/"
		      text)))

(defun csid-parse-henie-onstad (dom)
  (loop for event in (dom-by-class
		      (loop for elem in (dom-by-class dom "calendar")
			    when (eq (dom-tag elem) 'ul)
			    return elem)
		      "^row$")
	for type = (dom-text (dom-by-class event "type"))
	when (member type '("Konsert" "Performance"))
	collect (list (csid-parse-numeric-date
		       (dom-text (dom-by-class event "date-start")))
		      "http://hok.no/kalender"
		      (dom-text (dom-by-tag event 'h3)))))

(defun csid-parse-rommen (dom)
  (loop for event in (dom-by-tag (dom-by-class dom "eventlist--upcoming")
				 'article)
	collect (list (dom-attr (dom-by-tag event 'time) 'datetime)
		      (shr-expand-url (dom-attr (dom-by-tag event 'a) 'href))
		      (dom-texts (dom-by-class event "eventlist-title")))))

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
      (insert
       (format
	"<head><title>Crowdsourcing Is Dead</title><meta charset='utf-8'><link href='csid.css?ts=%s' rel='stylesheet' type='text/css'><meta name='viewport' content='width=device-width, initial-scale=1'><link href='pikaday.css' rel='stylesheet' type='text/css'><link rel='icon' href='http://csid.no/favicon.ico'><body><div id='body-container'><div id='large-heading'><img src='csid.png?ts=%s' id='logo'><p>(Also known as <a href='http://lars.ingebrigtsen.no/2013/09/crowdsourcing-is-dead.html'>'Konserter i Oslo'</a>.)</p></div></div><div id='small-heading'><div id='small-menu'><span class='box-shadow-menu'></span></div>Concerts in Oslo</div>"
	(csid-timestamp)
	(csid-timestamp)))
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
		 (insert (format "<tr name='%s' id='event-%s' data=%s date='%s' time='%s' lat='%s' lng='%s'%s><td><a href='%s'>%s<td>%s</tr>"
				 (replace-regexp-in-string " " "_" venue)
				 id
				 rank date fetch-date
				 (csid-latlng venue 0)
				 (csid-latlng venue 1)
				 (if (memq :nobound (assoc venue csid-sources))
				     " nobound='true'"
				   "")
				 url
				 (csid-make-text-breakable
				  (if (> (length name) 1000)
				      (substring name 0 1000)
				    name))
				 venue)))
	    (setq prev-date date))
      (insert "</table><div id='selector'></div>")
      (dolist (js '("jquery-1.10.2.min.js"
		    "jquery.cookie.js"
		    "jquery.colorbox-min.js"
		    "FileSaver.min.js"
		    "csid.js"
		    "pikaday.js"
		    "sha1.js"))
	(insert (format "<script type='text/javascript' src='%s?ts=%s'></script>"
			js
			(csid-timestamp))))
      (insert "<script type='text/javascript'>addNavigation();</script>"))))

(defun csid-timestamp ()
  (float-time))

(defun csid-latlng (venue index)
  ;; Special-case the only venue that has several sub-venues.
  (when (member venue '("Bushwick" "Leiligheten" "John Dee"))
    (setq venue "Rockefeller"))
  (let ((pos (loop for elem in (assoc venue csid-sources)
		   when (and (listp elem)
			     (= (length elem) 2)
			     (numberp (car elem))
			     (numberp (cadr elem)))
		   return elem)))
    ;; Default to the middle of the world, which is Sentrum Scene.
    (elt (or pos '(59.913074 10.751834)) index)))

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

(defun csid-add-weekday (date &optional year)
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
	     (t "th"))
	    (if year
		(format-time-string " %Y" time)
	      ""))))

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

(defun csid-write-date (dir &optional date)
  (csid-read-database)
  (let ((database (sort (copy-sequence csid-database)
			(lambda (e1 e2)
			  (string< (nth 1 e1)
				   (nth 1 e2)))))
	(this-date (or date (format-time-string "%Y-%m-%d")))
	(coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert
       (format
	"<head><title>Crowdsourcing Is Dead</title><meta charset='utf-8'><link href='http://csid.no/csid.css' rel='stylesheet' type='text/css'><meta name='viewport' content='width=device-width, initial-scale=1'><link href='http://csid.no/pikaday.css' rel='stylesheet' type='text/css'><link rel='icon' href='http://csid.no/favicon.ico'><body><div id='body-container'><div id='large-heading'><a href=\"http://csid.no/\"><img src='http://csid.no/csid.png' id='logo'></a><p>(Also known as <a href='http://lars.ingebrigtsen.no/2013/09/crowdsourcing-is-dead.html'>'Concerts In Oslo' or 'Konserter i Oslo'</a> on %s.)</p></div><div id='small-heading'><span class='box-shadow-menu' id='small-menu'></span></div>Crowdsourcing Is Dead</div>"
	(csid-add-weekday this-date t)))
      (insert "<table class='events'>")
      (loop for (venue date url name id scan-time) in
	    (sort database (lambda (e1 e2) (string< (car e1) (car e2))))
	    when (equal this-date date)
	    do (insert (format "<tr><td><a href=\"%s\">%s</a><td>%s"
			       url name venue)))
      (insert "</table>")
      (write-region (point-min) (point-max)
		    (expand-file-name (format "%s.html" this-date) dir)))))

(defun csid-get-facebook-events (id)
  (with-current-buffer
      (csid-retrieve-synchronously
       (format
	"https://graph.facebook.com/v3.0/%s/events?access_token=%s"
	id csid-facebook-access-token))
    (goto-char (point-min))
    (prog1
	(when (re-search-forward "^$" nil t)
	  (ignore-errors (json-read)))
      (kill-buffer (current-buffer)))))

(defvar csid-app-id nil)
(defvar csid-app-secret nil)

(defun csid-get-long-token ()
  (with-current-buffer
      (csid-retrieve-synchronously
       (format
	"https://graph.facebook.com/v2.9/oauth/access_token?grant_type=fb_exchange_token&client_id=%s&client_secret=%s&fb_exchange_token=%s"
	csid-app-id
	csid-app-secret
	csid-facebook-access-token))
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (ignore-errors (json-read)))))

(defun csid-update-long-token ()
  (let* ((json (csid-get-long-token))
	 (token (cdr (assq 'access_token json))))
    (when token
      (setq csid-facebook-access-token token))
    json))

(defun csid-get-access-token (code)
  (with-current-buffer
      (csid-retrieve-synchronously
       (format
	"https://graph.facebook.com/v2.8/oauth/access_token?client_id=%s&redirect_uri=%s&client_secret=%s&code=%s"
	csid-app-id
	"http://quimby.gnus.org/cicrus/token.php"
	csid-app-secret
	code))
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t)
      (ignore-errors (json-read)))))

(defun csid-make-oauth-url ()
  (with-temp-buffer
    (insert (format "https://www.facebook.com/v2.9/dialog/oauth?client_id=%s&redirect_uri=http://quimby.gnus.org/cicrus/token.php"
		    csid-app-id))
    (copy-region-as-kill (point-min) (point-max))))

;; To get a token, run (csid-make-oauth-url) and then open the URL in
;; Firefox.  Then log in and copy the CODE part of the 404 URL from
;; quimby. Then run
;; (csid-get-access-token code), and then set csid-facebook-access-token to thoe access_token
;; (csid-get-long-token) and set the token
;; again.
;; This should be streamlined.

(defun csid-get-facebook-events-public (id &optional cursor)
  (let* ((url-request-method "POST")
	 (boundary (mml-compute-boundary '()))
	 (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data
	  (concat
	  "av=0&__user=0&__a=1&__dyn=5V8WXBzamaUmgDBzFHpUR1ycCzScybGiWF3ozGFuS-CGgjK2a5RzoaqhEpyAubGqKi5azppEG5VGwwyKbG4V9B88x2axuF98SmjBXDmEgF3ebBz998iGtxifGcze8AzoSbBWAhfypfh6bx25UCiajz8gzAcy4mEepoG9Km4VVpV8KmuidwNxzx-q9CJ4gqz8ixbAJkUGrxjDUG6aJUhxR5zopAgSUCdyFE-5oV6x6WLGFEHAxpu9iFkF7GiumqyaA8DDio8lfy89V8KUK66bBOaqdyU4e4eby9p8HCzmXXghx69jGeyV8V7iQmuaVeaDCyoll4ykmmiQ4UK4ESmiaVp4ehbx6uegryEy4p9VEycGdxOeGFUO8x6V9azeUnKQUSq&__req=5&__be=-1&__pc=PHASED%3ADEFAULT&__rev=3926293&lsd=AVrTTEzk&fb_api_caller_class=RelayModern&variables=%7B%22pageID%22%3A%22"
	  id
	  (if cursor (url-hexify-string
		      (format "\",\"count\":9,\"cursor\":\"%s" cursor)))
	  "%22%7D&doc_id=1385041971602528")))
    (with-current-buffer (csid-retrieve-synchronously
			  "https://www.facebook.com/api/graphql")
      (goto-char (point-min))
      (when (re-search-forward "^\r?\n" nil t)
	(prog1
	    (ignore-errors (json-read))
	  (kill-buffer (current-buffer)))))))

(defun csid-write-event-summaries ()
  (csid-read-database)
  ;; Somehow loading certain images makes Facebook return the real
  ;; text page on subsequent accesses.
  (eww "https://www.facebook.com/events/419122858560701/")
  (sit-for 10)
  (loop for (nil date url nil event-id) in csid-database
	when (and url
		  (or (string> date (format-time-string "%F"))
		      (equal date (format-time-string "%F")))
		  (not (file-exists-p (csid-summary-file url))))
	do (csid-write-event-summary url event-id)))    

(defun csid-write-event-summary (url &optional event-id)
  (let ((file (csid-summary-file url))
	(dom (csid-retrieve-event-dom url)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (if (not dom)
	;; If we can't find anything, then write an empty JSON file.
	(with-temp-buffer
	  (insert "{}")
	  (write-region (point-min) (point-max) file))
      (let ((image (csid-get-event-image dom))
	    (summary (csid-get-event-summary dom))
	    (url-request-extra-headers '(("Cookie" . "fr=0iznHLOd07GF3Pj78..BZ8tLB.QG.AAA.0.0.Bano-m.AWVOfML3; sb=6tlhWvzwnenK3Wm6ZmN2WUgS; noscript=1")
					 ("Referer" . "https://www.facebook.com/events/791343834393278/?_fb_noscript=1"))))
	(with-temp-buffer
	  (insert "{")
	  (when image
	    (insert "\"image\": \"data:image/jpeg;base64,")
	    (insert
	     (with-current-buffer (csid-retrieve-synchronously image nil t)
	       (goto-char (point-min))
	       (if (not (re-search-forward "^\r?\n" nil t))
		   ""
		 (delete-region (point-min) (point))
		 (call-process-region (point) (point-max)
				      "convert"
				      t t nil
				      "-resize" "300x200>" "-" "jpg:-")
		 (base64-encode-region (point-min) (point-max))
		 (goto-char (point-min))
		 (while (search-forward "\n" nil t)
		   (replace-match ""))
		 (prog1
		     (buffer-string)
		   (kill-buffer (current-buffer))))))
	    (insert "\", "))
	  (insert "\"summary\": \""
		  (replace-regexp-in-string "\"" "" summary)
		  "\", \"id\": \""
		  (if event-id
		      (format "%s" event-id)
		    "")
		  "\"}\n")
	  (let ((coding-system-for-write 'binary))
	    (write-region (point-min) (point-max) file))
	  (message "%s" summary))))))

(defun csid-summary-file (url)
  (with-temp-buffer
    (insert (sha1 url))
    (goto-char (point-min))
    (forward-char 10)
    (while (not (eobp))
      (insert "/")
      (forward-char 10))
    (insert "-data.json")
    (goto-char (point-min))
    (insert csid-summary-directory "/")
    (buffer-string)))

(defun csid-get-event-image (dom)
  (let ((images (csid-rank-images dom url)))
    (when images
      (if (> (caar images) (* 200 200))
	  ;; We found a big image, so just use that.
	  (cadr (car images))
	(setq images
	      (sort
	       (csid-get-image-sizes images)
	       (lambda (i1 i2)
		 (> (car i1) (car i2)))))
	(cadr (car images))))))

(defun csid-retrieve-event-dom (url)
  (message "%s" url)
  (with-current-buffer (csid-retrieve-synchronously url)
    (goto-char (point-min))
    (when (re-search-forward "^\r?\n" nil t)
      (prog1
	  (libxml-parse-html-region (point) (point-max))
	(kill-buffer (current-buffer))))))

(defun csid-preferred-image (dom)
  (let ((srcset (or (dom-attr dom 'srcset)
		    (dom-attr dom 'data-srcset)))
        (width (string-to-number (or (dom-attr dom 'width) "100")))
        candidate)
    (when (> (length srcset) 0)
      ;; srcset consist of a series of URL/size specifications
      ;; separated by the ", " string.
      (setq srcset
            (sort (mapcar
                   (lambda (elem)
                     (let ((spec (split-string elem "[\t\n\r ]+")))
                       (cond
                        ((= (length spec) 1)
                         ;; Make sure it's well formed.
                         (list (car spec) 0))
                        ((string-match "\\([0-9]+\\)x\\'" (cadr spec))
                         ;; If we have an "x" form, then use the width
                         ;; spec to compute the real width.
                         (list (car spec)
                               (* width (string-to-number
                                         (match-string 1 (cadr spec))))))
                        (t
                         (list (car spec)
                               (string-to-number (cadr spec)))))))
                   (split-string (replace-regexp-in-string
				  "\\`[\t\n\r ]+\\|[\t\n\r ]+\\'" "" srcset)
				 "[\t\n\r ]*,[\t\n\r ]*"))
                  (lambda (e1 e2)
                    (> (cadr e1) (cadr e2)))))
      ;; Choose the smallest picture that's bigger than the current
      ;; frame.
      (setq candidate (caar (last srcset))))
    (or candidate (dom-attr dom 'src))))

(defun csid-get-imgs (dom url)
  (loop for image in (dom-by-tag dom 'img)
	for width = (dom-attr image 'width)
	for height = (dom-attr image 'height)
	for src = (csid-preferred-image image)
	when (and src
		  (not (string-match
			"banner\\|progapr\\|for-print\\|svg$" src)))
	collect (list (if (and width height)
			  (* (string-to-number width)
			     (string-to-number height))
			-1)
		      (shr-expand-url src url))))

(defun csid-get-backgrounds (dom url)
  (loop for style in (dom-by-tag dom 'style)
	for style-text = (dom-text style)
	for image = (and style-text
			 (string-match "background-image.*url.*\\(http[^)]+\\)"
				       style-text)
			 (match-string 1 style-text))
	when image
	collect (list -1
		      (shr-expand-url image url))))

(defun csid-rank-images (dom url)
  (sort
   (append (csid-get-imgs dom url)
	   (csid-get-backgrounds dom url))
   (lambda (i1 i2)
     (> (car i1) (car i2)))))

(defun csid-get-image-sizes (images)
  (loop for (size url) in images
	when (= size -1)
	do (with-current-buffer (csid-retrieve-synchronously url)
	     (goto-char (point-min))
	     (when (re-search-forward "^\r?\n" nil t)
	       (let ((dimensions
		      (ignore-errors
			(image-size (create-image
				     (buffer-substring (point)
						       (point-max))
				     nil t)
				    t))))
		 (when dimensions
		   (setq size (* (car dimensions) (cdr dimensions))))
		 (kill-buffer (current-buffer)))))
	collect (list size url)))

(defun csid-highest-readability (node)
  (let ((result node)
	highest)
    (dolist (elem (dom-non-text-children node))
      (when (> (or (dom-attr
		    (setq highest (eww-highest-readability elem))
		    :eww-readability-score)
		   most-negative-fixnum)
	       (or (dom-attr result :eww-readability-score)
		   most-negative-fixnum))
        ;; We set a lower bound to how long we accept that the
        ;; readable portion of the page is going to be.
        (when (and (> (length (split-string (dom-texts highest))) 40)
		   (not (string-match "kapsler\\|cookies"
				      (dom-texts highest))))
	  (setq result highest))))
    result))

(defun csid-get-event-summary (dom)
  (dolist (tag '(style script comment))
    (loop for comment in (dom-by-tag dom tag)
	  do (dom-remove-node dom comment)))
  (eww-score-readability dom)
  (let ((text (dom-texts (csid-highest-readability dom))))
    (setq text (replace-regexp-in-string "[\t\n\r ]+" " " text))
    (if (> (length text) 400)
	(concat (substring text 0 400) "[...]")
      text)))

(provide 'csid)

;;; csid.el ends here
