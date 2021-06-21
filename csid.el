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
  '(("Revolver" "https://www.facebook.com/revolveroslo/events/?ref=page_internal" facebook (59.917146 10.749779))
    ("Kafé hærverk" "https://www.facebook.com/pg/kafehaerverk/events/?ref=page_internal" facebook (59.919202 10.751920))
    ("Blå" "http://www.blaaoslo.no/" blaa (59.920284 10.752836))
    ("Mir" "http://www.lufthavna.no/" mir (59.921667 10.761053))
    ("Victoria" "http://nasjonaljazzscene.no/arrangement/" victoria (59.914109 10.738198))
    ("Rockefeller" "http://rockefeller.no/index.html" rockefeller :multi (59.916125 10.750050))
    ;;("Mono" "http://www.cafemono.no/program/" mono (59.913942 10.749326))
    ("Parkteateret" "http://parkteatret.no/program/" parkteateret (59.923515 10.758537))
    ("Konsertforeninga" "https://www.facebook.com/Konsertforeninga/events/?ref=page_internal" facebook)
    ;;("Maksitaksi" "https://www.facebook.com/maksitaksii/events?ref=page_internal" facebook (59.918278 10.737577))
    ("Betong" "https://www.facebook.com/betongoslo/events" facebook (59.932264 10.712854))
    ("Bidrobon" "https://nb-no.facebook.com/pg/Drivhuset-musikkverksted-bidrobon-Biermannsg%C3%A5rden-202355836444/events/?ref=page_internal" facebook (59.931460 10.755416))
    ("Cosmopolite" "https://cosmopolite.no/program" cosmopolite (59.936133 10.765991))
    ("Vulkan" "https://vulkanarena.no/" vulkan (59.922435 10.751270))
    ("Jakob" "http://jakob.no/program/" jakob (59.918090 10.754294))
    ("Ultima" "http://ultima.no/program" ultima)
    ("Blitz" "http://www.blitz.no/kalender" blitz (59.918438 10.737446))
    ("Magneten" "http://magnetenpub.blogspot.no//feeds/pages/default?alt=json&v=2&dynamicviews=1"
     magneten :json :date (59.936159 10.765462))
    ("Herr Nilsen" "http://www.herrnilsen.no/program2009.html" nilsen (59.915406 10.740988))
    ("Spektrum" "http://www.oslospektrum.no/" spektrum (59.913001 10.753873))
    ("Ny Musikk" "https://www.facebook.com/nymusikk/events/?ref=page_internal" facebook (59.911173 10.765284))
    ("Konserthuset" "http://www.oslokonserthus.no/program/json/all" konserthuset :json (59.913270 10.729669))
    ("Riksscenen" "http://www.riksscenen.no/program.95415.no.html" riksscenen (59.919877 10.761074))
    ("Olsen" "https://www.facebook.com/olsenbryn/events/?ref=page_internal" facebook (59.907644 10.818268))
    ("Verkstedet" "https://www.facebook.com/verkstedetbar/events/?ref=page_internal" facebook (59.917728 10.754123))
    ("Gamla" "https://www.gamla.no/" gamla (59.913654 10.745297))
    ;;("Sawol" "http://www.sawol.no/category/program/" sawol)
    ;;("Buckleys" "http://www.buckleys.no/konserter.html" buckleys :date)
    ;;("New Orleans" "http://www.neworleansworkshop.com/program" neworleans :date)
    ;;("NB" "http://www.nb.no/Hva-skjer/Arrangementer/Konserter" nasjonalbiblioteket)
    ("Uhørt" "https://www.facebook.com/uhortistroget/events" facebook (59.914105 10.748769))
    ("Kulturhuset" "https://www.facebook.com/kulturhusetioslo/events" facebook (59.914646 10.750909))
    ("Kampen Bistro" "http://www.kampenbistro.no/hvaskjer" kampenbistro (59.913718003724746 10.780875043457623))
    ;;("Kampenjazz" "http://oysteineide.wix.com/kampenjazz#!konserter/cb30" kampenjazz :date)
    ("Cafeteatret" "http://nordicblacktheatre.no/wp-admin/admin-ajax.php?action=wpcal-getevents&end=1444600800&start=1440972000" cafeteatret :json (59.910344 10.767058))
    ("Telenor Arena" "http://telenorarena.no/en/calendar" telenor (59.903079 10.624335))
    ("Postkontoret" "https://www.facebook.com/toyenpostkontor/events?key=events" facebook (59.914083 10.775254))
    ;;("Per på hjørnet" "http://www.pph.oslo.no/" pph :date)
    ("The Villa" "http://www.thevilla.no/program/" villa (59.915832 10.748751))
    ("Dattera" "http://www.dattera.no/nb/pages/6-Kalender" dattera (59.913291 10.760122))
    ("Internasjonalen" "https://www.facebook.com/pg/internasjonalenbar/events/?ref=page_internal" facebook (59.914558 10.749595))
    ("Jæger" "https://www.facebook.com/jaegeroslo/events/?ref=page_internal" facebook (59.913957 10.743499))
    ("Union" "https://www.facebook.com/pg/UnionScene/events/?ref=page_internal" facebook (59.743974 10.192263) :nobound)
    ("Musikkflekken" "https://www.facebook.com/Musikkflekken/events" facebook (59.890424 10.524722) :nobound)
    ("Smelteverket" "https://www.facebook.com/Smelteverket/events" facebook (59.921999 10.752267))
    ;;("Skuret" "https://www.facebook.com/skuret/events" facebook)
    ;;("Pizdets" "https://www.facebook.com/pissjets/events" facebook (59.916321 10.748236))
    ;;("Hvaskjer" "https://www.facebook.com/hvaskjertorshov/events" facebook (59.936192 10.765818))
    ("UiO" "http://www.uio.no/om/aktuelt/arrangementer/konserter/" uio (59.940768 10.723079))
    ;;("Mr Pizza" "http://www.mrpizza.no/" pizza :date)
    ("Sub Scene" "http://www.subscene.no/" subscene (59.912176 10.736554))
    ("Vigeland" "https://www.facebook.com/emanuelvigeland/events" facebook (59.947077 10.692663))
    ("Josefine" "https://www.facebook.com/pg/JosefineVisescene/events/?ref=page_internal" facebook (59.923494 10.727687))
    ;;("Izakaya" "https://www.facebook.com/Izakaya-343430575679537/events?ref=page_internal" facebook (59.918220 10.741845))
    ("Sentralen" "http://www.sentralen.no/arrangementer" sentralen (59.911146 10.740328))
    ("Ingensteds" "https://www.facebook.com/pg/ingenstedsoslo/events/?ref=page_internal" facebook (59.919991 10.752761))
    ("Enga" "https://www.facebook.com/Enga-419256284860268/events?ref=page_internal" facebook (59.907470 10.783760))
    ("Big Dipper" "https://www.facebook.com/Platebutikken-Big-Dipper-428305525645/events?ref=page_internal" facebook (59.913707 10.745384))
    ("Krøsset" "https://www.facebook.com/krxsset/events?ref=page_internal" facebook (59.921141 10.751589))
    ("Cappelens forslag" "https://www.facebook.com/CappelensForslag/events" facebook (59.915077 10.753527))
    ;;("Barrikaden" "http://vestbredden.net/barrikaden/" barrikaden :date)
    ("Henie Onstad" "http://hok.no/kalender" henie-onstad :date (59.888617 10.553501) :nobound)
    ("Khartoum" "https://nb-no.facebook.com/pg/khartoumcontemporary/events/?ref=page_internal" facebook (59.917118 10.750163))
    ("Salt" "https://www.facebook.com/pg/SaltArtMusic/events/?ref=page_internal" facebook (59.907498 10.747032))
    ("Røverstaden" "https://www.facebook.com/pg/roverstaden/events/?ref=page_internal" facebook (59.913562 10.729115))
    ("No 53" "https://www.facebook.com/pg/No-53-139168752824276/events/?ref=page_internal" facebook (59.908699 10.767789))
    ("LilleKampen" "https://www.facebook.com/pg/lillekampen/events/?ref=page_internal" facebook (59.913884 10.781574))
    ("Vaterland" "https://www.facebook.com/pg/vaterlandoslo/events/?ref=page_internal" facebook (59.913885 10.756072))
    ("Rommen scene" "https://www.rommenscene.no/program/" rommen (59.967347 10.914572) :nobound)
    ("Aye Aye Club" "https://www.facebook.com/pg/ayeayeclub/events/?ref=page_internal" facebook (59.913293 10.748973))
    ("Last Train" "https://www.facebook.com/pg/lasttrainoslo/events/?ref=page_internal" facebook (59.914852 10.736654))
    ("Elefant" "https://www.facebook.com/pg/elefantoslo/events/?ref=page_internal" facebook (59.910894 10.737074))
    ;;("Ensjø" "https://www.facebook.com/pg/EnsjoMusikkscene/events/?ref=page_internal" facebook (59.913104 10.788831) :nobound)
    ("Ly" "https://www.facebook.com/pg/LyLokka/events/?ref=page_internal" facebook (59.924335 10.761052))
    ("Grünerløkka brygghus" "https://www.facebook.com/pg/grunerlokkabrygghus/events/?ref=page_internal" facebook (59.925014 10.759393))
    ("Parksalongen" "https://www.facebook.com/pg/ParksalongenBar/events/?ref=page_internal" facebook (59.923052 10.739031))
    ("Brewgata" "https://www.facebook.com/pg/Brewgata-157539921600511/events/?ref=page_internal" facebook (59.914066 10.755094))
    ("Sagene" "https://www.facebook.com/pg/onsdagsjazzen/events/?ref=page_internal" facebook (59.937541 10.756546))
    ("Rock In" "https://www.facebook.com/pg/rockinoslo/events/?ref=page_internal" facebook (59.913002 10.761144))
    ("Brød & Sirkus" "https://www.facebook.com/brodogsirkus/events/" facebook (59.91311858805727 10.736465987756189))
    ("Oslo Jazzfestival" "https://oslojazz.no/program/" oslo-jazzfestival)
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

(defun csid-parse-sources (&optional type debug)
  ;; Hack to work around problem with Facebook gzip chunks.
  (setq url-mime-encoding-string nil)
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
			      (if (and (not debug)
				       (fboundp function))
				  function
				'csid-parse-new)
			      (csid-source-type source)
			      name)
			   (progn
			     (csid-parse-source
			      url
			      (if (fboundp function)
				  function
				'csid-parse-new)
			      (csid-source-type source)
			      name)))))
		   (unless results
		     (message "No results for type %s %s" name url))
		   (loop for result in results
			 unless (memq :multi source)
			 do (push name result)
			 collect (csid-add-id result (memq :date source)))))))
  (format "%s" (with-temp-buffer
		 (insert-file-contents csid-database-file-name)
		 (buffer-string))))

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

(defun csid-parse-source (url function data-type name)
  (with-current-buffer
      (if (eq function 'csid-parse-facebook)
	  (with-current-buffer (generate-new-buffer "face")
	    (let ((file 
		   (format "/tmp/face-%s.html"
			   (cdr (assoc name csid-facebook-files)))))
	      (when (file-exists-p file)
		(insert "\n\n")
		(insert-file-contents file)
		(goto-char (point-min))
		(when (search-forward ">Tidligere arrangementer<" nil t)
		  (delete-region (point) (point-max))))
	      (current-buffer)))
	(csid-retrieve-synchronously url t t))
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
      (when (get-buffer-process (current-buffer))
	(delete-process (get-buffer-process (current-buffer))))
      (kill-buffer (current-buffer)))))

(defun csid-parse-blaa (dom)
  (loop for month in (dom-by-class dom "^month$")
	for month-name = (dom-texts (dom-by-tag month 'h1))
	append (loop for day in (dom-by-class month "^day$")
		     append (loop for elem in (dom-by-tag day 'article)
		     for h1 = (dom-by-tag elem 'h1)
		     when elem
		     collect (list
			      (csid-parse-english-month-date
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

;; "fredag, august 8, 2014"
(defun csid-parse-english-month-date-with-year (string)
  (setq string (downcase string))
  (when (string-match (format "\\(%s\\).*?\\([0-9]+\\).*?\\([0-9]+\\)"
			      (mapconcat 'identity csid-english-months "\\|"))
		      string)
    (format "%04d-%02d-%02d"
	    (string-to-number (match-string 3 string))
	    (1+ (position (match-string 1 string) csid-english-months
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
  (when (and (string-match (format "\\([0-9]+\\).*\\(%s\\).*?\\([0-9]+\\)"
				   (mapconcat
				    (lambda (month)
				      (substring month 0 3))
				    csid-months "\\|"))
			   string)
	     (= (length (match-string 3 string)) 3))
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

;; 23 .  09
(defun csid-parse-sloppy-numeric-date (string)
  (and (string-match "\\([0-9]+\\).*?\\([0-9]+\\)" string)
       (csid-expand-date (string-to-number (match-string 2 string))
			 (string-to-number (match-string 1 string)))))

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
  (cl-loop for event in (dom-by-tag dom 'a)
	   when (equal (dom-attr event 'aria-label) "event photo url")
	   collect (let* ((parent (dom-parent dom (dom-parent dom event)))
			  (time (dom-texts (dom-by-tag parent 'span)))
			  (link (cdr (dom-by-tag parent 'a))))
		     (list (csid-parse-facebook-time time)
			   (replace-regexp-in-string
			    "[?].*" "" (dom-attr link 'href))
			   (dom-texts link)))))
	   
(defun csid-parse-facebook-time (time)
  (or (csid-parse-shortish-month time)
      (csid-parse-short-yearless-month time)
      (and (string-match "I DAG" time)
	   (format-time-string "%F"))
      (and (string-match "I MORGEN" time)
	   (format-time-string "%F" (+ (float-time)
				       (* 60 60 24))))
      (and (string-match "KOMMENDE \\([^ ]+\\)" time)
	   (let ((day-num (seq-position csid-weekdays
					(downcase (match-string 1 time))))
		 (start (float-time)))
	     (while (not (= (1- (string-to-number
				 (format-time-string "%u" start)))
			    day-num))
	       (cl-incf start (* 60 60 24)))
	     (format-time-string "%F" start)))))

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
	for date = (loop for meta in (dom-by-tag elem 'meta)
			 when (equal (dom-attr meta 'itemprop)
				     "startDate")
			 return (dom-attr meta 'content))
	when date
	collect (list (csid-parse-sloppy-iso8601 date)
		      (dom-attr (dom-by-tag elem 'a) 'href)
		      (dom-texts (dom-by-tag elem 'span)))))

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
  (let* ((url-request-method "POST")
	 (boundary (mml-compute-boundary '()))
	 (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	 (url-request-data
	  "view_name=events&view_display_id=page_1&view_args=&view_path=%2Fprogram&view_base_path=program&view_dom_id=4e285d54286047ecb3656969a128c2bdd8b1880b3f455afe4349965e15bf87b8&pager_element=0&page=1&_drupal_ajax=1&ajax_page_state%5Btheme%5D=cosmo&ajax_page_state%5Btheme_token%5D=&ajax_page_state%5Blibraries%5D=better_exposed_filters%2Fauto_submit%2Cbetter_exposed_filters%2Fgeneral%2Cbetter_exposed_filters%2Fselect_all_none%2Ccosmo%2Fglobal%2Cextlink%2Fdrupal.extlink%2Cgoogle_analytics%2Fgoogle_analytics%2Csystem%2Fbase%2Cviews%2Fviews.module%2Cviews_infinite_scroll%2Fviews-infinite-scroll%2Czurb_foundation%2Fglobal"))
    (with-current-buffer (csid-retrieve-synchronously
			  "https://cosmopolite.no/views/ajax?_wrapper_format=drupal_ajax")
      (goto-char (point-min))
      (when (re-search-forward "^\r?\n" nil t)
	(prog1
	    (csid-parse-cosmopolite-1 (ignore-errors (json-read)))
	  (kill-buffer (current-buffer)))))))

(defun csid-parse-cosmopolite-1 (json)
  (cl-loop for elem across json
	   when (equal (cdr (assq 'command elem)) "insert")
	   return (csid-parse-cosmopolite-2
		   (with-temp-buffer
		     (insert (cdr (assq 'data elem)))
		     (libxml-parse-html-region (point-min) (point-max))))))

(defun csid-parse-cosmopolite-2 (dom)
  (cl-loop for elem in (dom-by-tag dom 'li)
	   collect (list (csid-parse-month-date
			  (dom-texts (dom-by-class elem "bottom-row")))
			 (shr-expand-url
			  (dom-attr (dom-by-tag elem 'a) 'href))
			 (dom-texts (dom-by-class elem "views-field-title")))))

(defun csid-parse-vulkan (dom)
  (loop for elem in (dom-by-class dom "event_container")
	collect (list (csid-parse-short-yearless-month
			(dom-texts (dom-by-class elem "^date\\b")))
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
  (cl-loop for elem in (dom-by-class dom "listeblokk")
	   collect (list (csid-parse-full-numeric-date
			  (dom-texts (dom-by-class elem "arrdato")))
			 (shr-expand-url (dom-attr (dom-by-tag elem 'a) 'href))
			 (dom-texts (dom-by-tag elem 'h2)))))

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
  (loop for elem in (dom-by-tag dom 'rs-slide)
	collect (list
		 (csid-parse-month-date
		  (dom-texts (dom-by-class elem "tribe_formatted_event_date")))
		 (shr-expand-url (dom-attr elem 'data-link))
		 (dom-texts (last (dom-by-tag elem 'span))))))

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
  (loop for event in (dom-by-class dom "type-event")
	for link = (dom-by-tag event 'a)
	when (and link
		  (string-match "http" (dom-attr link 'href)))
	collect (list (csid-parse-month-date (dom-texts event))
		      (dom-attr link 'href)
		      (dom-texts (dom-by-tag event 'h1)))))

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
  (loop for event in (dom-by-class dom "event-list-item")
	for date = (csid-parse-sloppy-numeric-date
		    (dom-texts (dom-by-class event "event-date")))
	for url = (dom-attr (dom-by-tag event 'a) 'href)
	when (csid-valid-date-p date)
	collect (list date
		      (shr-expand-url url)
		      (dom-texts (dom-by-class event "event-name")))))

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

(defun csid-generate-html (&optional file summaries)
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
      (if summaries
	  (insert "<title>Concerts in Oslo</title><meta charset='utf-8'><link href='csid.css?ts=%s' rel='stylesheet' type='text/css'><h1>Konserter i Oslo</h1><h2>Concerts in Oslo</h2>")
	(insert
	 (format
	  "<head><title>Concerts in Oslo</title><meta charset='utf-8'><link href='csid.css?ts=%s' rel='stylesheet' type='text/css'><link disabled id='dark-css' href='dark.css' rel='stylesheet' type='text/css'><meta name='viewport' content='width=device-width, initial-scale=1'><link href='pikaday.css' rel='stylesheet' type='text/css'><link rel='icon' href='https://csid.no/favicon.ico'><link href='https://fonts.googleapis.com/css?family=Passion+One' rel='stylesheet'><link href='https://fonts.googleapis.com/css?family=Roboto' rel='stylesheet'><div id='large-heading'><div class='title'>Concerts in Oslo</div><div class='subtitle'>Konserter i Oslo</div></div><div id='body-wrap'><div id='leftmargin'>&nbsp;</div><div id='body-container'><div id='small-heading'><div id='small-menu'><span class='box-shadow-menu'></span></div>Concerts in Oslo</div>"
	  (csid-timestamp)
	  (csid-timestamp))))
      (insert "<table class='events'><colgroup><col class='band'><col class='venue'><col class='button'></colgroup>")
      (setq start (point))
      (loop with prev-date
	    with i = 0
	    for (venue date url name id fetch-date rank) in data
	    when (and (not (string< date now))
		      ;; Only do some hundred lines when there's summaries
		      ;; (to avoid excessive length).
		      (or (not summaries)
			  (< i 200)))
	    do (progn
		 (unless (equal date prev-date)
		   (insert (format "<tr class='%s date'><td colspan=3>%s</tr>"
				   (if (equal prev-date date)
				       "invisible"
				     "visible")
				   (csid-add-weekday date))))
		 (setq prev-date date)
		 (insert (format "<tr name='%s' id='event-%s' data=%s date='%s' time='%s' lat='%s' lng='%s'%s><td><a href='%s'>%s<td>%s</tr>"
				 (replace-regexp-in-string
				  "&" "x" 
				  (replace-regexp-in-string " " "_" venue))
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
				 venue))
		 (when (and summaries
			    url)
		   (incf i)
		   (insert (format "<tr><td colspan=3>%s%s</td></tr>"
				   (let ((img (csid-summary url 'image)))
				     (if (or (not img)
					     (> i 10))
					 ""
				       (format "<img src=%S>" img)))
				   (or (csid-summary url 'summary) "")))
		   (insert "</table><table class='events'><colgroup><col class='band'><col class='venue'><col class='button'></colgroup>")))
	    (setq prev-date date))
      (insert "</table><div id='meta-misc'><div id='selector'></div></div></div><div id='rightmargin'>&nbsp;</div></div>")
      (unless summaries
	(dolist (js '("jquery-3.3.1.min.js"
		      "jquery.cookie.js"
		      "jquery.colorbox-min.js"
		      "FileSaver.min.js"
		      "csid.js"
		      "pikaday.js"
		      "sha1.js"))
	  (insert (format "<script type='text/javascript' src='%s?ts=%s'></script>"
			  js
			  (csid-timestamp))))
	(insert "<script type='text/javascript'>addNavigation();</script>")))))

(defun csid-summary (url type)
  (let ((file (csid-summary-file url)))
    (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((json (json-read)))
	(cdr (assq type json)))))))

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
    (elt (or pos '(59.915440 10.751550)) index)))

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
    (csid-download-facebook-urls)
    (csid-parse-sources))
  (csid-generate-html file))

(defun csid-write-atom (file)
  (csid-read-database)
  (let ((feed (atom-create "Concerts in Oslo" "https://csid.no/"))
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
				(format "https://csid.no/?goto=%s" id)
				name
				venue))
	  when events
	  do (atom-add-html-entry
	      feed
	      (format "Concerts Registered on %s" this-date)
	      (format "https://csid.no/scan-date=%s" this-date)
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
	"<head><title>Crowdsourcing Is Dead</title><meta charset='utf-8'><link href='https://csid.no/csid.css' rel='stylesheet' type='text/css'><meta name='viewport' content='width=device-width, initial-scale=1'><link href='pikaday.css' rel='stylesheet' type='text/css'><link rel='icon' href='https://csid.no/favicon.ico'><body><div id='body-container'><div id='large-heading'><a href=\"https://csid.no/\"><img src='https://csid.no/csid.png' id='logo'></a><p>(Also known as <a href='https://lars.ingebrigtsen.no/2013/09/crowdsourcing-is-dead.html'>'Concerts In Oslo' or 'Konserter i Oslo'</a> on %s.)</p></div><div id='small-heading'><span class='box-shadow-menu' id='small-menu'></span></div>Crowdsourcing Is Dead</div>"
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

(defvar csid-facebook-event-files nil)

(defun csid-write-event-summaries ()
  (csid-read-database)
  ;; Somehow loading certain images makes Facebook return the real
  ;; text page on subsequent accesses.
  ;; (eww "https://www.facebook.com/events/419122858560701/")
  ;; (sit-for 10)
  (setq csid-facebook-event-files nil)
  (let ((data
	 (cl-loop for (nil date url nil event-id) in csid-database
		  when (and url
			    (or (string> date (format-time-string "%F"))
				(equal date (format-time-string "%F")))
			    (not (file-exists-p (csid-summary-file url))))
		  collect (cons event-id url))))
    (with-temp-buffer
      (cl-loop for (id . url) in data
	       for i from 1000
	       when (string-match "facebook.com" url)
	       do
	       (insert (format "%d %s\n" i url))
	       (push (cons url i) csid-facebook-event-files))
      (write-region (point-min) (point-max) "/tmp/faceurls.txt"))
    (call-process "~larsi/src/csid/faceget.py")
    (cl-loop for (event-id . url) in data
	     do (csid-write-event-summary url event-id))))

(defun csid-get-event-summary-loop (dom)
  ;; Facebook will return no text other than the cookie warning,
  ;; sometimes, so check for that and repeat.
  (loop repeat 5
	for summary = (csid-get-event-summary dom)
	unless (string-match "cookies.*Facebook" summary)
	return summary
	finally (return summary)))

(defun csid-find-facebook-image (dom)
  (cl-loop for image in (dom-by-tag dom 'img)
	   when (equal (dom-attr image 'data-imgperflogname)
		       "profileCoverPhoto")
	   return (dom-attr image 'src)))

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
      (let ((image (if (string-match "facebook.com" url)
		       (csid-find-facebook-image dom)
		     (csid-get-event-image dom url)))
	    (summary (csid-get-event-summary-loop dom))
	    (url-request-extra-headers '(("Cookie" . "fr=0iznHLOd07GF3Pj78..BZ8tLB.QG.AAA.0.0.Bano-m.AWVOfML3; sb=6tlhWvzwnenK3Wm6ZmN2WUgS; noscript=1")
					 ("Referer" . "https://www.facebook.com/events/791343834393278/?_fb_noscript=1"))))
	;; Remove Facebook data.
	(setq summary (replace-regexp-in-string
		       "^.*dem til som venner............." "" summary))
	(with-temp-buffer
	  (insert "{")
	  (when image
	    (let ((base64
		   (with-current-buffer
		       (csid-retrieve-synchronously image nil t)
		     (goto-char (point-min))
		     (if (not (re-search-forward "^\r?\n" nil t))
			 nil
		       (delete-region (point-min) (point))
		       (let ((success
			      (call-process-region (point) (point-max)
						   "convert"
						   t t nil
						   "-resize" "300x200>" "-" "jpg:-")))
			 (if (not (zerop success))
			     (progn
			       (kill-buffer (current-buffer))
			       nil)
			   (base64-encode-region (point-min) (point-max))
			   (goto-char (point-min))
			   (while (search-forward "\n" nil t)
			     (replace-match ""))
			   (prog1
			       (buffer-string)
			     (kill-buffer (current-buffer)))))))))
	      (when base64
		;; We managed to download (and convert) the image.
		(insert "\"image\": \"data:image/jpeg;base64,"
			base64
			"\", "))))
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
    (forward-char 3)
    (insert "/")
    (goto-char (point-max))
    (insert "-data.json")
    (goto-char (point-min))
    (insert csid-summary-directory "/")
    (buffer-string)))

(defun csid-get-event-image (dom url)
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
  (if (and (string-match "facebook.com" url)
	   (assoc url csid-facebook-event-files))
      (with-temp-buffer
	(insert-file-contents
	 (format "/tmp/face-%d.html"
		 (cdr (assoc url csid-facebook-event-files))))
	(libxml-parse-html-region (point-min) (point-max)))
    (csid-retrieve-direct-event-dom url)))

(defun csid-retrieve-direct-event-dom (url)
  (with-current-buffer (csid-retrieve-synchronously url)
    (goto-char (point-min))
    (when (re-search-forward "^\r?\n" nil t)
      (prog1
	  (libxml-parse-html-region (point) (point-max))
	(kill-buffer (current-buffer))))))

(defun csid-retrieve-phantom-event-dom (url)
  (let ((html "/tmp/event.html")
	(js "/tmp/dump.js"))
    ;; Ensure that phantomjs doesn't talk to the X server.
    (setenv "QT_QPA_PLATFORM" "offscreen")
    (when (file-exists-p html)
      (delete-file html))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name
	(file-name-nondirectory js)
	(file-name-directory (locate-library "csid.el"))))
      (search-forward "<URL>")
      (replace-match (format "%S" url) t t)
      (write-region (point-min) (point-max) js nil 'silent))
    (call-process "phantomjs" nil nil nil js)
    (prog1
	(when (file-exists-p html)
	  (with-temp-buffer
	    (insert-file-contents html)
	    (libxml-parse-html-region (point-min) (point-max))))
      (when (file-exists-p html)
	(delete-file html))
      (delete-file js))))

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
  (dolist (tag '(style script comment noscript))
    (loop for comment in (dom-by-tag dom tag)
	  do (dom-remove-node dom comment)))
  (eww-score-readability dom)
  (let ((text (dom-texts (csid-highest-readability dom))))
    (setq text (replace-regexp-in-string "[\t\n\r ]+" " " text))
    (if (> (length text) 400)
	(concat (substring text 0 400) "[...]")
      text)))

(defvar csid-facebook-files nil)

(defun csid-download-facebook-urls ()
  (setq csid-facebook-files nil)
  (with-temp-buffer
    (cl-loop for source in csid-sources
	     for i from 1
	     when (eq (nth 2 source) 'facebook)
	     do
	     (insert (format "%d %s\n" i (nth 1 source)))
	     (push (cons (nth 0 source) i) csid-facebook-files))
    (write-region (point-min) (point-max) "/tmp/faceurls.txt"))
  (call-process "~larsi/src/csid/faceget.py"))

(defun csid-parse-kampenbistro (dom)
  (cl-loop for event in (dom-by-class dom "eventlist-event")
	   for link = (dom-by-tag (dom-by-tag event 'h1) 'a)
	   collect (list (csid-parse-english-month-date-with-year
			  (dom-texts (dom-by-class event "^date$")))
			 (dom-attr link 'href)
			 (dom-texts link))))

(defun csid-parse-oslo-jazzfestival (dom)
  (cl-loop for name in (dom-by-tag dom 'h2)
	   for event = (dom-parent dom name)
	   collect (list (csid-parse-numeric-date
			  (dom-texts
			   (caddr (memq name event))))
			 (shr-expand-url (dom-attr (dom-by-tag event 'a) 'href))
			 (dom-texts (dom-by-tag event 'h2)))))

(provide 'csid)

;;; csid.el ends here

