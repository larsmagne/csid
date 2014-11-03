;;; dom.el --- Traverse HTML DOMs
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; dom.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dom.el is distributed in the hope that it will be useful, but
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

(defmacro dom-attr (node attr)
  "Return the attribute ATTR from NODE.
A typical attribute is `:href."
  (let ((n (gensym)))
    `(let ((,n ,node))
       ;; Called on a list of nodes.  Use the first.
       (when (consp (car ,n))
	 (setq ,n (car ,n)))
       (cdr (assq ,attr (cdr ,n))))))

(defun dom-text (node)
  "Return all the text bits in the current node concatenated."
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car node))
    (setq node (car node)))
  (mapconcat
   'identity
   (loop for elem in (cdr node)
	 when (eq (car elem) 'text)
	 collect (cdr elem))
   " "))

(defun dom-texts (node &optional separator)
  "Return all textual data under NODE."
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car node))
    (setq node (car node)))
  (mapconcat
   'identity
   (loop for elem in (cdr node)
	 when (eq (car elem) 'text)
	 collect (cdr elem)
	 when (consp (cdr elem))
	 collect (dom-texts elem separator))
   (or separator " ")))

(defun dom-by-tag (dom name)
  "Return elements in DOM that is of type NAME.
A name is a symbol like `td'."
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car dom))
    (setq dom (car dom)))
  (let ((dom-elements nil))
    (dom-by-tag-1 dom name)
    (nreverse dom-elements)))

(defun dom-by-tag-1 (dom name)
  (when (eq (car dom) name)
    (push dom dom-elements))
  (dolist (entry (cdr dom))
    (when (consp (cdr entry))
      (dom-by-tag-1 entry name))))

(defun dom-by-class (dom match)
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car dom))
    (setq dom (car dom)))
  (dom-elements dom :class match))

(defun dom-by-style (dom match)
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car dom))
    (setq dom (car dom)))
  (dom-elements dom :style match))

(defun dom-by-id (dom match)
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car dom))
    (setq dom (car dom)))
  (dom-elements dom :id match))

(defun dom-elements (dom attribute match)
  "Find elements matching MATCH (a regexp) in ATTRIBUTE.
ATTRIBUTE would typically be `:class', `:id' or the like."
  (let ((dom-elements nil))
    (dom-elements-1 dom attribute match)
    (nreverse dom-elements)))

(defun dom-elements-1 (dom attribute match)
  (dolist (entry (cdr dom))
    (when (and (eq (car entry) attribute)
	       (stringp (cdr entry))
	       (string-match match (cdr entry)))
      (push dom dom-elements))
    (when (consp (cdr entry))
      (dom-elements-1 entry attribute match))))

(defun dom-parent (dom node)
  "Return the parent of NODE in DOM."
  ;; Called on a list of nodes.  Use the first.
  (when (consp (car node))
    (setq node (car node)))
  (if (memq node dom)
      dom
    (let ((result nil))
      (dolist (entry (cdr dom))
	(when (and (not result)
		   (consp (cdr entry)))
	  (setq result (dom-parent entry node))))
      result)))

(provide 'dom)

;;; dom.el ends here
