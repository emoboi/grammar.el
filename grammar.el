;;; -*- coding: utf-8; lexical-binding: t -*-

;;; grammar.el --- Grammar checker for Emacs
;;
;; Copyright (C) 2010 Baoqiu Cui
;;
;; Filename: grammar.el
;; Author: Baoqiu Cui <cbaoqiu AT yahoo DOT com>
;; Maintainer: Baoqiu Cui <cbaoqiu AT yahoo DOT com>
;; Keywords: grammar spell
;; Description: Perform grammar checking in Emacs buffers
;;
;; $Id: grammar.el,v 1.12 2010/06/14 06:00:06 bcui Exp $
;;
;; This file is NOT part of GNU Emacs.
;;
;; grammar.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; A copy of the GNU General Public License can be made from
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Grammar is a minor Emacs mode that performs semi-on-the-fly grammar
;; checking using Link Grammar (http://www.link.cs.cmu.edu/link/).  A
;; separate Link Grammar client, grammar.cc (included in this package),
;; is required in order to make this mode work.
;;
;; To enable Grammar minor mode, type M-x grammar-mode.  This applies
;; only to the current buffer.  Command grammar-mode toggles Grammar
;; mode in the current buffer.
;;

(require 'ispell)
(require 'request) ;; https://github.com/tkf/emacs-request
(require 'json)
(require 'ht) ;;my forked ht
(require 's)


(defvar ginger-end-point
  "http://services.gingersoftware.com/Ginger/correct/json/GingerTheText"  )


(defgroup grammar nil
  "Grammar checking on the fly."
  :tag "Grammar"
  :group 'processes)
  
(defcustom grammar-mode-line-string " Grammar"
  "String displayed on the modeline when grammar is active.
Set this to nil if you don't want a modeline indicator."
  :group 'grammar
  :type '(choice string (const :tag "None" nil)))

(defcustom grammar-program-name "grammar"
  "Program invoked by Grammar mode.
Make sure this program can be located in your environment."
  :type 'string
  :group 'grammar)

(defcustom grammar-delay 3
  "The number of seconds to wait before checking, after a \"delayed\" command."
  :type 'number
  :group 'grammar)

(defface grammar-error-face
  '((((class color)) :underline "darkgreen")
    (t :underline t))
  "Face for highlighting grammar errors."
  :group 'grammar)



(defun s-replace-not-regexp (from to s &optional result)
  (let(
       (n (string-match from s)))
    (if n
	(let ((m (match-end 0)))
	  (s-replace-not-regexp
	   from to (substring s m)
	   (cons to (cons (substring s 0 n) result))
	   ))
      (if result
	  (mapconcat #'identity (reverse (cons s result)) "")
	s))))

(defun s-replace-not-regexp-all-ht! (tbl s) 
  (maphash
   (lambda (key value)  (setq s (s-replace-not-regexp  key value s)))
   tbl)
  s
  )



(defun replace-latex-command-in-string-with-replace-tabel (str tbl)
  (let* 
      ((s (substring str 0))
       (n (string-match  "\\\\\\(cite\\|label\\|ref\\){[^}]*}"  s)))
    (if n
	(let* ((m (match-end 0))
	      (ss-o (substring s n m))
	      (ss-r
	       (ht-gen-str-key tbl ss-o)
	       ))
	  (setf (gethash ss-r tbl) ss-o)
	  (setf (substring s n m) ss-r)
	  (let ((pre-str (substring s 0 m) )
		(result
		  (replace-latex-command-in-string-with-replace-tabel 
		   (substring s m)
		   tbl)))
	    (list 
	     (concat 
	      pre-str
	      (car result))
	     tbl)
	  ))
      (progn
	(list str tbl)	
	))))



(defun replace-latex-command-in-string (str)
  (let ((replace-table (ht-create)))
    (car (replace-latex-command-in-string-with-replace-tabel str replace-table ))))


(defun ginger-region-continuous (conti start0 end0 replace-table0)
  (let ((replaced
	 (replace-latex-command-in-string-with-replace-tabel
	 ;(replace-latex-command-in-string 
	  (buffer-substring-no-properties start0 end0)
	  replace-table0
	  )))


    (lexical-let* 
    ;(let* 
     (;(str str) 
      (cont conti)
      (text (car replaced))(results nil)
      ;;(result-str "")
      (start start0) (end end0) (replace-table (cadr replaced))
      )
    (request
     ginger-end-point
     :params `((lang . "US")
               (clientVersion . "2.0")               
	       (apiKey . "6ae0c3a0-afdc-4532-a810-82ded0054236")
               (text . ,text))
     :parser 'json-read
     :success (
	       function*
	       ;function
               (lambda (&key data &allow-other-keys)
                 (loop with elems = (assoc-default 'LightGingerTheTextResult data)
                       with i = 0
                       for elem across elems
                       for from = (assoc-default 'From elem)
                       for to   = (assoc-default 'To elem)
                       for suggest = (assoc-default
                                      'Text (aref (assoc-default 'Suggestions elem) 0))
                       do
                       (progn
                         (when (< i from)
                           (push (substring text i from) results))
                         (push (propertize suggest
                                           'face 
					   'error
					   ;'grammar-error-face
					   ) results)

                         (setq i (1+ to)))
                       finally
                       (when (< i (length text))
                         (push (substring text i) results)))
		 (let* (
			(result-list (reverse results))
			(fixed-text-with-face (mapconcat 'identity (reverse results) ""))
			(fixed-text (substring-no-properties fixed-text-with-face))
			(candidate-bare-str-list
			 (progn 
			   ;(print 3.2)
			   (list fixed-text text)))
			(replace-alist 
			 (progn 
			   ;(print (list 3.4 candidate-bare-str-list ))
			   (ht-to-alist replace-table)))
			(candidate-list 
			 (progn
			   ;(print (list 3.6 replace-alist))
			   (mapcar
			    (lambda (s) 
			      (if replace-alist 
				  (s-replace-not-regexp-all-ht!
				   replace-table s) s))
			    candidate-bare-str-list )))
			)
		   (when (not (
			       string-equal
			       fixed-text text))
		     (progn
		       (let* (
			      (selected-str
			       (progn 
				  ;(print (list 4.7  candidate-list ))
				  (ido-completing-read 
				   "Select:Ctr-s :" 
				   candidate-list))))

			 (setf (buffer-substring start end) selected-str)
			 )
		       )
		     )
		   (funcall cont end)
		   )))))))

;;  AAA is AAA.

(defun grammar-buffer ()
  "Check grammar buffer"
  (interactive)
  (let ((replace-table (make-hash-table :test #'equal))
	start end)
    ;(save-excursion
      (backward-sentence)
      (setq start (point))
      (forward-sentence)
      (setq end (point))
      ;;(ispell-highlight-spelling-error-overlay start end t)
      ;(grammar-sentence-check-only start end)
      ;(ginger-region-recursive start end replace-table)

      (ginger-region-continuous
       (lambda ( dummy)  
      	 ;(print "in cont")
      	 (goto-char (+ end 4)) 
      	 (grammar-buffer) )
       start end replace-table)

    )
)


(defun grammar-sentence ()
  "Check grammar buffer"
  (interactive)
  (let ((replace-table (make-hash-table :test #'equal))
	start end)
    ;(save-excursion
      (backward-sentence)
      (setq start (point))
      (forward-sentence)
      (setq end (point))
      ;;(ispell-highlight-spelling-error-overlay start end t)
      ;(grammar-sentence-check-only start end)
      ;(ginger-region-recursive start end replace-table)

      (ginger-region-continuous
       (lambda ( dummy)  
      	 ;(print "in cont")
      	 (goto-char (+ end 4)) 
      	 ;(grammar-buffer) 
	 )
       start end replace-table
       )

    )
)



(provide 'grammar)
