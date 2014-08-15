;;; text-translator.el --- Text Translator

;; Copyright (C) 2007-2014  HAMANO Kiyoto

;; Author: HAMANO Kiyoto <khiker.mail+elisp@gmail.com>
;;         plus          <MLB33828@nifty.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Translates character strings on Emacs.
;; This package use the text translation service that exists on the internet.

;; Read README.en (English) or README.ja (Japanese).

;;; Code:

(require 'url-queue)

(require 'text-translator-vars)
(require 'text-translator-window)


(defun text-translator (arg &optional last engine-or-func)
  "The function which does text translation.
Use Excite, Google and so translation site.
1. Mark is active
 - Prefix was supplied.
   1. Choose translation site which you use.
   2. Translate by type which you selected.
 - Prefix was not supplied.
   Translate range of region that you selected by
   first element of `text-translator-engine-history'.
   (If `text-translator-engine-history' is nil,
    use `text-translator-default-engine'.)
2. Mark is deactive
 - Prefix was supplied.
   1. Choose translation site which you use.
   2. Translate value which you input from minibuffer by type you selected.
 - Prefix was not supplied.
   Translate value which you input from minibuffer by
   first element of `text-translator-engine-history'.
   (If `text-translator-engine-history' is nil,
    use `text-translator-default-engine'.)"
  (interactive "P")
  (let ((engine (text-translator-check-valid-translation-engine
                 engine-or-func
                 (cond
                  ((and text-translator-all-history
                        (car text-translator-all-history))
                   (nth 0 (caar text-translator-all-history)))
                  (t
                   text-translator-default-engine))))
        str)
    ;; If prefix-arg is non-nil, change translation type.
    (when (or arg last)
      (let ((minibuffer-history
             (let (engines)
               (dolist (i text-translator-all-history)
                 (dolist (j i)
                   (setq engines (cons (nth 0 j) engines))))
               (nreverse engines))))
        (setq engine (completing-read
                      (format "Select translation engine (default %s): "
                              engine)
                      text-translator-site-data-alist nil t nil nil engine))))
    ;; Initialize (init global variable, delete running processess etc...).
    (setq str
          (cond
           (last
            (nth 1 (caar text-translator-all-history)))
           (t
            (text-translator-region-or-read-string
             (format "Enter string translated by %s: " engine))))
          text-translator-all-site-number   1
          text-translator-all-results       nil
          text-translator-processes-alist   nil
          text-translator-all-before-string nil)
    (cond
     ;; The translating engine and traslanting string is same of last
     ;; translation. So, Text-translator display last result.
     ;; Todo: To check whether or not the result string has
     ;; "TRANSLATION: (TIMEOUT|FAILED)".
     ((and (= text-translator-all-site-number
              (length (car text-translator-all-history)))
           (string= str (nth 1 (caar text-translator-all-history)))
           (string= engine (nth 0 (caar text-translator-all-history))))
      (setq text-translator-all-results
            (list (cons engine
                        (nth 2 (assoc engine
                                      (car text-translator-all-history))))))
      (text-translator-display))
     ;; Newly translating
     (t
      (text-translator-client
       (text-translator-check-valid-translation-engine
        (and (not arg)
             (functionp engine-or-func)
             (funcall engine-or-func engine str))
        engine)
       str)))))

(defun text-translator-translate-by-auto-selection (arg)
  "Function that translates by auto selection of translation
engine.  Function that select automatically is value of
`text-translator-auto-selection-func'."
  (interactive "P")
  (text-translator arg nil text-translator-auto-selection-func))

(defun text-translator-translate-by-auto-selection-enja (engine str)
  "Automatic selection function for English to Japanese(or Japanese to
English) translation.
If alphabet ratio is over 40%, select engine which is translating from
English to Japanese.  Otherwise, from Japanese to English."
  (let ((str (or str ""))
        (regex "[^A-Za-z 0-9]+")
        (engine (text-translator-get-engine-type-or-site engine t))
        (site   (text-translator-get-engine-type-or-site engine))
        (percentage 40))
    (cond
     ((member site '("enja" "jaen" ""))
      (cond
       ((> (* (/ (float (length (replace-regexp-in-string regex "" str)))
                 (length str))
              100)
             percentage)
        (format "%s_enja" engine))
       (t
        (format "%s_jaen" engine))))
     (t
      (message (concat "Selected engine is not enja or jaen. "
                       "So use selected engine : %s")
               engine)
      engine))))

(defun text-translator-region-or-read-string (&optional prompt)
  "If mark is active, return the region, otherwise, read string with PROMPT."
  (cond
   (mark-active
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (let ((minibuffer-history
           (mapcar #'(lambda (x)
                       (nth 1 (car x)))
                   text-translator-all-history)))
      (read-string (or prompt "Enter string: "))))))

(defun text-translator-all (arg &optional key str)
  "The function to translate in all of translate sites that matches
the selected type."
  (interactive "P")
  (let ((hash text-translator-sitedata-hash)
        (str (or str
                 (text-translator-region-or-read-string)))
        keys)
    ;; Initalize global variable.
    (setq text-translator-all-results       nil
          text-translator-all-site-number   nil
          text-translator-processes-alist   nil
          text-translator-all-before-string nil)
    ;; confirm and update translation site data.
    (when (or (null hash) arg)
      (setq text-translator-sitedata-hash (text-translator-update-hashtable)
            hash text-translator-sitedata-hash))
    (maphash '(lambda (x y)
                (setq keys (cons x keys)))
             hash)
    (when (setq key (or key
                        (completing-read "Select type: " keys nil t)))
      (let ((sites (gethash key hash)))
        (setq text-translator-all-site-number (length sites))
        (cond
         ;; The translating engine and traslanting string is same of last
         ;; translation. So, Text-translator display last result.
         ;; Todo: To check whether or not the result string has
         ;; "TRANSLATION: (TIMEOUT|FAILED)".
         ((and (= (length (car text-translator-all-history))
                  text-translator-all-site-number)
               (string= (nth 1 (caar text-translator-all-history)) str)
               (member (nth 0 (caar text-translator-all-history)) sites))
          (dolist (i sites)
            (setq text-translator-all-results
                  (cons (cons i
                              (nth 2
                                   (assoc i
                                          (car text-translator-all-history))))
                        text-translator-all-results)))
          (text-translator-display t))
         ;; Newly translating
         (t
          (dolist  (i sites)
            (text-translator-client i str t))))))))

(defun text-translator-all-by-auto-selection (arg)
  "The function to translate in all of translate sites, whose
translation engine is selected automatically.  The selection function
is the value of `text-translator-auto-selection-func'."
  (interactive "P")
  (let ((str (text-translator-region-or-read-string)))
    (text-translator-all
     arg
     (substring (funcall text-translator-auto-selection-func "" str) 1)
     str)))

(defun text-translator-client (engine str &optional all sync)
  "Function that throws out words and phrases that want to translate into
specified site, and receives translation result."
  (let* ((history-delete-duplicates t)
         (truncate-partial-width-windows nil)
         (str    (text-translator-client-preprocess-string engine str))
         (config (assoc engine text-translator-site-data-alist))
         (url    (nth 1 config))
         (method (nth 2 config))
         (coding (nth 4 config))
         (param  (format (nth 3 config)
                         (text-translator-url-encode-string str coding)))
         (text-translator-display-function
          (if sync 'ignore text-translator-display-function)))
    (setq text-translator-all-before-string
          (cons (cons engine str) text-translator-all-before-string)
          text-translator-processes-alist
          (cons (cons engine nil) text-translator-processes-alist)
          text-translator-all-results
          (cons (cons engine nil) text-translator-all-results))
    (text-translator-client-request engine url method param)
    ;; Display only once (Countermesure for text-translator-all).
    (when (and (not sync) (= 1 (length text-translator-processes-alist)))
      (message "Translating..."))
    ;; If `sync' was `t', Synchronize a output.
    (when (and sync (not all))
      (while (not (cdar text-translator-all-results))
        ;; url-queue-retrieve of url-queue.el use run-with-idle-timer.
        ;; In synchronize mode of text-translator, it is not move idle
        ;; state (waits by busy loop using sit-for). So, we call
        ;; url-queue-run-queue directly. url-queue-run-queue is a
        ;; callback function that registered in the run-with-idle-timer.
        (url-queue-run-queue)
        (sit-for 0.01 t))
      (cdar text-translator-all-results))))

(defun text-translator-client-request (engine url method param)
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Content-Type"    . "application/x-www-form-urlencoded")
           ("User-Agent"      . ,text-translator-user-agent)
           ("Accept-Encoding" . ,text-translator-accept-encoding)
           ("Accept-Charset"  . ,text-translator-accept-charset)
           ("Keep-Alive"      . ,text-translator-keep-alive)
           ("Connection"      . ,text-translator-connection)))
        (url-request-data (if (string= method "GET") nil param))
        (url-proxy-services url-proxy-services))
    (when (and text-translator-proxy-server text-translator-proxy-port)
      (setq url-proxy-services
            (cons "http"
                  (format "http://%s:%s/"
                          text-translator-proxy-server
                          text-translator-proxy-port))))
    (when (and text-translator-proxy-user text-translator-proxy-password)
      (push (cons "Proxy-Authorization"
                  (format "Basic %s"
                          (base64-encode-string
                           (concat text-translator-proxy-user ":"
                                   text-translator-proxy-password))))
            url-request-extra-headers))
    (when (string= method "GET")
      (setq url (concat url "?" param)))
    (url-queue-retrieve url #'text-translator-client-callback
                        (list engine)
                        nil t)))

(defun text-translator-client-callback (&optional status engine)
  (let* ((buf    (current-buffer))
         (config (assoc engine text-translator-site-data-alist))
         (all    (> text-translator-all-site-number 1))
         (str    "")
         (err    (plist-get status :error))
         (coding (nth 4 config))
         (parser (nth 5 config)))
    (cond
     ((or (not (null status)) err)
      (cond
       ((eq (nth 1 err) 'url-queue-timeout)
        (setq str "TRANSLATION: TIMEOUT"))
       (t
        (setq str (format "TRANSLATION: FAILED (%s)" (nth 1 err))))))
     (t
      (unwind-protect
          (setq str (text-translator-client-matcher buf parser coding))
        (unless text-translator-debug
          (kill-buffer buf)))
      (when (or (null str) (string= "" str))
        (setq str "TRANSLATION: FAILED"))))
    (when (not (string= "" str))
      (when text-translator-all-results
        (when (assoc engine text-translator-all-results)
          (setcdr (assoc engine text-translator-all-results) str))
        (with-current-buffer (window-buffer)
          (text-translator-display all t))))))

(defun text-translator-client-preprocess-string (engine str)
  ;; For example, if engine is "excite.co.jp_enja",
  ;; this code returns "en".
  (let* ((lang (substring (text-translator-get-engine-type-or-site engine)
                         0 2))
         (rep (cond
               ((not text-translator-do-fill-region)
                text-translator-pre-string-replace-alist)
               ((member lang text-translator-space-division-languages)
                ;; replace "\n" to " ".
                (append '(("\n" . " ") ("\r" . ""))
                        text-translator-pre-string-replace-alist))
               (t
                ;; replace "\n" to "".
                (append '(("\n" . "") ("\r" . ""))
                        text-translator-pre-string-replace-alist)))))
    (text-translator-replace-string str rep)))

(defun text-translator-client-matcher (buf parser coding)
  (with-current-buffer buf
    (when (re-search-forward "^\r?\n" nil t)
      (backward-char 1))
    (let ((str (decode-coding-string (buffer-substring (point) (point-max))
                                     coding)))
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (text-translator-replace-string
         (or (cond
              ((functionp parser)
               (funcall parser))
              ((re-search-forward parser nil t)
               (match-string 1)))
             "")
         text-translator-post-string-replace-alist)))))

(defun text-translator-display (&optional all hist)
  (ding)
  (message "Translating...done")
  (cond
   (all
    (when (not (member 'nil (mapcar 'cdr text-translator-all-results)))
      (when hist
        (text-translator-add-history))
      (cond
       (text-translator-display-function
        (funcall text-translator-display-function))
       (t
        (text-translator-window-display)))))
   (t
    (when hist
      (text-translator-add-history))
    (cond
     (text-translator-display-function
      (funcall text-translator-display-function))
     (t
      (text-translator-window-display))))))

(defun text-translator-add-history ()
  (let ((make-history
         #'(lambda ()
             (mapcar
              #'(lambda (i)
                  (let* ((engine  (car i))
                         (tstring (cdr i))
                         (match   (assoc engine
                                         text-translator-all-before-string)))
                    ;; match is '(engine before-string).
                    (when (and match (cdr match))
                      (list engine (cdr match) tstring))))
              text-translator-all-results))))
    (cond
     (text-translator-all-history
      (add-to-history 'text-translator-all-history (funcall make-history)))
     (t
      (setq text-translator-all-history
            (cons (funcall make-history) text-translator-all-history))))))

(defun text-translator-update-hashtable ()
  (let ((hash (make-hash-table :test 'equal)))
    (mapc #'(lambda (x)
	      (let ((matched (replace-regexp-in-string "\\([^_]*\\)_"
						       ""
						       (car x))))
		(unless (or (string= (car x) matched)
			    (eq ?* (aref matched 0)))
		  (cond
		   ((gethash matched hash)
		    (puthash matched
			     (cons (car x) (gethash matched hash))
			     hash))
		   (t
		    (puthash matched (list (car x)) hash))))))
          text-translator-site-data-alist)
    hash))

(defun text-translator-replace-string (str replace)
  "Function that converts character string specified for argument STR
according to rule REPLACE."
  (with-temp-buffer
    (insert str)
    ;; convert unusable string
    (format-replace-strings replace)
    (buffer-string)))

(defun text-translator-extract-tag-exclusion-string (regex &optional dont-convert-br)
  (when (re-search-forward regex nil t)
    ;; first: convert <br> tag to '\n' (when variable dont-convert-br is nil)
    ;; second: convert any another tags to empty string.
    (let ((matchstr (match-string 1)))
      (setq matchstr
            (text-translator-replace-string
             matchstr
             text-translator-post-string-replace-alist))
      (replace-regexp-in-string
       "<.*?>" "" (if dont-convert-br
                      matchstr
                    (replace-regexp-in-string
                     "<[bB][rR]\\( /\\)?>" "\n" matchstr))))))

(defun text-translator-check-valid-translation-engine (engine valid-engine)
  "Check ENGINE that is registered in `text-translator-site-data-alist'.
Return ENGINE if it is already registered, otherwise return VALID-ENGINE."
  (or (car (member engine (mapcar 'car text-translator-site-data-alist)))
      valid-engine))

(defun text-translator-get-engine-type-or-site (engine &optional get-site)
  "Get a translation engine type or site name.
If optional argument GET-SITE is nil, return a translation engine type.
Otherwise return a translation site name."
  (let ((val (nth (if get-site 0 1) (split-string engine "_"))))
    (if val val "")))

;; by google2.el
(defun text-translator-url-encode-string (str &optional coding)
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((eq ch ?\n)               ; newline
              "%0D%0A")
             ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
              (char-to-string ch))      ; printable
             ((char-equal ch ?\x20)     ; space
              "+")
             (t
              (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                  nil))))

;; initialization function
(defun text-translator-site-data-init ()
  ;; initialize
  (setq text-translator-site-data-alist nil)
  (setq text-translator-site-data-alist
        text-translator-site-data-minimum-alist)
  (dolist (site text-translator-site-data-template-alist)
    (let ((tt-convert-name
           #'(lambda (lang)
               (let ((match-lang (assoc lang (nth 7 site))))
                 (if match-lang
                     (cdr match-lang)
                   lang))))
          (tt-replace-string
           #'(lambda (pstr olang tlang)
               (when olang
                 (setq pstr (replace-regexp-in-string "%o" olang pstr)))
               (when tlang
                 (setq pstr (replace-regexp-in-string "%t" tlang pstr))
                 pstr)))
          engine param config)
      (dolist (i (nth 6 site))
        ;; 0 ... engine
        ;; 1 ... url
        ;; 2 ... method
        ;; 3 ... param
        ;; 4 ... coding
        ;; 5 ... extract function
        (setq engine (concat (nth 0 site)
                             "_"
                             (funcall tt-convert-name (car i))
                             (funcall tt-convert-name (cdr i)))
              param  (funcall tt-replace-string (nth 3 site) (car i) (cdr i))
              config
              (list engine
                    (nth 2 site)
                    (nth 1 site)
                    param
                    (nth 4 site)
                    (nth 5 site)))
        (add-to-list 'text-translator-site-data-alist config)))))
(text-translator-site-data-init)        ; init

(provide 'text-translator)

;;; text-translator.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
