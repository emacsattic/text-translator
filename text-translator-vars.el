;;; text-translator-vars.el --- Text Translator

;; Copyright (C) 2007-2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Variables for text-translator

;;; Code:

(require 'text-translator-sites)


;; Variables:

(defconst text-translator-version "1.0.50"
  "version numbers of this version of text-translator")

(defconst text-translator-buffer "*translated*"
  "Buffer name that displays translation result.")

(defconst text-translator-window-mode-name "Translator"
  "Major mode name for displaying to mode line.")

(defconst text-translator-work-buffer (concat " " text-translator-buffer)
  "Output Buffer name from translation site.")

(defgroup text-translator nil
  "Text Translator"
  :tag "Text Translator"
  :group 'text-translator)

(defcustom text-translator-window-prefix-key "\C-c"
  "*Prefix key for text-translator commands."
  :tag "Prefix Key of text-translator"
  :type '(string :size 10)
  :group 'text-translator)

(defcustom text-translator-auto-window-adjust t
  "*Whether or not you adjust height of window displayed by dividing."
  :type  'boolean
  :group 'text-translator)

(defcustom text-translator-window-min-height 4
  "*Specify minimum height of the translation result display buffer."
  :type  'integer
  :group 'text-translator)

(defcustom text-translator-leave-string nil
  "*Whether or not you leave the character string before the translating."
  :type  'boolean
  :group 'text-translator)

(defcustom text-translator-pre-string-replace-alist
  '(("+" . "＋") ("&#8211;" . "-")  ("&#8226;" . "・"))
  "*Rule that converts character string that wants to translate."
  :type  '(repeat
           (cons :tag "Rule"
                 (string :tag "Letter before the converting.")
                 (string :tag "Letter after the converting.")))
  :group 'text-translator)

(defcustom text-translator-post-string-replace-alist
  '(("\r" . "") ("&#39;" . "'") ("&quot;" . "\"")
    ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&#8211;" . "-")
    ("&#264;" . "Ĉ") ("&#265;" . "ĉ") ("&#284;" . "Ĝ") ("&#285;" . "ĝ")
    ("&#292;" . "Ĥ") ("&#293;" . "ĥ") ("&#308;" . "Ĵ") ("&#309;" . "ĵ")
    ("&#348;" . "Ŝ") ("&#349;" . "ŝ") ("&#364;" . "Ŭ") ("&#365;" . "ŭ"))
  "*Rule that converts character string after the translation."
  :type  '(repeat
           (cons :tag "Rule"
                 (string :tag "Letter before the converting.")
                 (string :tag "Letter after the converting.")))
  :group 'text-translator)

(defcustom text-translator-proxy-server
  (let ((proxy (or (getenv "HTTP_PROXY") "")))
    (and (string-match "^\\(http://\\)?\\(.+\\):\\([0-9]+\\)" proxy)
         (match-string 2 proxy)))
  "*Proxy server used."
  :type  '(choice (string :tag "specify proxy")
                  (const :tag "not use proxy" nil))
  :group 'text-translator)

(defcustom text-translator-proxy-port
  (let ((proxy (or (getenv "HTTP_PROXY") "")))
    (or (and (string-match "^\\(http://\\)?\\(.+\\):\\([0-9]+\\)" proxy)
             (string-to-number (match-string 3 proxy)))
        8080))
  "*Proxy port number used."
  :type  'integer
  :group 'text-translator)

(defcustom text-translator-proxy-user nil
  "*Basic proxy authorization user name."
  :type  '(choice (string :tag "Basic proxy authorization user name")
                  (const :tag "Not use Basic proxy authorization" nil))
  :group 'text-translator)

(defcustom text-translator-proxy-password nil
  "*Basic proxy authorization password."
  :type  '(choice (string :tag "Basic proxy authorization password")
                  (const :tag "Not use Basic proxy authorization" nil))
  :group 'text-translator)

(defcustom text-translator-default-engine "google.com_enja"
  "*Translation engine used by default."
  :type  (cons 'radio
               (mapcar
                (lambda (x)
                  (list 'const (car x)))
                text-translator-site-data-alist))
  :group 'text-translator)

(defcustom text-translator-user-agent
;;  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.4) Gecko/20070515 Firefox/2.0.0.4"
  "Mozilla/5.0 (Windows; U; Windows NT 5.0; ja; rv:1.9) Gecko/2008052906 Firefox/3.0"
  "*text-translator's User Agent. Default is Firefox."
  :type  'string
  :group 'text-translator)

(defcustom text-translator-mode-hook nil
  "*Hook run at the end of function `text-translator-mode'."
  :type 'hook
  :group 'text-translator)

(defcustom text-translator-auto-selection-func nil
  "*Value is function that select translation engine automatic.
this value is function for `text-translator-translate-by-auto-selection'."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-display-popup nil
  "*Non-nil means translated message is displayed by using popup-tip.
To use this option, you have to require popup.el.
popup.el URL: http://github.com/m2ym/auto-complete"
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-do-fill-region nil
  "*Default is nil. if value is non-nil, it deletes
linefeed\\(and CR\\) from pre translation string(\"\\n\" -> \" \",
\"\r\" -> \"\"). and processing to straighten faces with
fill-paragraph after the translation. it is countermeasure
against the translation engines that processes per line."
  :type 'symbol
  :group 'text-translator)

(defcustom text-translator-space-division-languages
  '("en" "es" "fr" "de" "it" "pt" "ru" "nl" "el" "no")
  "*List of language that word is delimited by blank."
  :type '(repeat (string :tag "language(2char)"))
  :group 'text-translator)

(defvar text-translator-all-history nil
  "")

(defvar text-translator-display-function nil
  "*")

(defvar text-translator-timeout-interval 3.00
  "*")

(defvar text-translator-debug nil
  "*")

(defvar text-translator-all-results nil)

(defvar text-translator-all-site-number nil)

(defvar text-translator-all-before-string nil)

(defvar text-translator-processes-alist nil)

(defvar text-translator-sitedata-hash nil)

(defvar text-translator-timeout nil)


(provide 'text-translator-vars)
;;; text-translator-vars.el ends here

;; Local Variables:
;; Coding: utf-8
;; End:
