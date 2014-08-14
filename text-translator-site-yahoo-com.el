;;; text-translator-site-yahoo-com.el --- Text Translator

;; Copyright (C) 2014  HAMANO kiyoto

;; Author: HAMANO kiyoto <khiker.mail@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Site data (yahoo.com) for text-translator.el

;;; Code:

(defconst text-translator-site-yahoo-com--request
  (mapconcat #'identity
             '("ei=UTF-8"
               "doit=done"
               "intl=1"
               "tt=urltext"
               "trtext=%s"
               "lp=%o_%t"
               "btnTrTxt=Translate")
             "&"))

(defvar text-translator-site-yahoo-com
  `(("yahoo.com"
     "GET"
     "http://babelfish.yahoo.com/translate_txt/"
     ,text-translator-site-yahoo-com--request
     utf-8-dos
     text-translator-site-yahoo-com--extract
     (("en" . "ja") ("ja" . "en")
      ("en" . "nl") ("nl" . "en")
      ("en" . "fr") ("fr" . "en")
      ("en" . "de") ("de" . "en")
      ("en" . "el") ("el" . "en")
      ("en" . "it") ("it" . "en")
      ("en" . "ko") ("ko" . "en")
      ("en" . "pt") ("pt" . "en")
      ("en" . "ru") ("ru" . "en")
      ("en" . "es") ("es" . "en")
      ("nl" . "fr")
      ("fr" . "de")
      ("fr" . "el") ("el" . "fr")
      ("fr" . "it") ("it" . "fr")
      ("fr" . "pt") ("pt" . "fr")
      ("fr" . "es") ("es" . "fr")
      ("en" . "zh") ("zh" . "en")
      ("en" . "zt") ("zt" . "en"))
     (("zh" . "ch") ("zt" . "tw")))))

(defun text-translator-site-yahoo-com--extract ()
  (text-translator-extract-tag-exclusion-string
   "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>"))

(provide 'text-translator-site-yahoo-com)

;;; text-translator-site-yahoo-com.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
