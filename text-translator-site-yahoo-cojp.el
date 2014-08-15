;;; text-translator-site-yahoo-cojp.el --- Text Translator

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

;; Site data (yahoo.co.jp) for text-translator.el

;;; Code:

;; Yahoo.co.jp requires a value of hidden input tag that have id
;; "TTcrumb" to translate.  So, Text-translator have to visit a
;; http://honyaku.yahoo.co.jp once.  The current text-translator do not
;; support a mechanism corresponding to this.  So, Text-translator
;; currently commented out a supporting yahoo.co.jp.

(defvar text-translator-site-yahoo-cojp
  '(("yahoo.co.jp"
     "GET"
     "http://honyaku.yahoo.co.jp//transtext/"
     "both=TH&text=%s&clearFlg=1&eid=CR-%o%t"
     utf-8
     text-translator-site-yahoo-cojp--extract
     (("E" . "J") ("J" . "E")
      ("C" . "J") ("J" . "C-CN")
      ("K" . "J") ("J" . "K"))
     (("E" . "en") ("J" . "ja")
      ("C" . "ch") ("C-CN" . "ch")
      ("K" . "ko")))))

(defun text-translator-site-yahoo-cojp--extract ()
  (text-translator-extract-tag-exclusion-string
   (concat "<textarea id=\"trn_textText\" [^>]*>"
           "\\([^<]*\\)"
           "</textarea>")))

(provide 'text-translator-site-yahoo-cojp)

;;; text-translator-site-yahoo-cojp.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
