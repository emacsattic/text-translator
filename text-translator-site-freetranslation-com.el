;;; text-translator-site-freetranslation-com.el --- Text Translator

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

;; Site data (freetranslation.com) for text-translator.el

;;; Code:

;; Freetranslation english -> arabia
;;
;; - lwSrc  : src language (english, eng)
;; - lwDest : dst language (arabia,  ara)
;; - LwPair : magic number (calced by javascript code)
;; - srcText : source text
;;
;; Since the calculation of LwPair have to see javascript code,
;; Text-translator currently is not support a freetranslation eng -> ara
;; translation.

(defconst text-translator-site-freetranslation-com--request
  (mapconcat #'identity
             '("sequence=core"
               "mode=html"
               "charset=UTF-8"
               "template=results_en-us.htm"
               "language=%o/%t"
               "srctext=%s")
             "&"))

(defvar text-translator-site-freetranslation-com
  `(("freetranslation.com"
     "GET"
     "http://ets.freetranslation.com/"
     ,text-translator-site-freetranslation-com--request
     utf-8
     text-translator-site-freetranslation-com--extract
     (("English" . "Spanish")    ("Spanish" . "English")
      ("English" . "French")     ("French" . "English")
      ("English" . "German")     ("German" . "English")
      ("English" . "Italian")    ("Italian" . "English")
      ("English" . "Dutch")      ("Dutch" . "English")
      ("English" . "Portuguese") ("Portuguese" . "English")
      ("English" . "Norwegian"))
     (("English"    . "en")
      ("Spanish"    . "es")
      ("French"     . "fr")
      ("German"     . "de")
      ("Italian"    . "it")
      ("Dutch"      . "nl")
      ("Portuguese" . "pt")
      ("Norwegian"  . "no")))

    ("freetranslation.com"
     "GET"
     "http://ets6.freetranslation.com/"
     ,text-translator-site-freetranslation-com--request
     utf-8
     text-translator-site-freetranslation-com--extract
     (("English" . "Russian") ("Russian" . "English")
      ("English" . "SimplifiedChinese")
      ("English" . "TraditionalChinese"))
     (("English" . "en")
      ("Russian" . "ru")
      ("SimplifiedChinese" . "ch")
      ("TraditionalChinese" . "tw")))

    ("freetranslation.com"
     "GET"
     "http://tets9.freetranslation.com/"
     ,text-translator-site-freetranslation-com--request
     utf-8-dos
     text-translator-site-freetranslation-com--extract
     (("English" . "Japanese") ("Japanese" . "English"))
     (("English" . "en")
      ("Japanese" . "ja")))))


(defun text-translator-site-freetranslation-com--extract ()
  (text-translator-extract-tag-exclusion-string
   (concat "<textarea "
           "name=\"dsttext\" "
           "cols=\"40\" "
           "rows=\"6\" "
           "style=\"width:99%;height:142px;\" "
           "id=\"resultsBox\">"
           "\\([^<]*\\)"
           "</textarea>")))

(provide 'text-translator-site-freetranslation-com)

;;; text-translator-site-freetranslation-com.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
