;;; text-translator-site-excite-cojp.el --- Text Translator

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

;; Site data (excite.co.jp) for text-translator.el

;;; Code:

(defvar text-translator-site-excite-cojp
  '(("excite.co.jp"
     "www.excite.co.jp"
     "/world/english/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("EN" . "JA") ("JA" . "EN"))
     (("EN" . "en") ("JA" . "ja")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "CH") ("CH" . "JA"))
     (("JA" . "ja") ("CH" . "ch")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.1"
     "wb_lp=%o%t&big5=yes&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "CH") ("CH" . "JA"))
     (("JA" . "ja") ("CH" . "tw")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/korean/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "KO") ("KO" . "JA"))
     (("JA" . "ja") ("KO" . "ko")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/french/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "FR") ("FR" . "JA")
      ("EN" . "FR") ("FR" . "EN"))
     (("JA" . "ja") ("FR" . "fr")
      ("EN" . "en")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/german/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "DE") ("DE" . "JA")
      ("EN" . "DE") ("DE" . "EN"))
     (("JA" . "ja") ("DE" . "de")
      ("EN" . "en")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/italian/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "IT") ("IT" . "JA")
      ("EN" . "IT") ("IT" . "EN"))
     (("JA" . "ja") ("IT" . "it")
      ("EN" . "en")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/spanish/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "ES") ("ES" . "JA")
      ("EN" . "ES") ("ES" . "EN"))
     (("JA" . "ja") ("ES" . "es")
      ("EN" . "en")))

    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/portuguese/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     text-translator-site-excite-cojp--extract
     (("JA" . "PT") ("PT" . "JA")
      ("EN" . "PT") ("PT" . "EN"))
     (("JA" . "ja") ("PT" . "pt")
      ("EN" . "en")))))

(defun text-translator-site-excite-cojp--extract ()
  (text-translator-extract-tag-exclusion-string
   "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"))

(provide 'text-translator-site-excite-cojp)

;;; text-translator-site-excite-cojp.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
