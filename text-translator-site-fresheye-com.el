;;; text-translator-site-fresheye-com.el --- Text Translator

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

;; Site data (fresheye.com) for text-translator.el

;;; Code:

(defvar text-translator-site-fresheye-com
  '(("fresheye.com"
     "GET"
     "http://mt.fresheye.com/ft_result.cgi"
     "gen_text=%s&e=%o%t"
     utf-8-dos
     text-translator-site-fresheye-com--extract
     (("E" . "J") ("J" . "E"))
     (("E" . "en") ("J" . "ja")))

    ("fresheye.com"
     "GET"
     "http://mt.fresheye.com/ft_cjresult.cgi"
     "gen_text=%s&charset=gb2312&cjjc=%o%t"
     utf-8
     text-translator-site-fresheye-com--extract
     (("c" . "j") ("j" . "c"))
     (("c" . "ch") ("j" . "ja")))

    ("fresheye.com"
     "GET"
     "http://mt.fresheye.com/ft_cjresult.cgi"
     "gen_text=%s&charset=big5&cjjc=%o%t"
     utf-8
     text-translator-site-fresheye-com--extract
     (("c" . "j") ("j" . "c"))
     (("c" . "tw") ("j" . "ja")))))

(defun text-translator-site-fresheye-com--extract ()
  (text-translator-extract-tag-exclusion-string
   (concat
    "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">"
    "\\([^<]*\\)"
    "</TEXTAREA>")))

(provide 'text-translator-site-fresheye-com)

;;; text-translator-site-fresheye-com.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
