;;; text-translator-site-lou5-jp.el --- Text Translator

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

;; Site data (lou5.jp) for text-translator.el

;;; Code:

(defconst text-translator-site-lou5-jp--request
  (mapconcat #'identity
             '("v=1"
               "text=%s")
             "&"))

(defvar text-translator-site-lou5-jp
  `(("lou5.jp_*normal"
     "http://lou5.jp/"
     "GET"
     ,text-translator-site-lou5-jp--request
     utf-8
     text-translator-site-lou5-jp--extract)))

(defun text-translator-site-lou5-jp--extract ()
  (text-translator-extract-tag-exclusion-string
   "<p class=\"large align-left box\">\\(\\(.\\|\n\\)*?\\)</p>" t))

(provide 'text-translator-site-lou5-jp)

;;; text-translator-site-lou5-jp.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
