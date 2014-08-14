;;; text-translator-site-tatoeba-org.el --- Text Translator

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

;; Site data (tatoeba.org) for text-translator.el

;;; Code:

(defvar text-translator-site-tatoeba-org
  `(("tatoeba.org_furigana"
     "http://tatoeba.org/eng/tools/romaji_furigana"
     "GET"
     "query=%s&type=furigana"
     utf-8
     "class=\"furigana\">\\(\\(.\\|\n\\)*?\\)</div><form id=\"ToolRomajiFuriganaForm")

    ("tatoeba.org_romaji"
     "http://tatoeba.org/eng/tools/romaji_furigana"
     "GET"
     "query=%s&type=romaji"
     utf-8
     "class=\"furigana\">\\(\\(.\\|\n\\)*?\\)</div><form id=\"ToolRomajiFuriganaForm")))

(provide 'text-translator-site-tatoeba-org)

;;; text-translator-site-tatoeba-org.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
