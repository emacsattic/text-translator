;;; text-translator-site-traduku-net.el --- Text Translator

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

;; Site data (traduku.net) for text-translator.el

;;; Code:

(defvar text-translator-site-traduku-net
  `(("traduku.net_eoen"
     "http://lingvo.org/cgi-bin/traduku"
     "GET"
     "eo_en_trukilo&u=%s"
     utf-8
     " id=\"rezulto\">\\(\\(.\\|\n\\)*?\\)</div>")

    ("traduku.net_eneo"
     "http://lingvo.org/cgi-bin/traduku"
     "GET"
     "u=%s&en_eo_trukilo"
     utf-8
     " id=\"rezulto\">\\(\\(.\\|\n\\)*?\\)</div>")))

(provide 'text-translator-site-traduku-net)

;;; text-translator-site-traduku-net.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
