;;; text-translator-site-livedoor-com.el --- Text Translator

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

;; Site data (livedoor.com) for text-translator.el

;;; Code:

(require 'json)

(defconst text-translator-site-livedoor-com--request
  (mapconcat #'identity
             '("m=ajaxExecute"
               "translateParams.slang=%o"
               "translateParams.tlang=%t"
               "translateParams.originalText=%s"
               "translateParams.langDetect=N")
             "&"))

(defvar text-translator-site-livedoor-com
  `(("livedoor.com"
     "GET"
     "http://livedoor-translate.naver.jp/text/"
     ,text-translator-site-livedoor-com--request
     utf-8-dos
     text-translator-site-livedoor-com--extract
     (("en" . "ja") ("en" . "de") ("en" . "fr")
      ("en" . "it") ("en" . "es") ("en" . "pt")
      ("de" . "en") ("fr" . "en") ("it" . "en")
      ("es" . "en") ("pt" . "en")
      ("ja" . "en") ("ja" . "de") ("ja" . "fr")
      ("ja" . "it") ("ja" . "es") ("ja" . "pt")
      ("ja" . "ko") ("ja" . "zh")
      ("de" . "ja") ("fr" . "ja") ("it" . "ja")
      ("es" . "ja") ("pt" . "ja") ("ko" . "ja")
      ("zh" . "ja"))
     (("zh" . "ch")))))

(defun text-translator-site-livedoor-com--extract ()
  (let ((json (json-read))
        res)
    (when (setq res (assoc 'translateJsonResult json))
      (let ((array (json-read-from-string (cdr res)))
            translated)
        (when (setq translated (assoc 'translatedText (aref array 0)))
          (cdr translated))))))

(provide 'text-translator-site-livedoor-com)

;;; text-translator-site-livedoor-com.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
