;;; text-translator-sites.el --- Text Translator

;; Copyright (C) 2007-2014  HAMANO Kiyoto

;; Author: HAMANO Kiyoto <khiker.mail+elisp@gmail.com>
;;         plus          <MLB33828@nifty.com>

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

;; Site data for text-translator.el

;;; Code:

(require 'text-translator-site-google-com)
(require 'text-translator-site-freetranslation-com)
(require 'text-translator-site-livedoor-com)
(require 'text-translator-site-fresheye-com)
(require 'text-translator-site-yahoo-cojp)
(require 'text-translator-site-excite-cojp)

(require 'text-translator-site-lou5-jp)
(require 'text-translator-site-tatoeba-org)
(require 'text-translator-site-traduku-net)

;; Variables:


(defcustom text-translator-site-data-template-alist
  (let ((res '()))
    (dolist (site
             `(,text-translator-site-google-com
               ,text-translator-site-freetranslation-com
               ,text-translator-site-livedoor-com
               ,text-translator-site-fresheye-com
               ,text-translator-site-yahoo-cojp
               ,text-translator-site-excite-cojp))
      (dolist (def site)
        (push def res)))
    (nreverse res))
  "The alist where setting of the site which is used for text translation is
described. To update site-data, evalute `text-translator-site-data-init`."
  :type '(repeat
          (list :tag "Web Site"
                (string :tag "Web site name and translation type")
                (string :tag "HTTP method (GET or POST)")
                (string :tag "Request URL")
                (string :tag "Parameter of HTTP request")
                (symbol :tag "Character code")
                (choice (string :tag "regexp") (symbol :tag "function"))
                (list :tag (concat "The correspondence of translation-able "
                                   "name (used by translation sites)"))
                (list :tag (concat "The correspondence of name that used by "
                                   "text-translator and name that used by "
                                   "translation sites"))))
  :group 'text-translator)

(defcustom text-translator-site-data-minimum-alist
  (let ((res '()))
    (dolist (site
             `(,text-translator-site-lou5-jp
               ,text-translator-site-tatoeba-org
               ,text-translator-site-traduku-net))
      (dolist (def site)
        (push def res)))
    (nreverse res))
  "*The alist where setting of the site which is used for text translation is
described. To update site-data, evalute `text-translator-site-data-init`."
  :type  '(repeat
           (list :tag "Web Site"
                 (string :tag "Web site name and translation type")
                 (string :tag "Request URL")
                 (string :tag "HTTP method (GET or POST)")
                 (string :tag "Parameter of HTTP request")
                 (symbol :tag "Character code")
                 (choice (string :tag "regexp") (symbol :tag "function"))))
  :group 'text-translator)

;; text-translator-site-data-template-alist
;; + text-translator-site-data-minimum-alist
;; = text-translator-site-data-alist
(defvar text-translator-site-data-alist nil
  "The alist where setting of the site which is used for text translation is
described.")

(provide 'text-translator-sites)

;;; text-translator-sites.el ends here

;; Local Variables:
;; Coding: utf-8
;; indent-tabs-mode: nil
;; End:
