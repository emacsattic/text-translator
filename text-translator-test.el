;;; text-translator-test.el --- Text Translator

;; Copyright (C) 2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>

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

;; test code for test-translator.el.
;;
;; M-x text-translator-test or make test

;;; Code:

(require 'text-translator-vars)
(require 'text-translator)


;; Variables:

(defvar text-translator-test-google.com
  '("google.com" . (("en" "ja" "Japan" "日本")
                    ("ja" "en" "日本" "Japan")
                    ("en" "es" "Japan" "Japón")
                    ("es" "en" "Japón" "Japan")
                    ("en" "fr" "Japan" "Le Japon")
                    ("fr" "en" "Le Japon" "Japan")
                    ("en" "de" "English" "Englisch")
                    ("de" "en" "Englisch" "English")
                    ("en" "it" "English" "Inglese")
                    ("it" "en" "Inglese" "English")
                    ("en" "ar" "Japan" "اليابان")
                    ("ar" "en" "اللغة الإنجليزية" "English")
                    ("de" "fr" "Englisch" "En anglais")
                    ("fr" "de" "En anglais" "Englisch")
                    ("en" "pt" "Japan" "Japão")
                    ("pt" "en" "Japão" "Japan")
                    ("en" "ru" "Japan" "Япония")
                    ("ru" "en" "Япония" "Japan")
                    ("en" "ko" "Japan" "일본")
                    ("ko" "en" "일본" "Japan")
                    ("en" "ch" "Hello, World" "你好，世界")
                    ("ch" "en" "你好，世界" "Hello, world")
                    ("en" "tw" "China" "中國")
                    ("tw" "en" "中國" "China")
                    ("ch" "tw" "中国" "中國")
                    ("tw" "ch" "中國" "中国")))
  "")

(defvar text-translator-test-yahoo.com
  '("yahoo.com" .
    (("en" "ja" "Japan" "日本")
     ("ja" "en" "日本" "Japan")
     ("en" "fr" "Japan" "Le Japon")
     ("fr" "en" "Le Japon" "Japan")
     ("en" "de" "English" "Englisch")
     ("de" "en" "Englisch" "English")
     ("en" "el" "English" "Αγγλικά")
     ("el" "en" "Αγγλικά" "English")
     ("en" "ko" "Japan" "일본")
     ("ko" "en" "일본" "Japan")
     ("en" "pt" "Japan" "Japão")
     ("pt" "en" "Japão" "Japan")
     ("en" "ru" "Japan" "Япония")
     ("ru" "en" "Япония" "Japan")
     ("en" "es" "Japan" "Japón")
     ("es" "en" "Japón" "Japan")
     ("en" "nl" "Emacs is a great interpreter." "Emacs is een groot tolk.")
     ("nl" "en" "Emacs is een groot tolk." "Emacs are large an interpreter.")
     ("fr" "de" "En anglais" "Auf englisch")
     ("nl" "fr" "Emacs is een groot tolk." "Emacs est grand un interprète.")
     ("fr" "el" "En anglais" "Στους Άγγλους")
     ("el" "fr" "Στους Άγγλους" "Aux Anglais")
     ("fr" "it" "Le Japon" "Il Giappone")
     ("it" "fr" "Il Giappone" "Le Japon")
     ("fr" "pt" "Le Japon" "O Japão")
     ("pt" "fr" "O Japão" "Le Japon")
     ("fr" "es" "Le Japon" "Japón")
     ("es" "fr" "Japón" "Le Japon")
     ("en" "tw" "China" "中國")
     ("tw" "en" "中國" "China")
     ("en" "ch" "Hello, World" "你好，世界")
     ("ch" "en" "你好，世界" "You are good, world")))
  "")


;; Functions:

(defun text-translator-test-internal (site data &optional wait)
  (let (engine before after translated status)
    (dolist (i data)
      (setq text-translator-all-site-number   1
            text-translator-all-results       nil
            text-translator-processes-alist   nil
            text-translator-all-before-string nil
            engine (concat site "_" (nth 0 i) (nth 1 i))
            before (nth 2 i)
            after  (nth 3 i))
      (when (and before after)
        (cond
         ((not (string=
                (setq translated
                      (prog1 (text-translator-client engine before nil t)
                        (text-translator-timeout-start)))
                after))
          (princ (format "NG: %s: %s != %s\n" engine after translated)))
         (t
          (princ (format "OK: %s: %s == %s\n" engine after translated))
          (setq status t))))
      (when (and wait (numberp wait))
        (sit-for wait)))
    status))

(defun text-translator-test-google.com ()
  (let ((site-val text-translator-test-google.com))
    (princ (format ";; %s\n" (car site-val)))
  (text-translator-test-internal (car site-val) (cdr site-val))))

(defun text-translator-test-yahoo.com ()
  (let ((site-val text-translator-test-yahoo.com))
    (princ (format ";; %s\n" (car site-val)))
  (text-translator-test-internal (car site-val) (cdr site-val))))

(defun text-translator-test ()
  (interactive)
  (and (text-translator-test-google.com)
       (text-translator-test-yahoo.com)))


(provide 'text-translator-test)

;;; text-translator-test.el ends here
