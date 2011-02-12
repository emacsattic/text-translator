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

(defvar text-translator-test-display-OK nil
  "*")

(defvar text-translator-test-google.com
  '("google.com" .
    (("en" "ja" "Japan" "日本")
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

(defvar text-translator-test-freetranslation.com
  '("freetranslation.com" .
    (("en" "es" "Japan" "Japón")
     ("es" "en" "Japón" "Japan")
     ("en" "fr" "Japan" "Le Japon")
     ("fr" "en" "Le Japon" "Japan")
     ("en" "de" "English" "Englisch")
     ("de" "en" "Englisch" "English")
     ("en" "it" "English" "Inglesi")
     ("it" "en" "Inglesi" "English")
     ("en" "nl" "English" "Engels")
     ("nl" "en" "Engels" "Angels")
     ("en" "pt" "Japan" "Japão")
     ("pt" "en" "Japão" "Japan")
     ("en" "no" "English" "Engelsk")
     ("en" "ru" "English" "Английский язык")
     ("ru" "en" "Английский язык" "English language")
     ("en" "ch" "English" "英语")
     ("en" "tw" "English" "英語")
     ("en" "ja" "English" "英語")
     ("ja" "en" "英語" "English   ")))
  "")

(defvar text-translator-test-livedoor.com
  '("livedoor.com" .
    (("en" "ja" "English" "英語")
     ("ja" "en" "英語" "English")
     ("ja" "ko" "英語" "영어")
     ("ko" "ja" "영어" "英語")
     ("ja" "ch" "英語" "英语")
     ("ch" "ja" "英语" "英語")))
  "")


;; Functions:

(defun text-translator-test-internal (site data &optional wait)
  (let (engine before after translated status errors successes)
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
                      (prog2
                          (text-translator-timeout-start)
                          (text-translator-client engine before nil t)
                        (text-translator-timeout-stop)))
                after))
          (princ (format "NG: %s: '%s' != '%s'\n"
                         engine after translated))
          (setq errors (cons (cons (nth 0 i) (nth 1 i)) errors)))
         (t
          (when text-translator-test-display-OK
            (princ (format "OK: %s: %s == %s\n"
                           engine after translated)))
          (setq successes (cons (cons (nth 0 i) (nth 1 i)) successes))
          (setq status t))))
      (when (and wait (numberp wait))
        (sit-for wait)))
    (when errors
      (princ (format ";; > FAILED: %s\n" site))
      (dolist (i errors)
        (princ (format ";;     %s%s\n" (car i) (cdr i))))
      (princ "\n"))
    (when successes
      (princ (format ";; > PASSED: %s\n" site))
      (dolist (i successes)
        (princ (format ";;     %s%s\n" (car i) (cdr i))))
      (princ "\n"))
    status))

(defun text-translator-test-google.com ()
  (let ((site-val text-translator-test-google.com))
    (princ (format ";; %s\n" (car site-val)))
    (text-translator-test-internal (car site-val) (cdr site-val))))

(defun text-translator-test-yahoo.com ()
  (let ((site-val text-translator-test-yahoo.com)
        (text-translator-timeout-interval 5)
        (sleep-wait 3))
    (princ (format ";; %s\n" (car site-val)))
    (text-translator-test-internal (car site-val) (cdr site-val))))

(defun text-translator-test-freetranslation.com ()
  (let ((site-val text-translator-test-freetranslation.com)
        (text-translator-timeout-interval nil)
        (sleep-wait 2))
    (princ (format ";; %s\n" (car site-val)))
    (text-translator-test-internal (car site-val) (cdr site-val) sleep-wait)))

(defun text-translator-test-livedoor.com ()
  (let ((site-val text-translator-test-livedoor.com))
    (princ (format ";; %s\n" (car site-val)))
    (text-translator-test-internal (car site-val) (cdr site-val))))

(defun text-translator-test ()
  (interactive)
  ;; 全走査
  (progn
    (text-translator-test-google.com)
    (text-translator-test-yahoo.com)
    (text-translator-test-freetranslation.com)
    (text-translator-test-livedoor.com)))


(provide 'text-translator-test)

;;; text-translator-test.el ends here
