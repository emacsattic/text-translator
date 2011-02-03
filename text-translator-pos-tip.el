;;; text-translator-pos-tip.el --- Text Translator

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

;; `pos-tip' support for text-translator.el
;;
;; Todo: do customizable: face, timeout etc...

;;; Code:

(require 'pos-tip)
(require 'text-translator-vars)


;; Variables:

(defvar text-translator-pos-tip-timeout 0
  "*")

(defvar text-translator-pos-tip-tip-color nil
  "*")

(defvar text-translator-pos-tip-separator-face 'font-lock-keyword-face
  "*")


;; Functions:

(defun text-translator-pos-tip-display ()
  (ding)
  (message "Translating...done")
  (cond
   ((= 1 text-translator-all-site-number)
    (text-translator-pos-tip-show (cdar text-translator-all-results)))
   (t
    (text-translator-pos-tip-show
     (mapconcat
      #'(lambda (x)
          (let ((engine  (substring (car x)
                                    (length text-translator-buffer)))
                (str     (cdr x)))
            (concat (propertize (concat "----- " engine " -----")
                                'face text-translator-pos-tip-separator-face)
                    "\n\n" str "\n")))
      (sort text-translator-all-results
            #'(lambda (x y) (string< (car x) (car y))))
      "\n")))))

(defun text-translator-pos-tip-show (msg)
  (pos-tip-show-no-propertize
   msg
   text-translator-pos-tip-tip-color
   nil
   nil
   text-translator-pos-tip-timeout))


(provide 'text-translator-pos-tip)

;;; text-translator-pos-tip.el ends here
