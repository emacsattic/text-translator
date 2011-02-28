;;; text-translator-window.el --- Text Translator

;; Copyright (C) 2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

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

;; The window support for text-translator.el
;; Display translated strings to another buffer (window).

;;; Code:

(require 'text-translator-vars)


;; Functions:

(defun text-translator-window-display ()
  "Display a translation result by `pop-to-buffer'.
If you want to use this function for displaying translation
result, please add a following code to your .emacs.

\(setq text-translator-display-function 'text-translator-window-display\)"
  (let ((buf (get-buffer-create text-translator-buffer))
        (window-min-height
         (if (> text-translator-window-min-height (/ (frame-height) 2))
             (/ (frame-height) 2)
           (1+ text-translator-window-min-height)))
        engine)
    ;; Create contents of buffer.
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (text-translator-window-mode)
      ;; Todo: Insert a process of when
      ;; `text-translator-do-fill-region' is t.
      (when text-translator-leave-string
        (insert (concat (propertize "---- original -----"
                                    'face font-lock-keyword-face)
                        "\n\n" (nth 2 (car text-translator-all-before-string))
                        "\n\n")))
      (cond
       ((= 1 text-translator-all-site-number)
        (when text-translator-leave-string
          (insert (concat
                   (propertize
                    (format "---- %s -----"
                            (nth 1 (car text-translator-all-before-string)))
                    'face font-lock-keyword-face)
                   "\n\n")))
        (insert (cdar text-translator-all-results))
        (setq engine (substring
                      (caar text-translator-all-results)
                      (length text-translator-buffer))))
       (t
        (insert
         (mapconcat #'(lambda (x)
                        (let ((engine (substring
                                       (car x)
                                       (length text-translator-buffer)))
                              (str (cdr x)))
                          (concat (propertize (format "----- %s -----" engine)
                                              'face font-lock-keyword-face)
                                  "\n\n" str "\n")))
                    (sort text-translator-all-results
                          #'(lambda (x y) (string< (car x) (car y))))
                    "\n"))
        (setq engine (format "all(%s)"
                             (substring
                              (caar text-translator-all-results)
                              -4)))))
      ;; update mode-line
      (setq mode-line-buffer-identification
            `("%b [" ,engine "]")))
    ;; Display buffer.
    (save-selected-window
      (pop-to-buffer buf)
      (set-buffer-modified-p nil)
      (let ((window (get-buffer-window text-translator-buffer)))
        (when (and text-translator-auto-window-adjust
                   (window-live-p window))
          (balance-windows)
          (shrink-window-if-larger-than-buffer window)))
      (ding)
      (message "Translating...done"))))


;;;; major-mode text-translator-window-mode

;; variables for major mode
(defvar text-translator-window-mode nil)
(defvar text-translator-window-mode-map nil)
(defvar text-translator-window-mode-pkey-map nil)
(defvar text-translator-window-mode-syntax-table nil)
(defvar text-translator-window-mode-abbrev-table nil)
(define-abbrev-table 'text-translator-window-mode-abbrev-table ())

;; keymap definition
(unless text-translator-window-mode-map
  (define-prefix-command 'text-translator-window-mode-pkey-map)
  (define-key text-translator-window-mode-pkey-map
    "\C-q" 'text-translator-window-quit)
  (define-key text-translator-window-mode-pkey-map
    "\C-a" 'text-translator-translate-recent-type)
  (define-key text-translator-window-mode-pkey-map
    "\C-l" 'text-translator-display-last-string)
  (define-key text-translator-window-mode-pkey-map
    "\C-d" 'text-translator-translate-default)
  (define-key text-translator-window-mode-pkey-map
    "\C-s" 'text-translator-toggle-leave-string))

;; major-mode
(defun text-translator-window-mode ()
  "Major mode for text-translator."
  (kill-all-local-variables)
  (setq local-abbrev-table text-translator-window-mode-abbrev-table)
  (set-syntax-table text-translator-window-mode-syntax-table)
  (setq mode-name text-translator-window-mode-name)
  (setq major-mode 'text-translator-window-mode)
  (setq text-translator-window-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map text-translator-window-prefix-key
            text-translator-window-mode-pkey-map)
          map))
  (use-local-map text-translator-window-mode-map)
  (run-hooks 'text-translator-window-mode-hook))

;; syntax-table
(unless text-translator-window-mode-syntax-table
  (setq text-translator-window-mode-syntax-table (make-syntax-table)))

;; functions for major-mode
(defun text-translator-window-quit ()
  "Function that closes buffer for text-translator.
If window only have *translated* buffer, change another buffer."
  (interactive)
  (bury-buffer)
  (unless (one-window-p)
    (delete-window)))

;; Todo: Delete this function.
(defun text-translator-toggle-leave-string ()
  "Function that change value of `text-translator-leave-string'.
Toggle to display a translation result buffer of character
string that used last time."
  (interactive)
  (setq text-translator-leave-string (not text-translator-leave-string))
  (message "Pretranslational string switched %s to leave."
           (if text-translator-leave-string "" " not")))

;; Todo: Delete this function.
(defun text-translator-display-last-string (arg)
  "Function that displays translated character string last time.
Default display to minibuffer.
With prefix-arg, insert buffer."
  (interactive "P")
  (let ((last-str (nth 1 (caar text-translator-all-history))))
    (if arg
        (insert last-str)
      (message "%s" last-str))))

(defun text-translator-translate-recent-type ()
  "Function that translates by type corresponding to the language
that used last time.
For example, last time, if you have used excite.co.jp_enja,
this time select from **_enja, and, translates."
  (interactive)
  (let* ((minibuffer-history
          (let (engines)
            (dolist (i text-translator-all-history)
              (dolist (j i)
                (setq engines (cons (nth 0 j) engines))))
            (nreverse engines)))
         (engine (nth 0 (caar text-translator-all-history)))
         (last-type
          (concat "_" (text-translator-get-engine-type-or-site engine)))
         (type (completing-read
                (format "Select translation engine (last %s): " engine)
                (delq nil
                      (mapcar
                       (lambda (x)
                         (when (string-match last-type (car x))
                           x))
                       text-translator-site-data-alist))
                nil t)))
    (unless (string= "" type)
      (text-translator-client type (nth 1 (caar text-translator-all-history))))))

(defun text-translator-translate-default ()
  "Function that translates by default type only.
Default is value of `text-translator-default-engine'."
  (interactive)
  (text-translator nil nil text-translator-default-engine))


(provide 'text-translator-window)

;;; text-translator-window.el ends here
