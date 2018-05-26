;;; line-reminder.el --- Remind current line status by current buffer.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-05-25 15:10:29

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Remind current line status by current buffer.
;; Keyword: customize font ttf
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (s "1.12.0"))
;; URL: https://github.com/jcs090218/line-reminder

;; This file is NOT part of GNU Emacs.

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
;;
;; Remind current line status by current buffer.
;;

;;; Code:


(defgroup line-reminder nil
  "Reminder what is the status of each line for current buffer/file."
  :prefix "line-reminder:"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/line-reminder.git"))


(defcustom line-reminder:linum-left-string ""
  "String on the left side of the line number.")

(defcustom line-reminder:linum-right-string "  "
  "String on the right side of the line number.")

(defcustom line-reminder:modified-sign "▐"
  "Modified sign."
  :group 'line-reminder)

(defcustom line-reminder:saved-sign "▐"
  "Saved sign."
  :group 'line-reminder)

(defface line-reminder:modified-sign-face
  `((t :inherit linum
       :foreground "#EFF284"))
  "Modifed sign face."
  :group 'line-reminder)

(defface line-reminder:saved-sign-face
  `((t :inherit linum
       :foreground "#577430"))
  "Modifed sign face."
  :group 'line-reminder)


(defun line-reminder:get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (line-reminder:get-current-line-string)))

(defun line-reminder:get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun line-reminder:linum-format-string ()
  "Return format string align on the right."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (format "%%%dd" w)))

(defun line-reminder:get-propertized-normal-sign (line-number)
  "Return a default propertized normal sign.
LINE-NUMBER : pass in by `linum-format' variable."
  (propertize (format (concat line-reminder:linum-left-string
                              (line-reminder:linum-format-string)
                              line-reminder:linum-right-string)
                      line-number)
              'face 'linum))

(defun line-reminder:get-propertized-modified-sign ()
  "Return a propertized modifoied sign."
  (propertize line-reminder:modified-sign 'face 'line-reminder:modified-sign-face))

(defun line-reminder:get-propertized-saved-sign ()
  "Return a propertized saved sign."
  (propertize line-reminder:saved-sign 'face 'line-reminder:saved-sign-face))


(defun line-reminder:propertized-sign-by-type (type &optional line-number)
  "Return a propertized sign string by type.
TYPE : type of the propertize sign you want.
LINE-NUMBER : Pass is line number for normal sign."
  (let ((face ""))
    (cond ((string= type 'normal)
           (progn
             (if (not line-number)
                 (error "Normal line but with no line number pass in")
               ;; Just return normal linum format.
               (setq face (line-reminder:get-propertized-normal-sign line-number)))))
          ((string= type 'modified)
           (progn
             (setq face (line-reminder:get-propertized-modified-sign))))
          ((string= type 'saved)
           (progn
             (setq face (line-reminder:get-propertized-saved-sign)))))
    face))

(defun line-reminder:linum-format (line-number)
  "Core line reminder format string logic here.
LINE-NUMBER : pass in by `linum-format' variable."
  (let ((reminder-sign "")
        (result-sign "")
        (normal-sign (line-reminder:propertized-sign-by-type 'normal line-number))
        (is-sign-exists nil))

    (cond (;; TODO(jenchieh): Implement modified algoirhtm..
           (= line-number 14)
           (progn
             (setq reminder-sign (line-reminder:propertized-sign-by-type 'modified))
             (setq is-sign-exists t)))
          (;; TODO(jenchieh): Implement saved algoirhtm..
           (= line-number 17)
           (progn
             (setq reminder-sign (line-reminder:propertized-sign-by-type 'saved))
             (setq is-sign-exists t))))

    (when is-sign-exists
      (setq normal-sign (substring normal-sign 0 (1- (length normal-sign)))))

    ;; Combnie the result format.
    (setq result-sign (concat normal-sign reminder-sign))
    result-sign))


(defun line-reminder:after-save-hook ()
  "Do stuff after save hook."
  )
(add-hook 'after-save-hook 'line-reminder:after-save-hook)

(defun jcs-test ()
  (interactive)
  ;;(highlight-changes-next-change)
  (setq linum-format 'line-reminder:linum-format)
  )
;;(setq linum-format 'line-reminder:linum-format)


(provide 'line-reminder)
;;; line-reminder.el ends here
