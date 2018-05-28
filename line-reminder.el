;;; line-reminder.el --- Remind current line status by current buffer.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-05-25 15:10:29

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Remind current line status by current buffer.
;; Keyword: customize font ttf
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6"))
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

(require 'cl-lib)
(require 'linum)


(defgroup line-reminder nil
  "Reminder what is the status of each line for current buffer/file."
  :prefix "line-reminder-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/line-reminder.git"))


(defcustom line-reminder-linum-left-string ""
  "String on the left side of the line number."
  :group 'line-reminder
  :type 'string)

(defcustom line-reminder-linum-right-string " "
  "String on the right side of the line number."
  :group 'line-reminder
  :type 'string)

(defcustom line-reminder-modified-sign "▐"
  "Modified sign."
  :group 'line-reminder
  :type 'string)

(defcustom line-reminder-saved-sign "▐"
  "Saved sign."
  :group 'line-reminder
  :type 'string)

(defface line-reminder-modified-sign-face
  `((t :inherit linum
       :foreground "#EFF284"))
  "Modifed sign face."
  :group 'line-reminder)

(defface line-reminder-saved-sign-face
  `((t :inherit linum
       :foreground "#577430"))
  "Modifed sign face."
  :group 'line-reminder)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defvar-local line-reminder-change-lines '()
  "List of line that change in current temp buffer.")

(defvar-local line-reminder-saved-lines '()
  "List of line that saved in current temp buffer.")

(defvar-local line-reminder-last-buffer-max-line -1
  "Record down the current bufer's maxinum line.")

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defun line-reminder-total-line ()
  "Return current buffer's maxinum line."
  (save-excursion
    (goto-char (point-max))
    (line-reminder-get-current-line-integer)))

(defun line-reminder-get-file-name ()
  "Get current file name."
  (file-name-nondirectory buffer-file-name))

(defun line-reminder-get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (line-reminder-get-current-line-string)))

(defun line-reminder-get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun line-reminder-linum-format-string-align-right ()
  "Return format string align on the right."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (format "%%%dd" w)))

(defun line-reminder-get-propertized-normal-sign (line-number)
  "Return a default propertized normal sign.
LINE-NUMBER : pass in by `linum-format' variable."
  (propertize (format (concat line-reminder-linum-left-string
                              (line-reminder-linum-format-string-align-right)
                              line-reminder-linum-right-string)
                      line-number)
              'face 'linum))

(defun line-reminder-get-propertized-modified-sign ()
  "Return a propertized modifoied sign."
  (propertize line-reminder-modified-sign 'face 'line-reminder-modified-sign-face))

(defun line-reminder-get-propertized-saved-sign ()
  "Return a propertized saved sign."
  (propertize line-reminder-saved-sign 'face 'line-reminder-saved-sign-face))


(defun line-reminder-propertized-sign-by-type (type &optional line-number)
  "Return a propertized sign string by type.
TYPE : type of the propertize sign you want.
LINE-NUMBER : Pass is line number for normal sign."
  (let ((face ""))
    (cond ((string= type 'normal)
           (progn
             (if (not line-number)
                 (error "Normal line but with no line number pass in")
               ;; Just return normal linum format.
               (setq face (line-reminder-get-propertized-normal-sign line-number)))))
          ((string= type 'modified)
           (progn
             (setq face (line-reminder-get-propertized-modified-sign))))
          ((string= type 'saved)
           (progn
             (setq face (line-reminder-get-propertized-saved-sign)))))
    face))


(defun line-reminder-is-contain-list-integer (in-list in-int)
  "Check if a integer contain in any string in the string list.
IN-LIST : list of integer use to check if IN-INT in contain one of the integer.
IN-INT : integer using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-int) (= lb-sub-int in-int)) in-list))


(defun line-reminder-linum-format (line-number)
  "Core line reminder format string logic here.
LINE-NUMBER : pass in by `linum-format' variable."
  (let ((reminder-sign "")
        (result-sign "")
        (normal-sign (line-reminder-propertized-sign-by-type 'normal line-number))
        (is-sign-exists nil))

    (cond ((line-reminder-is-contain-list-integer line-reminder-change-lines line-number)
           (progn
             (setq reminder-sign (line-reminder-propertized-sign-by-type 'modified))
             (setq is-sign-exists t)))
          ((line-reminder-is-contain-list-integer line-reminder-saved-lines line-number)
           (progn
             (setq reminder-sign (line-reminder-propertized-sign-by-type 'saved))
             (setq is-sign-exists t))))


    ;; If the sign exist, then remove the last character from the normal sign.
    ;; So we can keep our the margin/padding the same without modifing the
    ;; margin/padding width.
    (when is-sign-exists
      (setq normal-sign (substring normal-sign 0 (1- (length normal-sign)))))

    ;; Combnie the result format.
    (setq result-sign (concat normal-sign reminder-sign))
    result-sign))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;;;###autoload
(defun line-reminder-clear-reminder-lines-sign ()
  "Clear all the reminder lines' sign."
  (interactive)
  (setq-local line-reminder-change-lines '())
  (setq-local line-reminder-saved-lines '()))

(defun line-reminder-delta-list-lines-by-bound (in-list bound delta)
  "Delta the count of line in the list by bound.
IN-LIST : input line list.
BOUND : everything above is affective by delta.
DELTA : addition/subtraction value of the line count."
  (let ((index 0))
    (dolist (tmp-linum in-list)
      (if (< delta 0)
          (when (< bound tmp-linum)
            (setf (nth index in-list) (+ tmp-linum delta)))
        (when (<= bound tmp-linum)
          (setf (nth index in-list) (+ tmp-linum delta))))
      (setq index (1+ index))))
  in-list)



(defun line-reminder-post-command-hook ()
  "Do stuff when post command."
  (when (not buffer-read-only)
    (unless (buffer-modified-p)
      ;; Transfer the `change-lines' to `saved-lines'.
      (setq-local line-reminder-saved-lines
                  (append line-reminder-saved-lines line-reminder-change-lines))
      ;; Clear the change lines.
      (setq-local line-reminder-change-lines '()))))

(defun line-reminder-after-save-hook ()
  "Do stuff after save hook."
  ;; Transfer the `change-lines' to `saved-lines'.
  (setq-local line-reminder-saved-lines
              (append line-reminder-saved-lines line-reminder-change-lines))
  ;; Clear the change lines.
  (setq-local line-reminder-change-lines '()))

(defun line-reminder-before-change-functions (begin end)
  "Do stuff before buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes."

  (when (not buffer-read-only)
    (save-excursion
      ;; When begin and end are not the same, meaning the there
      ;; is deletion happening in the current buffer.
      (unless (= begin end)
        (let ((begin-linum -1)
              (end-linum -1))
          (goto-char end)
          (setq end-linum (line-reminder-get-current-line-integer))
          (goto-char begin)
          (setq begin-linum (line-reminder-get-current-line-integer))

          (let ((current-linum begin-linum))
            (while (< current-linum end-linum)
              (setq-local line-reminder-change-lines
                          (remove (line-reminder-get-current-line-integer) line-reminder-change-lines))
              (setq-local line-reminder-saved-lines
                          (remove (line-reminder-get-current-line-integer) line-reminder-saved-lines))
              (forward-line 1)
              (setq current-linum (line-reminder-get-current-line-integer)))
            ))))))

(defun line-reminder-after-change-functions (begin end length)
  "Do stuff after buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes.
LENGTH : deletion length."

  (when (not buffer-read-only)
    (let (;; Get the current total line.
          (tmp-buffer-max-line (line-reminder-total-line))
          (delta-line-count 0)
          ;; Current line is the bound. Is the line after we do
          ;; either addition/subtraction.
          (bound-current-line (line-reminder-get-current-line-integer)))
      ;; If the total line had changed.
      (unless (= tmp-buffer-max-line line-reminder-last-buffer-max-line)
        ;; Get delta line count.
        ;; Delta line = Current total line - Original total line.
        (setq delta-line-count (- tmp-buffer-max-line line-reminder-last-buffer-max-line))

        (setq line-reminder-change-lines
              (line-reminder-delta-list-lines-by-bound line-reminder-change-lines
                                                       bound-current-line
                                                       delta-line-count))
        (setq line-reminder-saved-lines
              (line-reminder-delta-list-lines-by-bound line-reminder-saved-lines
                                                       bound-current-line
                                                       delta-line-count))

        ;; Update the last buffer max line.
        (setq-local line-reminder-last-buffer-max-line tmp-buffer-max-line)))

    (save-excursion
      (goto-char begin)
      (push (line-reminder-get-current-line-integer) line-reminder-change-lines)

      (goto-char end)
      (push (line-reminder-get-current-line-integer) line-reminder-change-lines))

    (delete-dups line-reminder-change-lines)))


(defun line-reminder-enable ()
  "Enable `line-reminder' in current buffer."
  (setq-local linum-format 'line-reminder-linum-format)
  (add-hook 'before-change-functions #'line-reminder-before-change-functions nil t)
  (add-hook 'after-change-functions #'line-reminder-after-change-functions nil t)
  (add-hook 'after-save-hook #'line-reminder-after-save-hook nil t)
  (add-hook 'post-command-hook #'line-reminder-post-command-hook nil t))

(defun line-reminder-disable ()
  "Disable `line-reminder' in current buffer."
  (remove-hook 'before-change-functions #'line-reminder-before-change-functions t)
  (remove-hook 'after-change-functions #'line-reminder-after-change-functions t)
  (remove-hook 'after-save-hook #'line-reminder-after-save-hook t)
  (remove-hook 'post-command-hook #'line-reminder-post-command-hook t)
  (line-reminder-clear-reminder-lines-sign))


(define-minor-mode line-reminder-mode
  "Automatically capitalize booleans"
  :lighter " LR"
  :group line-reminder
  (if line-reminder-mode
      (line-reminder-enable)
    (line-reminder-disable)))

(defun line-reminder-turn-on-line-reminder-mode ()
  "Turn on the 'line-reminder-mode'."
  (line-reminder-mode 1))

(define-globalized-minor-mode global-line-reminder-mode
  line-reminder-mode line-reminder-turn-on-line-reminder-mode
  :require 'line-reminder)


(provide 'line-reminder)
;;; line-reminder.el ends here
