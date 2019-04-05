;;; line-reminder.el --- Line annotation similar to Visual Studio.        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-05-25 15:10:29

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Line annotation similar to Visual Studio.
;; Keyword: annotation linum reminder
;; Version: 0.0.8
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
;; Line annotation similar to Visual Studio.
;;

;;; Code:

(require 'cl-lib)
(require 'linum)


(defgroup line-reminder nil
  "Visual Studio like line annotation in Emacs."
  :prefix "line-reminder-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/line-reminder"))


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

(defcustom line-reminder-ignore-buffer-names '("*Backtrace*"
                                               "*Minibuf-0*"
                                               "*Minibuf-1*"
                                               "*Buffer List*"
                                               "*Checkdoc Status*"
                                               "*Echo Area 0*"
                                               "*helm"
                                               "*Help*"
                                               "magit"
                                               "*Packages*"
                                               "*run*"
                                               "*shell*"
                                               "*undo-tree*")
  "Buffer Name list you want to ignore this mode."
  :group 'line-reminder
  :type 'list)

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defvar-local line-reminder-change-lines '()
  "List of line that change in current temp buffer.")

(defvar-local line-reminder-saved-lines '()
  "List of line that saved in current temp buffer.")

(defvar-local line-reminder-delta-line-count -1
  "Delta line count for `before-change-functions' and `after-change-functions'.")

(defvar-local line-reminder-do-saving nil
  "Is current buffer doing the saving hook/command?")

(defvar-local line-reminder-max-last-linum -1
  "Current buffer maxinum/last line number.")

(defvar-local line-reminder-record-max-last-linum -1
  "Record down the `line-reminder-max-last-linum' after the change hook runs.")

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

(defsubst line-reminder-get-current-line-string ()
  "Get the current line as string."
  (format-mode-line "%l"))

(defun line-reminder-total-line ()
  "Return current buffer's maxinum line."
  (line-number-at-pos (point-max)))

(defun line-reminder-is-contain-list-string (in-list in-str)
  "Check if a string contain in any string in the string list.
IN-LIST : list of string use to check if IN-STR in contain one of
the string.
IN-STR : string using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun line-reminder-get-current-line-integer ()
  "Get the current line as integer."
  (string-to-number (line-reminder-get-current-line-string)))

(defsubst line-reminder-linum-format-string-align-right ()
  "Return format string align on the right."
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (format "%%%dd" w)))

(defsubst line-reminder-get-propertized-normal-sign (ln)
  "Return a default propertized normal sign.
LN : pass in by `linum-format' variable."
  (propertize (format (concat line-reminder-linum-left-string
                              (line-reminder-linum-format-string-align-right)
                              line-reminder-linum-right-string)
                      ln)
              'face 'linum))

(defsubst line-reminder-get-propertized-modified-sign ()
  "Return a propertized modifoied sign."
  (propertize line-reminder-modified-sign 'face 'line-reminder-modified-sign-face))

(defsubst line-reminder-get-propertized-saved-sign ()
  "Return a propertized saved sign."
  (propertize line-reminder-saved-sign 'face 'line-reminder-saved-sign-face))


(defun line-reminder-propertized-sign-by-type (type &optional ln)
  "Return a propertized sign string by type.
TYPE : type of the propertize sign you want.
LN : Pass is line number for normal sign."
  (cl-case type
    ('normal (if (not ln)
                 (error "Normal line but with no line number pass in")
               ;; Just return normal linum format.
               (line-reminder-get-propertized-normal-sign ln)))
    ('modified (line-reminder-get-propertized-modified-sign))
    ('saved (line-reminder-get-propertized-saved-sign))))


(defun line-reminder-is-contain-list-integer (in-list in-int)
  "Check if a integer contain in any string in the string list.
IN-LIST : list of integer use to check if IN-INT in contain one of the integer.
IN-INT : integer using to check if is contain one of the IN-LIST."
  (cl-some #'(lambda (lb-sub-int) (= lb-sub-int in-int)) in-list))


(defun line-reminder-linum-format (ln)
  "Core line reminder format string logic here.
LN : pass in by `linum-format' variable."
  (let ((reminder-sign "")
        (result-sign "")
        (normal-sign (line-reminder-propertized-sign-by-type 'normal ln))
        (is-sign-exists nil))

    (cond (;; NOTE(jenchieh): Check if change lines list.
           (line-reminder-is-contain-list-integer line-reminder-change-lines
                                                  ln)
           (progn
             (setq reminder-sign (line-reminder-propertized-sign-by-type 'modified))
             (setq is-sign-exists t)))
          (;; NOTE(jenchieh): Check if saved lines list.
           (line-reminder-is-contain-list-integer line-reminder-saved-lines
                                                  ln)
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

(defun line-reminder-is-valid-line-reminder-situation (&optional begin end)
  "Check if is valid to apply line reminder at the moment.
BEGIN : start changing point.
END : end changing point."
  (if (and begin
           end)
      (and (not buffer-read-only)
           (not (line-reminder-is-contain-list-string line-reminder-ignore-buffer-names
                                                      (buffer-name)))
           (<= begin (point-max))
           (<= end (point-max)))
    (and (not buffer-read-only)
         (not (line-reminder-is-contain-list-string line-reminder-ignore-buffer-names
                                                    (buffer-name))))))

(defun line-reminder-delta-list-lines-by-bound (in-list bound delta)
  "Delta the count of line in the list by bound.
IN-LIST : input line list.
BOUND : everything above is affective by delta.
DELTA : addition/subtraction value of the line count."
  (let ((index 0))
    (dolist (tmp-linum in-list)
      (if (< delta 0)
          ;; Is negative. (Deleting delta of lines)
          (when (< bound tmp-linum)
            (setf (nth index in-list) (+ tmp-linum delta)))
        ;; Is positive. (Adding delta of lines)
        (when (<= bound tmp-linum)
          (setf (nth index in-list) (+ tmp-linum delta))))
      (setq index (1+ index))))
  in-list)

(defun line-reminder-remove-lines-out-range (in-list)
  "Remove all the line in the list that are above the last/maxinum line \
or less than zero line in current buffer.
IN-LIST : list to be remove or take effect with."
  ;; Remove line that are above last/max line in buffer.
  (let ((last-line-in-buffer (line-reminder-total-line)))
    (dolist (line in-list)
      ;; If is larger than last/max line in buffer.
      (when (or (< last-line-in-buffer line)
                (<= line 0))
        ;; Remove line because we are deleting.
        (setq in-list (remove line in-list)))))
  in-list)


(defun line-reminder-post-command-hook ()
  "Do stuff when post command."
  (when (line-reminder-is-valid-line-reminder-situation)
    (unless (buffer-modified-p)
      ;; Transfer the `change-lines' to `saved-lines'.
      (setq-local line-reminder-saved-lines
                  (append line-reminder-saved-lines line-reminder-change-lines))
      ;; Clear the change lines.
      (setq-local line-reminder-change-lines '())

      ;; Removed save duplicates
      (delete-dups line-reminder-saved-lines))))

(defun line-reminder-after-save-hook ()
  "Do stuff after save hook."
  ;; Transfer the `change-lines' to `saved-lines'.
  (setq-local line-reminder-saved-lines
              (append line-reminder-saved-lines line-reminder-change-lines))
  ;; Clear the change lines.
  (setq-local line-reminder-change-lines '())

  ;; Set saving trigger.
  (setq-local line-reminder-do-saving t)

  ;; Removed save duplicates
  (delete-dups line-reminder-saved-lines))

(defun line-reminder-before-change-functions (begin end)
  "Do stuff before buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes."

  (when (line-reminder-is-valid-line-reminder-situation begin end)
    (save-excursion
      (let ((begin-linum -1)
            (end-linum -1))
        (setq end-linum (line-number-at-pos end))
        (setq begin-linum (line-number-at-pos begin))
        (goto-char begin)

        ;; Something we need to know before we run `after-change-functions' hook.
        (setq-local line-reminder-delta-line-count (- begin-linum end-linum))

        ;; Get the maxinum/last line number in the current buffer before
        ;; the changes happens.
        (setq-local line-reminder-max-last-linum (line-reminder-total-line))

        ;; Update the record the fist time if it value is still default -1.
        (when (and (= line-reminder-record-max-last-linum -1)
                   (buffer-modified-p))
          (setq-local line-reminder-record-max-last-linum line-reminder-max-last-linum))

        ;; `begin-linum' and `end-linum' the same meaning there is no line
        ;; inserted or removed.
        (unless (= begin-linum end-linum)
          (let ((current-linum begin-linum)
                ;; Record down the last current line number, to make
                ;; sure that we don't fall into infinite loop.
                (record-last-linum begin-linum)
                (reach-last-line-in-buffer nil))
            (while (and (<= current-linum end-linum)
                        ;; Cannot be the same as last line in buffer.
                        (not reach-last-line-in-buffer))

              ;; Remove line because we are deleting.
              (setq-local line-reminder-change-lines
                          (remove current-linum line-reminder-change-lines))
              (setq-local line-reminder-saved-lines
                          (remove current-linum line-reminder-saved-lines))

              ;; To do the next line.
              (forward-line 1)
              (setq current-linum (line-reminder-get-current-line-integer))

              ;; NOTE(jenchieh): Check if we need to terminate this loop?
              (when (or
                     ;; Check if still the same as last line.
                     (= current-linum record-last-linum)
                     ;; Check if current linum last line in buffer
                     (= current-linum (line-reminder-total-line)))
                (setq reach-last-line-in-buffer t))

              ;; Update the last linum, make sure it won't do the same
              ;; line twice.
              (setq record-last-linum current-linum)))

          ;; Remove line that are above last/max line or below than zero line
          ;; in buffer.
          (setq-local line-reminder-change-lines
                      (line-reminder-remove-lines-out-range line-reminder-change-lines))
          (setq-local line-reminder-saved-lines
                      (line-reminder-remove-lines-out-range line-reminder-saved-lines)))
        (delete-dups line-reminder-change-lines)))))

(defun line-reminder-after-change-functions (begin end length)
  "Do stuff after buffer is changed.
BEGIN : beginning of the changes.
END : end of the changes.
LENGTH : deletion length."

  (when (line-reminder-is-valid-line-reminder-situation begin end)
    (save-excursion
      ;; When begin and end are not the same, meaning the there
      ;; is addition/deletion happening in the current buffer.
      (let ((begin-linum -1)
            (end-linum -1)
            (delta-line-count 0)
            ;; Current line is the bound. Is the line after we do
            ;; either addition/subtraction.
            (bound-current-line -1)
            ;; Is deleting line or adding new line?
            (is-deleting-line t))

        ;; Is deleting line can be depends on the length.
        (when (and (= length 0)
                   (not (= begin end)))
          (setq is-deleting-line nil))
        (setq end-linum (line-number-at-pos end))
        (setq begin-linum (line-number-at-pos begin))
        (goto-char begin)

        (if (or (not (= begin-linum end-linum))
                (not (= line-reminder-delta-line-count 0)))
            (progn
              ;; Get delta line count. (Either adding/deleting lines)
              (if is-deleting-line
                  ;; We use local variable `line-reminder-delta-line-count' here,
                  ;; because after deleting we do not know how many lines are
                  ;; deleted. Only way to find out is to get the information from
                  ;; `before-change-functions' hook and use it here.
                  (setq delta-line-count line-reminder-delta-line-count)
                ;; Notice, we cannot get the deletion lines infromation from here.
                ;; Fortunately, adding lines/newline can be calculated here by just
                ;; Minusing `end-linum' and `begin-linum'.
                (setq delta-line-count (- end-linum begin-linum)))

              ;; Bound is also the beginning of the line number.
              (setq bound-current-line begin-linum)

              ;; Add up delta line count to `change-lines' list.
              (setq line-reminder-change-lines
                    (line-reminder-delta-list-lines-by-bound line-reminder-change-lines
                                                             bound-current-line
                                                             delta-line-count))
              ;; Add up delta line count to `saved-lines' list.
              (setq line-reminder-saved-lines
                    (line-reminder-delta-list-lines-by-bound line-reminder-saved-lines
                                                             bound-current-line
                                                             delta-line-count))

              (if is-deleting-line
                  (progn
                    ;; NOTE(jenchieh): Preventing mouse clicking will trigger
                    ;; last or the second last linumber will be added to changes
                    ;; line. Is pretty weird though.
                    (when (not (= line-reminder-max-last-linum line-reminder-record-max-last-linum))
                      ;; Deleting line. (After deleting line/lines, we just need
                      ;; to push the current line to `line-reminder-change-lines',
                      ;; which is also the beginning of line.)
                      (push begin-linum line-reminder-change-lines)))
                (progn
                  ;; Adding line. (After adding line/lines, we just need to loop
                  ;; throught those lines and add it to `line-reminder-change-lines'
                  ;; list.)
                  (let ((current-linum begin-linum)
                        ;; Record down the last current line number, to make
                        ;; sure that we don't fall into infinite loop.
                        (record-last-linum begin-linum)
                        (reach-last-line-in-buffer nil))
                    (while (and (<= current-linum end-linum)
                                ;; Cannot be the same as last line in buffer.
                                (not reach-last-line-in-buffer))

                      (when (not (= line-reminder-max-last-linum line-reminder-record-max-last-linum))
                        ;; Push the current line to changes-line.
                        (push current-linum line-reminder-change-lines))

                      ;; To do the next line.
                      (forward-line 1)
                      (setq current-linum (line-reminder-get-current-line-integer))

                      ;; NOTE(jenchieh): Check if we need to terminate this loop?
                      (when (or
                             ;; Check if still the same as last line.
                             (= current-linum record-last-linum)
                             ;; Check if current linum last line in buffer
                             (= current-linum (line-reminder-total-line)))
                        (setq reach-last-line-in-buffer t))

                      ;; Update the last linum, make sure it won't do the same
                      ;; line twice.
                      (setq record-last-linum current-linum)))))

              ;; Update the maxinum/last line number in current buffer
              ;; after changes.
              (setq-local line-reminder-record-max-last-linum line-reminder-max-last-linum))
          (progn
            ;; NOTE(jenchieh): For some reason, while saving this will be
            ;; trigger and add line that not suppose to be added. In order
            ;; to avoid this we add this `unless' statement.
            (when (or (= begin end)
                      (not is-deleting-line))
              ;; Just add the current line.
              (push begin-linum line-reminder-change-lines))))
        (delete-dups line-reminder-change-lines)))))


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


;;;###autoload
(define-minor-mode line-reminder-mode
  "Minor mode 'line-reminder-mode'."
  :lighter " LR"
  :group line-reminder
  (if line-reminder-mode
      (line-reminder-enable)
    (line-reminder-disable)))

(defun line-reminder-turn-on-line-reminder-mode ()
  "Turn on the 'line-reminder-mode'."
  (line-reminder-mode 1))

;;;###autoload
(define-globalized-minor-mode global-line-reminder-mode
  line-reminder-mode line-reminder-turn-on-line-reminder-mode
  :require 'line-reminder)


(provide 'line-reminder)
;;; line-reminder.el ends here
