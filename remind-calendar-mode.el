;;; remind-calendar-mode.el --- Major mode for remind files -*- lexical-binding: t -*-

;; Author: Jonathan Neidel <emacs@jneidel.com>
;; Maintainer: Jonathan Neidel <emacs@jneidel.com>
;; URL: https://github.com/jneidel/remind-calendar-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Syntax highlighting for .rem files of the remind calendar system
;; by Dianne Skoll.

;;; Code:
(defvar remind-calendar-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table for `remind-calendar-mode'.")

(defface remind-calendar-mode-tag
  '((t :foreground "dark grey"))
  "Face for the remind TAG keyword.")

(defun remind-calendar-mode--match-color (limit)
  "Match SPECIAL COLOR r g b and apply the actual color as face."
  (when (re-search-forward
         "SPECIAL COLOR \\([0-9]\\{1,3\\}\\) \\([0-9]\\{1,3\\}\\) \\([0-9]\\{1,3\\}\\)"
         limit t)
    (let* ((r (string-to-number (match-string 1)))
           (g (string-to-number (match-string 2)))
           (b (string-to-number (match-string 3)))
           (hex (format "#%02x%02x%02x" r g b))
           (fg (readable-foreground-color hex)))
      (put-text-property (match-beginning 0) (match-end 0)
                         'face `(:background ,hex :foreground ,fg)))
    t))

(defvar remind-calendar-mode-syntax
  '((keywords-list . ("REM" "ONCE" "TODO" "COMPLETE-THROUGH" "SKIP" "BEFORE"
                      "AFTER" "OMIT" "ADDOMIT" "NOQUEUE" "OMITFUNC" "AT"
                      "SCHED" "WARN" "UNTIL" "THROUGH" "SCANFROM" "FROM"
                      "DURATION" "TZ" "INFO" "MSG" "MSF" "RUN" "CAL" "TAG"
                      "PRIORITY" "MAX-OVERDUE" "SATISFY" "SPECIAL"
                      "PS" "PSFILE"
                      "IF" "ENDIF" "RETURN" "EXPR" "INCLUDECMD" "RUN" "BANNER"
                      "PUSH-OMIT-CONTEXT" "CLEAR-OMIT-CONTEXT" "POP-OMIT-CONTEXT"
                      ))
    (keyword . "[A-Z]*")
    (keyword-with-param-n . "\\(PRIORITY\\|MAX-OVERDUE\\|DURATION\\) \\([0-9]+\\)")
    (variable . "$[A-Za-z]+")
    (tag . "TAG \\([^ ]+\\)")
    (one-liners . "^\\(SET\\|UNSET\\|INCLUDE\\|DO\\|SYSINCLUDE\\).+")
    (comment . "^\\([\s\t]+\\)?#.*$")
    (time . "[0-9]\\{1,2\\}:[0-9]\\{2\\}")
    (inline-date . "\\([A-Z][a-z][a-z] [0-9][0-9]? [0-9]\\{4\\}?\\)") ; e.g. Aug 15 2026
    (date-modifiers . " \\(\\*[0-9]+\\)\\|\\(--[0-9]+\\)")
    (start-of-line-date . "^[A-Z]* ?\\([A-Z][a-z][a-z]\\|[^A-Z]\\)+") ; maybe one uppercase word, followed by everything but uppercase except for Sss, e.g. REM Tue Aug 15
    (substitution-filter . "\\%\\([a-z1-9_!?@#:\"]\\|([^)]+)\\|<[^>]+>\\)")
    )
  "List of associations for .rem file syntax.")

(defconst remind-calendar-mode-font-lock-keywords
  (let ((syntax remind-calendar-mode-syntax))
    `((,(alist-get 'one-liners syntax)
       (0 'font-lock-keyword-face))
      (,(alist-get 'start-of-line-date syntax)
       (0 'font-lock-constant-face))
      (,(regexp-opt (alist-get 'keywords-list syntax)
                    'words)
       (1 'font-lock-keyword-face t))
      (,(alist-get 'variable syntax)
       (0 'font-lock-variable-use-face t))
      (,(alist-get 'inline-date syntax)
       (0 'font-lock-constant-face))
      (,(alist-get 'time syntax)
       (0 'font-lock-constant-face))
      (,(alist-get 'keyword-with-param-n syntax)
       (2 'font-lock-number-face))
      (,(alist-get 'date-modifiers syntax)
       (0 'font-lock-doc-face t))
      (,(alist-get 'substitution-filter syntax)
       (0 'font-lock-string-face t))
      (,(alist-get 'tag syntax)
       (1 'remind-calendar-mode-tag))
      ("\\(\\[\\)\\(.*?\\)\\(\\]\\)" ; scripting
       (1 'font-lock-preprocessor-face t) ; opening [
       (3 'font-lock-preprocessor-face t) ; closing ]
       ("\\([A-Za-z]+\\)(" (goto-char (match-beginning 2)) (goto-char (match-end 2))
        (1 'font-lock-function-call-face t))
       ("[0-9]+" (goto-char (match-beginning 2)) (goto-char (match-end 2))
        (0 'font-lock-number-face t))
       ("[-*/&|!<>=+]+" (goto-char (match-beginning 2)) (goto-char (match-end 2))
        (0 'font-lock-operator-face t))
       ("[()]+" (goto-char (match-beginning 2)) (goto-char (match-end 2))
        (0 'font-lock-bracket-face t)))
      (remind-calendar-mode--match-color
       (0 'font-lock-doc-face))
      (,(alist-get 'comment syntax)
       (0 'font-lock-comment-face t t))
      )))


;;;###autoload
(defun remind-calendar()
  "Display terminal version of the remind calendar in color.
Includes the last four week and the next three months.
Centered on today.

Can be inconsistent. You might need to call it multiple times."
  (interactive)
  (let ((buf (get-buffer-create "*remind-calendar*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (call-process-shell-command
       "rem -b1 -cu3 -m -w141 -@2,0 $(date -d '-4 weeks' +%Y-%m-%d ) 2>/dev/null"
       ;; -4 weeks = 1 month into the past, -cu3 = 3 months into the future
       nil buf t)
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf))
  (delete-other-windows)
  (search-forward "******") ;; find today
  (beginning-of-line)
  (recenter-top-bottom 1)) ;; this week at the top of the screen

(use-package ansi-color
  :ensure nil
  :commands (remind-calendar))

;;;###autoload
(define-derived-mode remind-calendar-mode fundamental-mode "rem"
  "Major mode for editing remind calendar files (.rem)."
  :syntax-table remind-calendar-mode-syntax-table
  (setq-local font-lock-defaults '(remind-calendar-mode-font-lock-keywords)
              comment-start "#"
              comment-start-skip (concat (regexp-quote comment-start) "+\\s *")))

(provide 'remind-calendar-mode)
;;; remind-calendar-mode.el ends here
