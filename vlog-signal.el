;;; vlog-signal.el --- handal signals in verilog files

;; Copyright (C)  2004 Sun Yijiang <sunyijiang@gmail.com>

;; Author:     Sun Yijiang
;; Maintainer: Sun Yijiang
;; Inherited:  From verilog-mode.el (Michael McNamara <mac@verilog.com>)
;; Created:    Dec. 2004
;; Keywords:   languages, verilog

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:


;;; Code:

(require 'cl)

(defvar vlog-signal-trace-regexp nil
  "DO NOT touch me.")
(make-variable-buffer-local 'vlog-signal-trace-regexp)
;;+ signal width detection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-show-this-signal-width-echo ()
  "Show signal width in echo area."
  (interactive)
  (vlog-signal-show-width nil))

(defun vlog-show-this-signal-width-tooltip ()
  "Show signal width with tooltip."
  (interactive)
  (vlog-signal-show-width t))

(defun vlog-signal-show-width (tooltip)
  "Show signal width in echo area if TOOLTIP is nil, or show it with tooltip."
  (let (sig result wid msg len delay)
    (save-excursion
      (vlog-lib-select-this-word)
      (setq sig (buffer-substring-no-properties (point) (mark))))
    (when sig
      (cond
       ;; signal
       ((setq result (vlog-signal-get-signal-width sig))
        (let ((type (car result))
              (msb  (nth 1 result))
              (lsb  (nth 2 result))
              (rwid (nth 3 result)))
          (if rwid
              (setq wid rwid)
            (setq wid 0))
          (setq msg
                (concat
                 (propertize (concat "[Signal width] ") 'face 'font-lock-function-name-face)
                 (propertize (concat "\"" sig "\": ") 'face font-lock-variable-name-face)
                 (propertize
                  (if (stringp wid) wid
                    (if (= 0 wid)
                        "N/A "
                      (concat (int-to-string wid) " ")))
                  'face font-lock-warning-face)))
          (setq len (string-width msg))
          (while (< len 36)
            (setq msg (concat msg " "))
            (setq len (+ len 1)))
          (setq msg
                (concat msg
                        (propertize
                         (concat (concat "\t\t// " type)
                                 (if (and msb lsb)
                                     (concat " [" msb ":" lsb "]")
                                   ""))
                         'face font-lock-comment-face)))
          (if tooltip
              (progn
                (setq delay tooltip-delay)
                (setq tooltip-delay 0)
                (tooltip-show
                 (concat
                  "\"" sig "\" width:  "
                  (if (= 0 wid)
                      "N/A "
                    (concat (int-to-string wid) " "))))
                (setq tooltip-delay delay))
            (message "%s" msg))))
       ;; macro
       ((setq result (vlog-signal-get-macro-value sig))
        (message (concat
                  (propertize (concat "[Macro definition] ") 'face 'font-lock-function-name-face)
                  (propertize (concat "\"" sig "\": \t") 'face font-lock-variable-name-face)
                  (propertize result 'face font-lock-warninmatch-beg))))
       ;; parameter
       ((setq result (vlog-lib-get-module-para-val sig))
        (message (concat
                  (propertize (concat "[Parameter definition] ") 'face 'font-lock-function-name-face)
                  (propertize (concat "\"" sig "\": \t") 'face font-lock-variable-name-face)
                  (propertize result 'face font-lock-warning-face))))
       (t (if tooltip
              (tooltip-show "Signal not found!")
            (message "Signal not found!")))))))

(defun vlog-signal-get-signal-width (sig)
  "Return signal SIG's width and declaration."
  (save-excursion
    (let (result type msb lsb wid para)
      (unless (vlog-re-search-backward
               "\\<\\(macro\\)*module[ \t\n]+\\sw+" (point-min) t)
        (beginning-of-buffer))
      (if (re-search-forward
           (concat "\\(in\\(?:[op]ut\\)\\|output\\|reg\\|tri\\|wire\\)"
                   "\\s-*\\(\\[\\s-*\\(.+\\)\\s-*:\\s-*\\([^ \t]+\\)\\s-*+\\]\\)*\\s-*"
                   sig "[^a-zA-Z0-9_]")
           (point-max) t)
          (progn
            (setq type (match-string-no-properties 1))
            (setq msb  (match-string-no-properties 3))
            (setq lsb  (match-string-no-properties 4))
            (if (match-string-no-properties 2)
                (if (and (string-match "^[0-9]+$" msb)
                         (string-match "^[0-9]+$" lsb))
                    (setq wid (+ 1 (abs (- (string-to-int msb) (string-to-int lsb)))))
                  (if (and (string-match "^\\([0-9a-zA-Z_]+\\)-1$" msb)
                           (progn (setq para (match-string 1 msb))
                                  (if (vlog-lib-get-module-para-val para) para nil))
                           (string= "0" lsb))
                      (setq wid para)
                    (setq wid 0)))
              (setq wid 1))
            (setq result (list type msb lsb wid)))
        (setq result nil))
      result)))

(defun vlog-signal-get-macro-value (sign)
  "Return sign SIGN's value."
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward
         (concat "`define\\s-+" sig "\\s-+\\([^ \t\n]+\\)") (point-max) t)
        (match-string-no-properties 1)
      nil)))
;;- signal width detection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vlog-signal-smart-insert ()
  "Insert the target signal and corresponding assignment.
This command is useful because in most cases only one signal
is concerned within an always block."
  (interactive)
  (let ((beg (point))
        (end (point))
        (sig nil))
    (if (vlog-re-search-backward "always" (point-min) t)
        (progn
          (if (vlog-re-search-forward
               "\\s-*\\(\\([0-9a-zA-Z_]+\\)\\|\\({.+}\\)\\)\\s-*\\(<*=\\)[^=]" end t)
              (setq sig (match-string 1))
            (setq sig (read-string "No signal found, please enter the signal name: ")))
          (goto-char beg)
          (insert sig " " (match-string 4) " ")
          (vlog-indent-line))
      (message "You should use this command when you are inside an always block."))))

(defun vlog-signal-trace-driver ()
  "Trace signal driver in current buffer."
  (interactive)
  (let ((sig         "")
        (sig-regexp  "")
        (sig-regexp2 ""))
    (vlog-lib-mark-signal-nomark)
    (setq sig (buffer-substring-no-properties (mark) (point)))
    (setq sig-regexp
          (concat "\\<" sig "\\(\\[.*\\]\\)*\\s-*<*=\\s-*[^=]"))
    (setq sig-regexp2
          (concat "([ \t]*" sig "[ \t]*)"))
    (setq vlog-signal-trace-regexp sig-regexp)
    (goto-char (point-min))
    (when (or (vlog-re-search-forward sig-regexp (point-max) t)
              (vlog-re-search-forward sig-regexp2 (point-max) t))
      (goto-char (match-beginning 0)))))

(defun vlog-signal-trace-driver-next ()
  "Trace signal driver in current buffer, goto next."
  (interactive)
  (if (save-excursion
        (forward-word 1)
        (vlog-re-search-forward vlog-signal-trace-regexp (point-max) t))
      (goto-char (match-beginning 0))
    (message "No more signal driver for signal %s." vlog-signal-trace-regexp)))

;;+ imenu support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (regexp-opt '("input" "output" "inout" "reg" "wire") t)
(defvar vlog-signal-decl-re "\\<\\(\\(?:in\\|out\\)put\\|reg\\|wire\\|inout\\)\\>"
  "Regexp for signal declaration.")

;; (regexp-opt '("reg" "wire" "signed" "unsigned") t)
(defvar vlog-signal-decl-2-re "\\[.+\\]\\|\\(reg\\|wire\\|signed\\|unsigned\\)\\>"
  "Regexp for signal declaration.")

(defun vlog-imenu-create-index-function ()
  "Imenu support function for verilog.   This function is set as the value of
`imenu-create-index-function'."
  (save-excursion
  (beginning-of-buffer)
  (let (entries bound)
    (while (vlog-re-search-forward vlog-signal-decl-re (point-max) t)
      (setq bound (line-end-position))
      (vlog-skip-blank-and-useless-forward bound nil vlog-signal-decl-2-re)
        (if (vlog-in-parens-p)
            ;; within module port list
            (when (looking-at "\\s-*\\([A-Za-z][A-Za-z0-9_]*\\)")
              (add-to-list
               'entries
               (cons (match-string-no-properties 1) (point-marker))))
          ;; not within module port list
          (while (vlog-re-search-forward
                  "\\s-*,?\\s-*\\([A-Za-z][A-Za-z0-9_]*\\)" bound t)
            (add-to-list
             'entries
             (cons (match-string-no-properties 1) (point-marker))))))
    entries)))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vlog-signal)

;;; vlog-signal.el ends here
