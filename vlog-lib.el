;;; vlog-lib.el --- library functions for common use

;; Copyright (C)  2004 Sun Yijiang <sunyijiang@gmail.com>

;; Author:     Sun Yijiang
;; Maintainer: Sun Yijiang
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

;;+ constants & variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst verilog-keywords
 '("`define" "`else" "`endif" "`ifdef" "`include" "`timescale"
   "`undef" "always" "and" "assign" "begin" "buf" "bufif0" "bufif1"
   "case" "casex" "casez" "cmos" "default" "defparam" "disable" "else" "end"
   "endcase" "endfunction" "endgenerate" "endmodule" "endprimitive"
   "endspecify" "endtable" "endtask" "event" "for" "force" "forever"
   "fork" "function" "generate" "if" "initial" "inout" "input" "integer"
   "join" "macromodule" "makefile" "module" "nand" "negedge" "nmos" "nor"
   "not" "notif0" "notif1" "or" "output" "parameter" "pmos" "posedge"
   "primitive" "pulldown" "pullup" "rcmos" "real" "realtime" "reg"
   "repeat" "rnmos" "rpmos" "rtran" "rtranif0" "rtranif1" "signed"
   "specify" "supply" "supply0" "supply1" "table" "task" "time" "tran"
   "tranif0" "tranif1" "tri" "tri0" "tri1" "triand" "trior" "trireg"
   "vectored" "wait" "wand" "while" "wire" "wor" "xnor" "xor" )
 "List of Verilog keywords.")

(defconst vlog-mode-xemacs-p (string-match "XEmacs" emacs-version))

(defvar vlog-decl-type-words
  '("input" "inout" "output" "reg" "wire"
    "parameter" "defparam" "event" "integer")
  "Keywords for port/net/identifiers declararion.  The full list is large,
but for performance reason only a few of them are encounted by default.
You can add more and then call `vlog-lib-make-regexp'.")
(defvar vlog-decl-type-words-re nil
  "Regexp made of `vlog-decl-type-words'.")

(defvar vlog-lib-auto-keyword-re
  "^//\\s-+\\(auto \\(\\sw+\\)\\)\\s-+//\\s-*$")
;;- constants & variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ regexp related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile
  (defun vlog-regexp-opt (a b)
    "Wrapper to deal with XEmacs and FSF emacs having similar functions
with differing number of arguments."
    (if (string-match "XEmacs" emacs-version)
        (regexp-opt a b 't)
      (regexp-opt a b))))

(defun vlog-regexp-opt (a b)
  "Deal with XEmacs & FSF both have `regexp-opt'; with different interface.
Call 'regexp-opt' on A and B."
  (if vlog-mode-xemacs-p
      (unwind-protect (regexp-opt a b 't))
    (regexp-opt a b)))

(defun vlog-regexp-wrap (str)
  (if (stringp str)
      (concat "\\<\\(" str "\\)\\>")
    ""))
;;- regexp related ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit)
    "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying how far back the
match can start."
    (not (null
          (save-excursion
            (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t))))))
;;- compatibility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ search without strings and comments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst vlog-re-search-forward (regexp bound noerror)
  "Like `re-search-forward', but skips over comments or strings."
  (let ((orig (point)))
    (store-match-data '(nil nil))
    (while (and (re-search-forward regexp bound noerror)
                (and (vlog-jump-out-useless-forward)
                     (progn
                       (store-match-data '(nil nil))
                       (if bound (< (point) bound) t)))))
    (or (match-end 0)
        (progn (goto-char orig) nil))))

(defsubst vlog-re-search-backward (regexp bound noerror)
  "Like `re-search-backward', but skips over comments or strings."
  (let ((orig (point)))
    (store-match-data '(nil nil))
    (while (and (re-search-backward regexp bound noerror)
                (and (vlog-jump-out-useless-backward)
                     (progn
                       (store-match-data '(nil nil))
                       (if bound (> (point) bound) t)))))
    (or (match-beginning 0)
        (progn (goto-char orig) nil))))

(defun vlog-jump-out-useless-forward ()
  "Skip strings and comments forward. Return t if inside them."
  (let ((state (parse-partial-sexp (point-min) (point))))
    (cond
     ((nth 3 state)         ;;Inside string
      (search-forward "\"")
      t)
     ((nth 7 state)         ;;Inside // comment
      (forward-line 1)
      t)
     ((nth 4 state)         ;;Inside any comment (hence /**/)
      (search-forward "*/"))
     (t
      nil))))

(defun vlog-jump-out-useless-backward ()
  "Skip strings and comments backward. Return t if inside them."
  (let ((state (parse-partial-sexp (point-min) (point))))
    (cond
     ((nth 3 state)             ;;Inside string
      (search-backward "\"")
      t)
     ((nth 7 state)             ;;Inside // comment
      (goto-char (line-beginning-position))
      (when (search-forward "//")
        (backward-char 2))
      t)
     ((nth 4 state)             ;;Inside /* */ comment
      (search-backward "/*")
      t)
     (t
      nil))))

(defun vlog-skip-blank-and-useless-backward (&optional limit)
  "Skip blanks, strings, comments and directives backward.
If LIMIT is non-nil, use it as limit."
  (let ((lim (if (numberp limit) limit (point-min))))
    (catch 'done
      (while t
        ;; boyond the limit, finish
        (when (<= (point) lim)
          (goto-char lim)
          (throw 'done 'beyond))
        (vlog-jump-out-useless-backward)
        (cond
         ;; after blanks, skip them
         ((looking-back "[ \t\n]")
          (skip-chars-backward " \t\n" lim))
         ;; in directive lines, skip this line
         ((save-excursion
            (beginning-of-line)
            (looking-at vlog-indent-directives-re))
          (if (= 0 (forward-line -1))
              (end-of-line)
            (beginning-of-line)))
         ;; after start comments
         ((looking-back "\\*/")
          (unless (search-backward "/*" limit t)
            (throw 'done 'err)))
         (t
          (throw 'done t)))))))

(defun vlog-skip-blank-and-useless-forward (&optional limit skip-delay)
  "Skip blanks, strings, comments and directives forward.
If 1st argument LIMIT is non-nil, use it as limit.
If 2nd argument SKIP-DELAY is non-nil. skip time delays."
  (let ((lim (if (numberp limit) limit (point-max)))
        orig end)
    (catch 'done
      (while t
        ;; boyond the limit, finish
        (when (>= (point) lim)
          (goto-char lim)
          (throw 'done 'beyond))
        (cond
         ;; skip blanks
         ((looking-at "[ \t\n]")
          (skip-chars-forward " \t\n" lim))
         ;; skip line comments
         ((looking-at "//")
          (forward-line 1))
         ;; skip star comments
         ((looking-at "/\\*")
          (unless (search-forward "*/" limit t)
            (goto-char lim)
            (throw 'done 'beyond)))
         ;; skip directive lines
         ((looking-at vlog-indent-directives-re)
          (forward-line 1))
         ;; skip time delays
         ((and skip-delay
               (looking-at "#"))
          (setq orig (point))
          (forward-char 1)
          (if (looking-at "`?[0-9a-zA-Z_]+")
              ;; sth. like #100
              (goto-char (match-end 0))
            ;; sth. like #(1, 5)
            (if (and (looking-at "(")
                     (/= (point)
                         (save-excursion
                           (condition-case nil (forward-list) (error nil))
                           (setq end (point)))))
                (goto-char end)
              (goto-char orig)
              (throw 'done nil))))
         ;; non-blank & non-comment reached, finish
         (t
          (throw 'done t)))))))
;;- search without strings and comments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ syntax parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-in-comments-p (&optional point)
  "Return t if point POINT (if nil, use current point) is in comments."
  (let* ((pt  (or point (point)))
         (syn (save-excursion (parse-partial-sexp (point-min) pt))))
    (and (numberp pt) (<= pt (point-max))
         (or (nth 4 syn) (nth 7 syn)))))

(defun vlog-in-star-comments-p (&optional point)
  "Return t if point POINT (if nil, use current point) is in star comments."
  (let* ((pt  (or point (point)))
         syn)
    (when (and (numberp pt) (<= pt (point-max)))
      (setq syn (save-excursion (parse-partial-sexp (point-min) pt)))
      (and (nth 4 syn) (not (nth 7 syn))))))

(defun vlog-in-line-comments-p (&optional point)
  "Return t if point POINT (if nil, use current point) is in line comments."
  (let ((pt (or point (point))))
    (and (numberp pt) (<= pt (point-max))
         (nth 7 (save-excursion (parse-partial-sexp (point-min) pt))))))

(defun vlog-in-parens-p (&optional point)
  "Return t if point POINT (if nil, use current point) is in parens."
  (let ((pt (or point (point))))
    (and (numberp pt) (<= pt (point-max))
         (/= 0 (car (save-excursion (parse-partial-sexp (point-min) pt)))))))
;;- syntax parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vlog-lib-select-this-word ()
  "Select the word I am looking at"
  (interactive)
  (forward-char 1)
  (condition-case nil
      (backward-sexp 1) (error ""))
  (when (looking-at "[^a-zA-Z0-9_-]")
    (skip-chars-forward "'\"`*"))
  (condition-case nil
      (mark-sexp) (error "")))

(defun vlog-lib-hungry-back-delete ()
  "Delete continuous whitespaces backwardly."
  (interactive)
  (let ((end (point)))
    (if (= 0 (skip-chars-backward " \t\n"))
        (unless (bobp)
          (backward-delete-char-untabify 1))
      (kill-region (point) end))))

(defun vlog-lib-make-regexp ()
  "Make regexps for vlog-lib."
  (dolist (regexp
           '((vlog-decl-type-words . vlog-decl-type-words-re)))
    (set (cdr regexp)
         (concat "\\<\\(?:"
                 (vlog-regexp-opt (symbol-value (car regexp)) nil) "\\)\\>"))))

(defun vlog-lib-indent-to-column (col)
  "Force Indent to column."
  (let ((ccol (current-column)))
    (when (and (numberp col) (/= col ccol))
      (when (< col ccol) (just-one-space))
      (indent-to-column col))))

(defun vlog-lib-mark-signal-nomark ()
  "Mark the signal current cursor sits on."
  (interactive)
  (forward-char 1)
  (backward-word 1)
  (mark-sexp)
  (exchange-point-and-mark)
  (setq mark-active nil))

(defun vlog-lib-get-current-file-name ()
  "Get current file name."
  (let ((name (buffer-file-name)))
    (if (stringp name)
        (file-name-nondirectory name)
      "")))

(defun vlog-lib-get-current-module-name ()
  "Get current module name."
  (save-excursion
    (goto-char (point-min))
    (if (vlog-re-search-forward "module[ \t\n]\\(\\sw+\\)" (point-max) t)
        (match-string-no-properties 1)
      nil)))

(defun vlog-lib-move-to-column (col)
  "Move to column COL. Return non-nil if done, or nil if failed.
If COL is inside tabs, return 'itab."
  (if (not (numberp col)) nil
    (if (> col (save-excursion (end-of-line) (current-column)))
        nil
      (move-to-column col)
      (if (= (current-column) col) t 'itab))))

(defun vlog-lib-str-memq (str slist)
  "If STR is member of string list SLIST, return t, or return nil."
  (catch 'done
    (dolist (mem slist)
      (when (string= str mem)
        (throw 'done t)))
    (throw 'done nil)))

(defun vlog-lib-get-module-parameters ()
  "Get current module's parameters.
Return the paramter alist: '((name .  value) (name . value) ...)"
  (save-excursion
    (let ((beg (point-min))
          (end (point-max))
          (ret nil)
          begt endt)
      (goto-char beg)
      (when (vlog-re-search-forward
             "\\<\\(macro\\)*module[ \t\n]+\\sw+" (point-max) t)
        (setq beg (match-end 0))
        (when (vlog-re-search-forward "\\<endmodule\\>" (point-max) t)
          (setq end (match-beginning 0))))
      (goto-char beg)
      (while (vlog-re-search-forward
              "parameter[ \t\n]*\\(\\[[^]]+\\]\\)*" end t)
        (setq begt (match-end 0))
        (when (setq endt (vlog-re-search-forward ";" end t))
          (goto-char begt)
          (while (vlog-re-search-forward
                  "\\(\\sw+\\)[ \t\n]*\\(=[ \t\n]*\\(.+?\\)\\)[ \t\n]*[,;]" endt t)
            (add-to-list 'ret (cons
                               (match-string-no-properties 1)
                               (match-string-no-properties 3)) t))))
      ret)))

(defun vlog-lib-get-module-para-val (pname)
  "Return parameter PNAME's value, if not defined, return nil."
  (cdr (assoc pname (vlog-lib-get-module-parameters))))

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return line number at position POS."
    (let* ((pt   (if pos pos (point)))
           (lnum (count-lines (point-min) pt)))
      (when (= (line-beginning-position) pt)
        (setq lnum (1+ lnum)))
      lnum)))

(defun vlog-lib-get-module-name ()
  "Return module name, nil if not found."
  (save-excursion
    (if (and
         (vlog-re-search-backward "\\<\\(macro\\)*module\\>" (point-min) t)
         (forward-word 1)
         (vlog-re-search-forward "\\<\\(\\sw+\\)\\>" (point-max) t))
        (match-string-no-properties 1)
      nil)))

(provide 'vlog-lib)

;;; vlog-lib.el ends here
