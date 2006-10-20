;;; vlog-align.el --- align verilog codes

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
(require 'align)
(require 'vlog-lib)

;;+ Customizations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup vlog-align nil
  "Customizations for code alignment."
  :group 'verilog)

(defcustom vlog-align-do-align-with-indent t
  "If t, do align with indent."
  :group 'vlog-align
  :type  'boolean)

(defcustom vlog-align-normal-comment-column 48
  "Comment column for `vlog-align-do-inparen-normal'."
  :group 'vlog-align
  :type  'integer)

(defcustom vlog-align-mod-inst-stop-list
  '(24 44)
  "Stop list of module instantiation line alignment.
For example, if you want to align your codes like this:
m_ff_ce #(1,1'b0) u_ff_my (
    .i_clk      (my_clk),       // comments...
                16              32
    .i_reset_b  (my_reste_b),   // comments...
Then you can set me to '(16 32)."
  :group 'vlog-align
  :type  '(repeat integer))

(defcustom vlog-align-declaration-stop-list
  '(10 18 30 32 48)
  "Stop list of declaration line alignment.
For example, if you want to align your codes like this:
reg       [31:0]  my_reg;                       // comments...
wire      [31:0]  my_wire     =   my_reg;       // comments...
parameter [31:0]  my_param    =   para_value;   // comments...
          10      18          30  34            48
Then you can set me to '(10 18 30 34 48)."
  :group 'vlog-align
  :type  '(repeat integer))

(defcustom vlog-align-do-align-for-port-list t
  "If t, do align with for port list."
  :group 'vlog-align
  :type  'boolean)

(defcustom vlog-align-do-align-for-assign t
  "If t, do align with for assign statement."
  :group 'vlog-align
  :type  'boolean)

(defcustom vlog-align-separate-method 'entire
  "The buffer local value for `align-region-separate'.
See `align-region-separate' for more information."
  :group 'vlog-align
  :type  '(choice
           (const    :tag "Entire region is one section" entire)
           (const    :tag "Align by contiguous groups" group)
           (regexp   :tag "Regexp defines section boundaries")
           (function :tag "Function defines section boundaries")))

(defcustom vlog-align-exclude-rules
  '(line-comments)
  "A more friendly list for users to choose alignment exclude rules."
  :group 'vlog-align
  :type  '(set (const :tag "Exclude line comments"
                      line-comments)))

(defcustom vlog-align-rules
  '(declaration sig-assign cont-assign)
  "A more friendly list for users to choose alignment rules."
  :group 'vlog-align
  :type  '(set (const :tag "Declaration:           (reg [7:0] counter;)"
                      declaration)
               (const :tag "Signal assignment:     (foo <= bar;)"
                      sig-assign)
               (const :tag "Continuous assignment: (sssign foo = bar;)"
                      cont-assign)))

;;+ Internal variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vlog-align-exclude-rules-list
  `(;; exclude line comments
    (line-comments
     (regexp . "^\\s-*//.+$")
     (group  . 0)))
  "A list describing texts that should be excluded from alignment.
See the documentation for `align-rules-list' for more info.")

(defvar vlog-align-rules-list
  `(;; reg unsigned [7:0] reg_1
    (declaration
     (regexp . ,(concat "\\<\\(?:" (vlog-regexp-opt vlog-decl-type-words nil)
                        "\\)\\>" "\\(\\s-*\\)"
                        "\\(" vlog-port-decl-2-re "\\s-*\\)*\\(\\s-+\\)"))
     (group . (1 4)))
    ;; foo <= bar
    (sig-assign
     (regexp . "\\S-\\(\\s-*\\)<=\\(\\s-*\\)\\S-")
     (group . (1 2)))
    ;; assign foo = bar
    (cont-assign
     (regexp . "^\\s-*assign\\(\\s-+\\)\\(?:{[^}]+}\\|[^= ]+\\)\\(\\s-*\\)=\\(\\s-*\\)")
     (group . (1 2 3))))
  "A list describing all available alignment rules for `vlog-mode'.
See the documentation for `align-rules-list' for more information.")

;;+ Alignment functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-align-line (&optional inparen)
  "Do align for this line. If INPAREN is set, 'inparen means in
parenthesis, and 'normal means not in parens.  If INPAREN is not
set, I'll figure it out myself."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((orig (point-marker))
          (icol (skip-syntax-forward "\\s-" (line-end-position))))
      (if (if inparen (eq inparen 'inparen) (vlog-in-parens-p))
          ;; inside parenthesis
          (cond
           ;; for port connection line
           ((eq (char-after) ?\.)
            (vlog-align-do-port-conn))
           ;; for declaration
           ((and vlog-align-do-align-for-port-list
                 (looking-at "\\(in[op]\\|outp\\)ut"))
            (vlog-align-do-declaration icol))
           ;; for others
           (t (vlog-align-do-inparen-normal)))
        ;; outside parenthesis
        (vlog-align-do-declaration icol))
      (goto-char (marker-position orig)))))

(defun vlog-align-do-inparen-normal ()
  "Do align for normal lines inside parens."
  (when (not (looking-at "/\\(/\\|\\*\\)"))
    (let ((cip t))
      (when (and (re-search-forward "\\()\\(;\\)?\\)" (line-end-position) t)
                 (not (match-end 2)))
        (setq cip nil))
      (when (and cip
                 (re-search-forward "/\\(/\\|\\*\\)" (line-end-position) t))
        (goto-char (match-beginning 0))
        (vlog-lib-indent-to-column vlog-align-normal-comment-column)))))

(defun vlog-align-do-port-conn ()
  "Do align for port connections."
  (let ((state 'iname))
    (forward-word 1)
    (skip-chars-forward " \t" (line-end-position))
    (while (and (not (eq state 'done))
                (not (eolp)))
      (cond
       ((eq state 'iname)
        (if (not (looking-at "("))
            (setq state 'done)
          (vlog-lib-indent-to-column (car vlog-align-mod-inst-stop-list))
          (if (not (vlog-re-search-forward "[,;]" (line-end-position) t))
              (setq state 'done)
            (skip-chars-forward " \t" (line-end-position))
            (setq state 'comment))))
       ((eq state 'comment)
        (when (looking-at "/\\(/\\|\\*\\)")
          (vlog-lib-indent-to-column (nth 1 vlog-align-mod-inst-stop-list)))
        (setq state 'done))
       (t
        (setq state 'done))))))

(defun vlog-align-do-declaration (&optional icol)
  "Do align for declaration lines.
Optional arg ICOL is the indentation column of current line."
  (setq icol (if (numberp icol) icol 0))
  (let ((state 'init))
    (while (and (not (eq state 'done))
                (not (eolp)))
      (cond
       ((eq state 'init)
        (if (not (looking-at vlog-decl-type-words-re))
            (if (and vlog-align-do-align-for-assign
                     (looking-at "assign\\>"))
                (progn
                  (forward-word 1)
                  (vlog-skip-blank-and-useless-forward (line-end-position))
                  (setq state 'name))
              (setq state 'done))
          (forward-word 1)
          (vlog-skip-blank-and-useless-forward (line-end-position))
            ;; for Verilog 2000, more stuffs show up ...
            (when vlog-mode-v2k-enabled
              (while (looking-at vlog-decl-type-words-re)
                (just-one-space)
                (forward-word 1)
                (vlog-skip-blank-and-useless-forward (line-end-position))))
            (setq state 'bitw)))
       ((eq state 'bitw)
        (if (not (looking-at "\\["))
            (setq state 'name)
          (vlog-lib-indent-to-column
           (+ icol (car vlog-align-declaration-stop-list)))
          (if (vlog-re-search-forward "\\]" (line-end-position) t)
              (progn
                (skip-chars-forward " \t" (line-end-position))
                (setq state 'name))
            (setq state 'done))))
       ((eq state 'name)
        (if (not (looking-at "\\sw"))
            (setq state 'done)
          (vlog-lib-indent-to-column
           (+ icol (nth 1 vlog-align-declaration-stop-list)))
          (if (vlog-re-search-forward "[=;]" (line-end-position) t)
              (if (string= (match-string 0) ";")
                  (progn
                    (skip-chars-forward " \t" (line-end-position))
                    (setq state 'comment))
                (backward-char 1)
                (setq state 'equal))
            (setq state 'done))))
       ((eq state 'equal)
        (vlog-lib-indent-to-column
         (+ icol (nth 2 vlog-align-declaration-stop-list)))
        (forward-char 1)
        (skip-chars-forward " \t" (line-end-position))
        (setq state 'value))
       ((eq state 'value)
        (if (not (looking-at "\\sw"))
            (setq state 'done)
          (vlog-lib-indent-to-column
           (+ icol (nth 3 vlog-align-declaration-stop-list)))
          (if (vlog-re-search-forward ";" (line-end-position) t)
              (progn
                (skip-chars-forward " \t" (line-end-position))
                (setq state 'comment))
            (setq state 'done))))
       ((eq state 'comment)
        (when (looking-at "/\\(/\\|\\*\\)")
          (vlog-lib-indent-to-column
           (+ icol (nth 4 vlog-align-declaration-stop-list))))
        (setq state 'done))
       (t (setq state 'done))))))

;;+ Make vlog-mode work with align.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-align-activate-align-rules ()
  "Filter `vlog-align-rules-list' with `vlog-align-rules' to get
the local value of `align-mode-rules-list'; and filter
`vlog-align-exclude-rules-list' with `vlog-align-exclude-rules'
to get the local value of `align-mode-exclude-rules-list'.  These
two local values will make vlog-mode buffers work with align.el."
  ;; Set rules for alignments
  (setq align-mode-rules-list
        (vlog-lib-filter-assoc vlog-align-rules vlog-align-rules-list)
        align-mode-exclude-rules-list
        (vlog-lib-filter-assoc vlog-align-exclude-rules
                               vlog-align-exclude-rules-list))
  ;; Make separate method buffer local
  (set (make-local-variable 'align-region-separate)
       vlog-align-separate-method)
  ;; Add vlog-mode to various align modes
  (add-to-list 'align-dq-string-modes 'vlog-mode)
  (add-to-list 'align-open-comment-modes 'vlog-mode))

(provide 'vlog-align)

;;; vlog-align.el ends here
