;;; vlog-mode.el --- a new major mode for editing verilog files

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
(require 'font-lock)

(require 'vlog-lib)
(require 'vlog-indent)
(require 'vlog-skel)
(require 'vlog-auto)
(require 'vlog-signal)

;;+ variables, constants and faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup vlog-mode nil
  "vlog-mode Options."
  :group 'languages)

(defgroup vlog-mode-faces nil
  "vlog-mode faces settings."
  :group 'vlog-mode)

(defvar vlog-mode-map nil
  "Keymap used in vlog-mode.")

(defcustom vlog-mode-make-keymap-hook nil
  "Normal hook that is run after vlog-mode-map is set.
You can add your own keymaps running this hook."
  :type    'hook
  :group   'vlog-mode)

(defcustom vlog-mode-auto-end-block t
  "If t, insert \"end\"/\"join\" after \"begin\"/\"fork\" automatically."
  :type  'toggle
  :group 'vlog-mode)

(defvar vlog-mode-syntax-table nil
  "Syntax table used in `vlog-mode'.")

;; keyword sets and regexps --------------------------------------------------
(defvar vlog-mode-keywordset-types
  '("defparam" "event" "inout" "input" "integer" "output" "parameter" "real"
    "realtime" "reg" "signed" "supply" "supply0" "supply1" "time" "tri"
    "tri0" "tri1" "triand" "trior" "trireg" "vectored" "wand" "wire" "wor")
  "Type keywords in verilog sources.")

(defvar vlog-mode-keywordset-structs
  '("initial" "always" "function" "endfunction"
    "task" "endtask" "module" "endmodule")
  "Structure keywords in verilog sources.")

(defvar vlog-mode-keywordset-keywords
  '("assign" "begin" "case" "casex" "casez" "default" "deassign" "disable"
    "else" "end" "endcase" "endgenerate" "endprimitive" "endspecify"
    "endtable" "for" "force" "forever" "fork" "generate" "if" "join"
    "macromodule" "negedge" "posedge" "or" "primitive" "repeat" "release"
    "specify" "table" "wait" "while")
  "Keywords in verilog sources.")

(defvar vlog-mode-keywordset-pragmas
  '("synopsys")
  "Pragma keywords in verilog sources.")

(defvar vlog-mode-keywordset-docs
  '("Fixme" "FIXME" "FixMe" "fixme"
    "Todo:" "TODO:" "ToDo:" "todo:"
    "Doc:" "DOC:" "doc:")
  "In-comments documentation keywords in verilog sources. Emacs
will highlight them with `vlog-mode-doc-face'.  To add you own,
just add new words to this list, and then don't forget to call
`vlog-mode-make-keywords' to rebuild regexps with the new list.")

(defcustom vlog-mode-keywords-use-generic-systask t
  "If non-nil, use system task keywords in `vlog-mode-keywordset-systasks'.
If nil, use generic system task keywords regexp."
  :group   'vlog-mode
  :type    'boolean)

(defvar vlog-mode-keywordset-systasks
  '("$bitstoreal" "$close" "$display" "$display" "$displayb" "$displayb"
    "$displayh" "$displayh" "$displayo" "$displayo" "$dist_chi_square"
    "$dist_erland" "$dist_exponential" "$dist_normal" "$dist_t"
    "$dist_uniform" "$dust_poisson" "$finish" "$hold" "$itor" "$monitor"
    "$monitorb" "$monitorh" "$monitoro" "$nochange" "$open" "$period"
    "$printtimescale" "$random" "$readmemb" "$readmemh" "$realtime"
    "$realtobits" "$recovery" "$rtoi" "$setup" "$setuphold" "$skew" "$stime"
    "$stop" "$strobe" "$strobeb" "$strobeh" "$strobeo" "$time" "$timeformat"
    "$width" "$write" "$write" "$writeb" "$writeb" "$writeh" "$writeh"
    "$writeo" "$writeo")
  "System task names.")

(defvar vlog-mode-keywordset-types-regexp nil
  "Regexp of type keywords in verilog sources.")

(defvar vlog-mode-keywordset-structs-regexp nil
  "Regexp of structure keywords in verilog sources.")

(defvar vlog-mode-keywordset-keywords-regexp nil
  "Regexp of keywords in verilog sources.")

(defvar vlog-mode-keywordset-pragmas-regexp nil
  "Regexp of pragma keywords in verilog sources")

(defvar vlog-mode-keywordset-docs-regexp nil
  "Regexp of documentation keywords in verilog sources")

(defvar vlog-mode-keywordset-systasks-regexp nil
  "Regexp of system task name.")

(defvar vlog-mode-keywordset-systasks-regexp-generic
  "\\$[a-zA-Z][a-zA-Z0-9_]*"
  "Generic system task name regexp.")

(defvar vlog-mode-keywordset-number-regexp
  "\\<\\([\\-]?[0-9]+\\('[hdxboHDXBO][0-9a-fA-FxXzZ_]+\\)*\\)\
\\|\\([0-9.]+[eE]-?[0-9]+\\)\\>"
  "Regexp of number keywords in verilog sources.")

(defvar vlog-mode-keywordset-operator-regexp
  "[+-*/!~&|\\^<>=?:]"
  "Regexp of Operator keyword in verilog sources.")

(defvar vlog-mode-keywords-basic nil
  "Level 0 (basic) to highlight in vlog-mode.")

(defvar vlog-mode-keywords-medium nil
  "Level 1 (medium) keywords to highlight in vlog-mode.")

(defvar vlog-mode-keywords-max nil
  "Level 2 (maximum) keywords to highlight in vlog-mode.")

;; faces ---------------------------------------------------------------------
(defface vlog-mode-psl-tag-face
  '((((class color)
      (background dark))
     (:foreground "salmon"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t (:italis t)))
  "Font lock mode face used to highlight psl tag."
  :group 'vlog-mode-faces)

(defface vlog-mode-psl-content-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "pink3"))
    (t (:italis t)))
  "Font lock mode face used to highlight psl contents."
  :group 'vlog-mode-faces)

(defcustom vlog-mode-psl-tag-face 'vlog-mode-psl-tag-face
  "PSL tag face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-psl-content-face 'vlog-mode-psl-content-face
  "PSL content face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-type-face 'font-lock-type-face
  "Type face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-keyword-face 'font-lock-keyword-face
  "Keyword face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-systask-face 'font-lock-function-name-face
  "System task face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-struct-face 'font-lock-function-name-face
  "Structure face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-operator-face 'font-lock-variable-name-face
  "Operator face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-module-face 'font-lock-constant-face
  "Module, task, function and primitive face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-pragma-face
  (if (boundp 'font-lock-preprocessor-face)
      'font-lock-preprocessor-face
    'font-lock-constant-face)
  "Pragma face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-macro-face 'font-lock-constant-face
  "Macro face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-number-face 'font-lock-doc-face
  "Number face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-parameter-face 'font-lock-type-face
  "Parameter passing face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-doc-face 'font-lock-warning-face
  "Documentation face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

(defcustom vlog-mode-warning-face 'font-lock-warning-face
  "Warning face used in vlog-mode."
  :group 'vlog-mode-faces
  :type  'variable)

;; options -------------------------------------------------------------------
(defcustom vlog-mode-auto-indent t
  "Toggle auto indentation when a new line is started."
  :group 'vlog-mode
  :type  'toggle)

(defcustom vlog-mode-auto-delete-empty-line t
  "Toggle auto deletion of empty lines when hit return."
  :group 'vlog-mode
  :type  'toggle)

(defcustom vlog-mode-auto-name-at-endmodule t
  "Toggle auto insertion of module name when feed the endmodule line."
  :group 'vlog-mode
  :type  'toggle)

(defcustom vlog-mode-endmodule-auto-name-prefix " // "
  "The string added before the module name after endmodule."
  :group 'vlog-mode
  :type  'string)
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;+ vlog-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode ()
  "Major mode for editing Verilog code."
  (interactive)
  ;;
  ;; major mode settings
  (kill-all-local-variables)
  (setq major-mode 'vlog-mode)
  (setq mode-name "vlog")
  (vlog-mode-make-keymap)
  (use-local-map vlog-mode-map)
  ;;
  ;; set syntax table
  (make-local-variable 'vlog-mode-syntax-table)
  (unless (syntax-table-p vlog-mode-syntax-table)
    (setq vlog-mode-syntax-table (make-syntax-table))
    (vlog-mode-make-sytax-table vlog-mode-syntax-table))
  (set-syntax-table vlog-mode-syntax-table)
  ;;
  ;; set font-lock keywords
  (unless (and (stringp vlog-mode-keywordset-types-regexp)
               (stringp vlog-mode-keywordset-structs-regexp)
               (stringp vlog-mode-keywordset-keywords-regexp)
               (stringp vlog-mode-keywordset-pragmas-regexp)
               (stringp vlog-mode-keywordset-docs-regexp)
               (stringp vlog-mode-keywordset-systasks-regexp))
    (vlog-mode-make-keywords))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        (list
         '(vlog-mode-keywords-basic vlog-mode-keywords-medium vlog-mode-keywords-max)
         nil nil nil nil))
  ;;
  ;; indentation
  (unless (and (stringp vlog-indent-directives)
               (stringp vlog-indent-paren-sexp-signs)
               (stringp vlog-indent-paren-cond-signs)
               (stringp vlog-indent-special-beg-daily-words)
               (stringp vlog-indent-special-beg-scarce-words)
               (stringp vlog-indent-special-end-daily-words)
               (stringp vlog-indent-special-end-scarce-words)
               (stringp vlog-indent-beh-words)
               (stringp vlog-indent-block-beg-words)
               (stringp vlog-indent-defun-words)
               (stringp vlog-indent-words)
               (stringp vlog-indent-calc-begs))
    (vlog-indent-make-regexps))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'vlog-indent-line)
  ;;
  ;; other settings
  (unless (stringp vlog-decl-type-words-re)
    (vlog-lib-make-regexp))
  (setq comment-start "//"))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ syntax and keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode-make-sytax-table (table)
  "Make syntax table for vlog-mode."
  (modify-syntax-entry ?\\ "\\"     table)
  (modify-syntax-entry ?+  "."      table)
  (modify-syntax-entry ?-  "."      table)
  (modify-syntax-entry ?=  "."      table)
  (modify-syntax-entry ?%  "."      table)
  (modify-syntax-entry ?<  "."      table)
  (modify-syntax-entry ?>  "."      table)
  (modify-syntax-entry ?&  "."      table)
  (modify-syntax-entry ?|  "."      table)
  (modify-syntax-entry ?`  "w"      table)
  (modify-syntax-entry ?_  "w"      table)
  (modify-syntax-entry ?#  "w"      table)  ;; add `#' for delay syntax.
  (modify-syntax-entry ?\' "."      table)
  (modify-syntax-entry ?/  ". 124b" table)
  (modify-syntax-entry ?*  ". 23"   table)
  (modify-syntax-entry ?\n "> b"    table))

(defun vlog-mode-make-keymap ()
  "Make `vlog-mode-map' and run `vlog-mode-make-keymap-hook'."
  (unless (keymapp vlog-mode-map)
    (setq vlog-mode-map (make-sparse-keymap))
    (vlog-skel-setup-keymap)
    (define-key vlog-mode-map "\C-c\C-c" vlog-skel-map)
    (define-key vlog-mode-map "\C-m"     'vlog-mode-electric-return)
    ;; for debug use
    (define-key vlog-mode-map "\C-c\C-s"
      (lambda () (interactive)
        (message (format "%s" (vlog-indent-figure-out)))))
    (define-key vlog-mode-map "\C-c\C-i"
      (lambda () (interactive) (vlog-indent-line t)))
    (define-key vlog-mode-map "\C-c\C-a" 'vlog-auto-sense)
    (define-key vlog-mode-map "\C-c\C-u" 'vlog-auto-sense-update-this-block)
    (define-key vlog-mode-map "\C-c\C-d" 'vlog-signal-trace-driver)
    (define-key vlog-mode-map "\C-c\C-n" 'vlog-signal-trace-driver-next)
    (define-key vlog-mode-map "\C-c "    'vlog-align-line)
    (define-key vlog-mode-map "\C-c\C-h" 'vlog-skel-smart-header)
    (define-key vlog-mode-map "d"        'vlog-mode-electric-d)
    (define-key vlog-mode-map "e"        'vlog-mode-electric-e)
    (define-key vlog-mode-map "f"        'vlog-mode-electric-f)
    (define-key vlog-mode-map "k"        'vlog-mode-electric-k)
    (define-key vlog-mode-map "n"        'vlog-mode-electric-n)
    (define-key vlog-mode-map "b"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "b")))
    (define-key vlog-mode-map "B"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "B")))
    (define-key vlog-mode-map "h"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "h")))
    (define-key vlog-mode-map "H"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "H")))
    (define-key vlog-mode-map "D"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "D")))
    (define-key vlog-mode-map "x"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "x")))
    (define-key vlog-mode-map "X"        (lambda () (interactive)
                                           (vlog-mode-electric-bitwidth "X")))
    (define-key vlog-mode-map ";"        'vlog-mode-electric-semi)
    (define-key vlog-mode-map " "        'vlog-mode-electric-space)
    (define-key vlog-mode-map ","        'vlog-mode-electric-comma)
    (define-key vlog-mode-map "\M-s"     'vlog-signal-smart-insert)
    (define-key vlog-mode-map [C-backspace]  'vlog-lib-hungry-back-delete)
    (define-key vlog-mode-map [f1]       'vlog-show-this-signal-width-echo))
  (run-hooks vlog-mode-make-keymap-hook))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ font-lock highlighting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode-make-keywords ()
  "Make keywords of different font-lock decoration levels,
using `vlog-mode-keywordset-types', `vlog-mode-keywordset-structs',
`vlog-mode-keywordset-keywords' and `vlog-mode-keywordset-pragmas'.
Refer to `font-lock-maximum-decoration' for more infomation.

To override or modify default keywords, set `vlog-mode-keywordset-*' and then
call me."
  ;;
  ;; make keyword-regexps first
  (dolist (regexp
           '((vlog-mode-keywordset-types . vlog-mode-keywordset-types-regexp)
             (vlog-mode-keywordset-structs . vlog-mode-keywordset-structs-regexp)
             (vlog-mode-keywordset-keywords . vlog-mode-keywordset-keywords-regexp)
             (vlog-mode-keywordset-pragmas . vlog-mode-keywordset-pragmas-regexp)
             (vlog-mode-keywordset-docs . vlog-mode-keywordset-docs-regexp)
             (vlog-mode-keywordset-systasks . vlog-mode-keywordset-systasks-regexp)))
    (set (cdr regexp)
         (vlog-regexp-opt (symbol-value (car regexp)) nil)))
  ;;
  ;; set basic keywords
  (setq vlog-mode-keywords-basic
        (list
         ;; keywords
         (cons (vlog-regexp-wrap vlog-mode-keywordset-keywords-regexp)
               vlog-mode-keyword-face)
         ;; types
         (cons (vlog-regexp-wrap vlog-mode-keywordset-types-regexp)
               vlog-mode-type-face)
         ;; system tasks
         (cons (vlog-regexp-wrap
                (if vlog-mode-keywords-use-generic-systask
                    vlog-mode-keywordset-systasks-regexp-generic
                  (concat "\\$\\(" vlog-mode-keywordset-systasks-regexp "\\)")))
               vlog-mode-systask-face)
         ;; structures
         (cons (vlog-regexp-wrap vlog-mode-keywordset-structs-regexp)
               vlog-mode-struct-face)))
  ;;
  ;; set medium keywords
  (setq vlog-mode-keywords-medium
        (append vlog-mode-keywords-basic
                (list
                 ;; operators
                 (list vlog-mode-keywordset-operator-regexp
                       (list 0 vlog-mode-operator-face))
                 ;; module/task/primitive definitions
                 (list "\\<\\(\\(macro\\)?module\\|primitive\\|task\\)\\>\\s-*\\(\\sw+\\)"
                       (list 1 vlog-mode-keyword-face)
                       (list 3 vlog-mode-module-face))
                 ;; function definitions
                 (list "\\<function\\>\\s-+\\(integer\\|real\\(time\\)?\\|time\\)\\s-+\\(\\sw+\\)"
                       (list 1 vlog-mode-keyword-face)
                       (list 3 vlog-mode-module-face t))
                 (list "\\<function\\>\\s-+\\(\\[[^]]+\\]\\)\\s-+\\(\\sw+\\)"
                       (list 1 vlog-mode-keyword-face)
                       (list 2 vlog-mode-module-face 'append))
                 (list "\\<function\\>\\s-+\\(\\sw+\\)"
                       (list 1 vlog-mode-module-face 'append))
                 ;; autos
                 (list vlog-lib-auto-keyword-re
                       (list 1 vlog-mode-doc-face t)
                       (list 2 vlog-mode-module-face t)))))
  ;;
  ;; set maximum keywords
  (setq vlog-mode-keywords-max
        (append vlog-mode-keywords-medium
                (list
                 ;; pragmas
                 (list (concat "//\\s-*\\(" vlog-mode-keywordset-pragmas-regexp
                               "\\)\\>\\s-*\\([^/\n]?+\\)\\(//.*\\)*$")
                       (list 1 vlog-mode-pragma-face t)
                       (list 2 vlog-mode-type-face t))
                 ;; macro definitions/uses
                 (list "`\\s-*[A-Za-z][A-Za-z0-9_]*"
                       (list 0 vlog-mode-macro-face))
                 ;; behavioural
                 (list "@"
                       (list 0 vlog-mode-operator-face))
                 ;; numbers
                 (list vlog-mode-keywordset-number-regexp
                       (list 0 vlog-mode-number-face))
                 ;; identifiers
                 (list "\\(begin\\|fork\\)\\s-*:\\s-*\\(\\sw+\\)"
                       (list 2 vlog-mode-macro-face 'append))
                 ;; delays
                 (list "#\\s-*[0-9]+"
                       (list 0 vlog-mode-parameter-face 'append))
                 ;; parameter passing
                 (list "#\\s-*([^)]+)"
                       (list 0 vlog-mode-parameter-face 'append))
                 ;; instantiation
                 (list "\\(\\.\\sw+\\)[ \t\n]*(\\([^()]+\\))"
                       (list 1 vlog-mode-type-face 'append)
                       (list 2 vlog-mode-operator-face 'append))
                 (list "\\(\\.\\sw+\\)[ \t\n]*\\((\\s-*)\\)"
                       (list 1 vlog-mode-warning-face 'append)
                       (list 2 vlog-mode-warning-face 'append))
                 (list "\\(\\sw+\\)[ \t\n]*\\(#\\s-*([^()]+)\\)*[ \t\n]*\\<\\(\\sw+\\)\\>[ \t\n]*("
                       (list 1 vlog-mode-struct-face 'append)
                       (list 3 vlog-mode-module-face 'append))
                 ;; inline PSL assertions
                 (list "^[ \t]*//[ \t]*\\(psl\\|PSL\\)\\(.*\\)$"
                       (list 1 vlog-mode-psl-tag-face t)
                       (list 2 vlog-mode-psl-content-face t))
                 ;; documents
                 (list (concat "//\\s-*\\(" vlog-mode-keywordset-docs-regexp "\\)")
                       (list 1 vlog-mode-doc-face t))))))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ vlog-mode electric keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode-electric-return ()
  "Start a new line. if `vlog-mode-auto-indent' is t, do indentation also.  If
`vlog-mode-auto-name-at-endmodule' is t, add module name after endmodule
automatically, with prefix `vlog-mode-endmodule-auto-name-prefix'."
  (interactive)
  (let ((block-end nil)
        (mod-name  nil)
        (icol      nil))
    (if (and vlog-mode-auto-delete-empty-line
             (looking-back "^\\s-+"))
        (delete-horizontal-space)
      (vlog-align-line))
    (cond
     ((looking-back "\\<begin\\s-*")
      (setq block-end "end"))
     ((looking-back "\\<fork\\s-*")
      (setq block-end "join")))
    (when block-end
      (setq icol (vlog-indent-level-at-pos)))
    ;; insert module name if looking at endmodule
    (when (and vlog-mode-auto-name-at-endmodule
               (looking-back "\\<endmodule\\s-*")
               (setq mod-name (vlog-lib-get-module-name)))
      (delete-horizontal-space)
      (insert (concat vlog-mode-endmodule-auto-name-prefix mod-name)))
    (insert "\n")
    (if (not (and vlog-mode-auto-end-block
                  (memq last-command '(vlog-mode-electric-n
                                       vlog-mode-electric-k))
                  icol))
        (when vlog-mode-auto-indent
          (funcall indent-line-function))
      (insert "\n")
      (vlog-lib-indent-to-column icol)
      (insert block-end)
      (forward-line -1)
      (funcall indent-line-function))))

(defun vlog-mode-electric-d ()
  "Insert `d', and then do something else."
  (interactive)
  (if (looking-back "\\<en")
      (progn (insert "d")
             (vlog-indent-line))
    (vlog-mode-electric-bitwidth "d")))

(defun vlog-mode-electric-e ()
  "Insert `e', and then do something else."
  (interactive)
  (insert "e")
  (when (looking-back "\\<\\(else\\)\\|\\(case\\)")
    (vlog-indent-line)))

(defun vlog-mode-electric-f ()
  "Insert `f', and then do something else."
  (interactive)
  (insert "f")
  (when (looking-back "\\<if")
    (vlog-indent-line)))

(defun vlog-mode-electric-k ()
  "Insert `k', and then do something else."
  (interactive)
  (insert "k")
  (when (looking-back "\\<fork")
    (vlog-indent-line)))

(defun vlog-mode-electric-n ()
  "Insert `n', and then do something else."
  (interactive)
  (insert "n")
  (when (looking-back "\\<\\(beg\\|jo\\)in")
    (vlog-indent-line)))

(defun vlog-mode-electric-bitwidth (c)
  "Convert \"3b\" to \"3'b\" automatically."
  (interactive)
  (insert c)
  (backward-char 2)
  (if (looking-at "[0-9]")
      (progn
        (forward-char 1)
        (insert "'")
        (forward-char 1))
    (forward-char 2)))

(defun vlog-mode-electric-space ()
  "Expand \"reg 2\" into \"reg [1:0]\" automatically, works with
defined parameters."
  (interactive)
  (if (looking-back (concat "\\(" vlog-decl-type-words-re "\\)"
                            "\\s-+\\([0-9a-zA-Z_]+\\)"))
      (let ((beg    (match-beginning 2))
            (end    (match-end 2))
            (str    (match-string-no-properties 2))
            (width  nil)
            (wstr   ""))
        (when (string-match "^[0-9]+$" str)
          (setq width (string-to-int str)))
        (setq wstr (if width
                       (if (> width 1) (int-to-string (1- width)) nil)
                     (if (vlog-lib-get-module-para-val str)
                         (concat str "-1")
                       "")))
        (if (and wstr (string= wstr ""))
            (insert " ")
          (kill-region beg end)
          (vlog-lib-indent-to-column (car vlog-align-declaration-stop-list))
          (if wstr (insert "[" wstr ":0" "]"))
          (delete-horizontal-space)
          (vlog-lib-indent-to-column (nth 1 vlog-align-declaration-stop-list))
          (vlog-align-line)))
    (insert " ")))

(defun vlog-mode-electric-semi ()
  "Insert `;', and then do something else."
  (interactive)
  (insert ";")
  (vlog-align-line))

(defun vlog-mode-electric-comma ()
  "Insert `,', and then do something else."
  (interactive)
  (if (/= ?\, (preceding-char))
      (insert ",")
    (backward-delete-char 1)
    (insert "<=")))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vlog-mode)

;;; vlog-mode.el ends here
