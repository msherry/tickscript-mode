;;; tickscript-mode.el --- A major mode for Tickscript files

;; Copyright (C) 2017  Marc Sherry
;; Homepage: https://github.com/msherry/tickscript-mode
;; Version: 0.1
;; Author: Marc Sherry <msherry@gmail.com>
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Copyright Marc Sherry <msherry@gmail.com>
;;
;; This package provides a major mode for working with TICKscript, a DSL for
;; use with Kapacitor and InfluxDB.

;;; Code:

(defvar tickscript-font-lock-keywords nil)
(defvar tickscript-properties nil)
(defvar tickscript-toplevel-nodes nil)
(defvar tickscript-nodes nil)
(defvar tickscript-chaining-methods nil)


(defgroup tickscript nil
  "TICKscript support for Emacs."
  :group 'languages
  :version "0.1")

(defcustom tickscript-indent-offset 4
  "Number of spaces per indentation level."
  :type 'integer
  :group 'tickscript
  :safe 'integerp)

(defcustom tickscript-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `tickscript-indent-line' call."
  :type '(repeat symbol)
  :group 'tickscript)

(defcustom tickscript-max-block-lookback 5000
  "When indenting, don't look back more than this many characters.

This should be eliminated, I'm just copying from julia-mode for now."
  :type 'integer
  :group 'tickscript
  :safe 'integerp)

(setq tickscript-properties
      '("align" "alignGroup" "buffer" "byMeasurement" "cluster" "create" "cron"
        "database" "every" "fill" "flushInterval" "groupBy" "groupByMeasurement"
        "measurement" "offset" "period" "precision" "retentionPolicy" "tag"
        "writeConsistency"))

(setq tickscript-toplevel-nodes
      '("batch" "stream"))

(setq tickscript-nodes
      '("alert" "batch" "combine" "default" "delete" "derivative"
        "eval" "flatten" "from" "groupBy" "httpOut" "httpPost" "influxDBOut"
        "influxQL" "join" "k8sAutoscale" "kapacitorLoopback" "log" "noOp"
        "query" "sample" "shift" "stateCount" "stateDuration" "stats" "stream" "uDF"
        "union" "where" "window"))

(setq tickscript-chaining-methods
      '("alert" "bottom" "combine" "count" "cumulativeSum" "deadman" "default"
        "delete" "derivative" "difference" "distinct" "elapsed" "eval" "exclude"
        "first" "flatten" "groupBy" "holtWinters" "holtWintersWithFit" "httpOut"
        "httpPost" "influxDBOut" "join" "kapacitorLoopback" "last" "log" "max"
        "mean" "median" "min" "mode" "movingAverage" "percentile" "sample"
        "shift" "spread" "stateCount" "stateDuration" "stats" "stddev" "sum"
        "top" "union" "where" "window"))

(setq tickscript-font-lock-keywords
      `(,
        ;; General keywords
        (rx symbol-start (or "var") symbol-end)
        (,(concat "\\_<" (regexp-opt tickscript-properties t) "\\_>") . font-lock-keyword-face)
        (,(concat "\\_<" (regexp-opt tickscript-nodes t) "\\_>") . font-lock-type-face)
        (,(concat "\\_<" (regexp-opt tickscript-chaining-methods t) "\\_>") . font-lock-function-name-face)
        (,(concat "\\_<" (regexp-opt tickscript-properties t) "\\_>") . font-lock-keyword-face)
        ;; Time units
        (,(rx symbol-start (1+ digit) (or "u" "µ" "ms" "s" "m" "h" "d" "w") symbol-end) . font-lock-constant-face)
        ;; Variable declarations
        ("\\_<\\(?:var\\)\\_>[[:space:]]+\\([[:alpha:]]\\(?:[[:alnum:]]\\|_\\)*\\)" (1 font-lock-variable-name-face nil nil))))

(defconst tickscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a dereferencing string delimiter
    (modify-syntax-entry ?\" "$" table)
    ;; | is punctuation?
    (modify-syntax-entry ?| "." table)
    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
     ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar tickscript-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Indentation
    (define-key map (kbd "<backtab>") 'tickscript-indent-dedent-line)
    ;; Util
    (define-key map "\C-c\C-c" 'tickscript-check)
    map)
  "Keymap for `tickscript-mode'.")

;; if backward-sexp gives an error, move back 1 char to move over the '('
(defun tickscript-safe-backward-sexp ()
  "Move backward by one sexp, ignoring errors."
  (if (condition-case nil (backward-sexp) (error t))
      (ignore-errors (backward-char))))

(defun tickscript-at-keyword (kw-list)
  "Return the word at point if it matches any keyword in KW-LIST.

KW-LIST is a list of strings."
  ;; It is assumed that this is always called at the beginning of a word --
  ;; either after backward-sexp or forward-to-indentation.
  (save-excursion
    (and (member (current-word t) kw-list)
         (not (looking-at "("))
         (not (or (tickscript-in-comment)
                  (tickscript-in-string))))))

(defun tickscript-at-node (&optional toplevel-only)
  "Return the word at point if it is a node.

To be a node, it must be a keyword in the nodes list, and either
be preceded by the \"|\" sigil, or no sigil.  Specifically, it
must not be preceded by \".\", as some keywords (like \"groupBy\"
are both properties and nodes."
  ;; Skip over any sigil, if present
  (save-excursion
    (when (looking-at "|")
      (forward-char))
    (and (or (= (point) 1)
             (equal (char-before (point)) ?|)
             (not (equal (char-before (point)) ?.)))
         (tickscript-at-keyword (if toplevel-only
                                    tickscript-toplevel-nodes
                                  tickscript-nodes)))))

(defun tickscript-at-property ()
  "Return the word at point if it is a property.

To be a property, it must be a keyword in the properties list, and
be preceded by the \".\" sigil."
  (and (> (point) 1)
       (equal (char-before (point)) ?.)
       (tickscript-at-keyword tickscript-properties)))

(defun tickscript-last-node-pos (&optional min)
  "Return the position of the last node, if found.
Do not move back beyond position MIN."
  (unless min
    (setq min 0))
  (save-excursion
    (let ((count 0))
      (while (not (or (> count 0) (<= (point) min)))
        (tickscript-safe-backward-sexp)
        (setq count
              (cond ((tickscript-at-node)
                     (+ count 1))
                    (t count))))
      (if (> count 0)
          (point)
        nil))))

(defun tickscript-node-indentation (&optional min)
  "Return indentation level for items under the last node.
Do not move back beyond MIN."
  ;; Ensure MIN is not before start of buffer
  (unless min
    (setq min 0))
  (save-excursion
    (setq min (max min (point-min)))
    (let ((pos (tickscript-last-node-pos min)))
      (when pos
        (goto-char pos)
        (+ tickscript-indent-offset (current-indentation))))))

(defun tickscript-in-string ()
  "Return non-nil if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun tickscript-in-comment ()
  "Return non-nil if point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun tickscript-indent-in-string ()
  "Indentation inside strings with newlines is \"manual\",
meaning always increase indent on TAB and decrease on S-TAB."
  ;; Taken from julia-mode.el
  (save-excursion
    (beginning-of-line)
    (when (tickscript-in-string)
      (if (member this-command tickscript-indent-trigger-commands)
          (+ tickscript-indent-offset (current-indentation))
        ;; return the current indentation to prevent other functions from
        ;; indenting inside strings
        (current-indentation)))))

(defun tickscript-indent-toplevel-node ()
  "Indentation for toplevel nodes, which are always at level 0.

 \"batch\" or \"stream\", with optional \"var\" declarations."
  (save-excursion
    (beginning-of-line)
    (forward-to-indentation 0)
    (and (or (looking-at "var")
             (tickscript-at-node t))
         0)))

(defun tickscript-indent-dedent-line ()
  "Deindent by `tickscript-indent-offset' regardless of current
indentation context."
  (interactive)
  (indent-line-to (max 0 (- (current-indentation) tickscript-indent-offset))))

(defun tickscript-indent-line ()
  "Indent current line of TICKscript code."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation))))
    (indent-line-to
     (or
      ;; Within a string
      (tickscript-indent-in-string)
      ;; Top-level node w/optional var declaration
      (tickscript-indent-toplevel-node)
      ;; General case
      (progn          ;save-excursion
        (beginning-of-line)
        ;; jump up out of any comments
        (let ((state (syntax-ppss)))
          (when (nth 4 state)
            (goto-char (nth 8 state))))
        (forward-to-indentation 0)
        (let ((proptok (tickscript-at-property))
              (nodetok (tickscript-at-node))
              (node-indent (tickscript-node-indentation
                            (- (point) tickscript-max-block-lookback))))
          (message "%s %s %s" proptok nodetok node-indent)
          (max 0 (+ (or node-indent 0)
                    (cond (proptok tickscript-indent-offset)
                          ;; Only dedent this as a node if we're on a node and the previous
                          ;; node is not a toplevel
                          ((and nodetok
                                (> node-indent tickscript-indent-offset))
                           (- tickscript-indent-offset))
                          (t 0))))))))
    ;; We've indented and point is now at the beginning of indentation. Restore
    ;; it to its original position relative to the start of indentation.
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))


;;;###autoload
(define-derived-mode tickscript-mode prog-mode "Tickscript"
  "Major mode for editing TICKscript files

\\{tickscript-mode-map}"
  :syntax-table tickscript-mode-syntax-table

  (set (make-local-variable 'font-lock-defaults) '(tickscript-font-lock-keywords))

  (set (make-local-variable 'comment-start) "// ")

  (set (make-local-variable 'indent-line-function) 'tickscript-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tick\\'" . tickscript-mode))

(provide 'tickscript-mode)
;;; tickscript-mode.el ends here
