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
;; This package provides a major mode for working with TICKscript
;; (https://docs.influxdata.com/kapacitor/v1.3/tick/), a DSL for use with
;; Kapacitor and InfluxDB.
;;
;; Usage:
;;
;; Add the following to your .init.el:
;;
;;     (add-to-list 'load-path "path-to-tickscript-mode")
;;     (require 'tickscript-mode)

;;; Code:

(defvar tickscript-font-lock-keywords nil)
(defvar tickscript-properties nil)
(defvar tickscript-toplevel-nodes nil)
(defvar tickscript-nodes nil)
(defvar tickscript-chaining-methods nil)
(defvar tickscript-series-name nil)
(defvar tickscript-series-type nil)
(defvar tickscript-series-dbrp nil)

(defgroup tickscript nil
  "TICKscript support for Emacs."
  :group 'languages
  :version "0.1")

(defcustom tickscript-indent-offset 4
  "Number of spaces per indentation level."
  :type 'integer
  :group 'tickscript
  :safe 'integerp)

(defcustom tickscript-kapacitor-prog-name "kapacitor"
  "The name of the executable used to invoke Kapacitor."
  :type 'string
  :group 'tickscript
  :safe 'stringp)

(defcustom tickscript-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `tickscript-indent-line' call."
  :type '(repeat symbol)
  :group 'tickscript)

(defcustom tickscript-max-block-lookback 5000
  "When indenting, don't look back more than this many characters."

  ;; TODO: This should be eliminated, I'm just copying from julia-mode for now.
  :type 'integer
  :group 'tickscript
  :safe 'integerp)

(defface tickscript-property
  '((t :inherit font-lock-keyword-face))
  "Face for properties in TICKscript, like align, groupBy, period, etc."
  :tag "tickscript-property"
  :group 'tickscript)

(defface tickscript-node
  '((t :inherit font-lock-type-face))
  "Face for nodes in TICKscript, like alert, batch, query, groupBy, etc."
  :tag "tickscript-node"
  :group 'tickscript)

(defface tickscript-variable
  '((t :inherit font-lock-variable-name-face))
  "Face for variables in TICKscript."
  :tag "tickscript-variable"
  :group 'tickscript)

(defface tickscript-time
  '((t :inherit font-lock-constant-face))
  "Face for time ranges in TICKscript, like 1h, 20us, etc.."
  :tag "tickscript-time"
  :group 'tickscript)

(defface tickscript-operator
  '((t :inherit font-lock-warning-face
     :foreground "#bf3d5e"))
  "Face used for highlighting operators like \"|\" and \"/\" in TICKscript."
  :tag "tickscript-operator"
  :group 'tickscript)


(setq tickscript-properties
      '("align" "alignGroup" "as" "buffer" "byMeasurement" "cluster" "create"
        "cron" "database" "every" "fill" "flushInterval" "groupBy"
        "groupByMeasurement" "keep" "measurement" "offset" "period" "precision"
        "quiet" "retentionPolicy" "tag" "tags" "writeConsistency"))

(setq tickscript-toplevel-nodes
      '("batch" "stream"))

(setq tickscript-nodes
      '("alert" "batch" "bottom" "combine" "count" "cumulativeSum" "deadman"
        "default" "delete" "derivative" "difference" "distinct" "elapsed"
        "eval" "exclude" "first" "flatten" "from" "groupBy" "holtWinters"
        "holtWintersWithFit" "httpOut" "httpPost" "influxDBOut" "join"
        "kapacitorLoopback" "last" "log" "max" "mean" "median" "min" "mode"
        "movingAverage" "percentile" "query" "sample" "shift" "spread"
        "stateCount" "stateDuration" "stats" "stddev" "stream" "sum" "top"
        "union" "where" "window"))

(setq tickscript-font-lock-keywords
      `(,
        ;; General keywords
        (rx symbol-start (or "var") symbol-end)
        ;; Node properties
        (,(concat "\\_<" (regexp-opt tickscript-properties t) "\\_>") . 'tickscript-property)
        ;; Nodes
        (,(concat "\\_<" (regexp-opt tickscript-nodes t) "\\_>") . 'tickscript-node)
        ;; Time units
        (,(rx symbol-start (* "-") (1+ digit) (or "u" "µ" "ms" "s" "m" "h" "d" "w") symbol-end) . 'tickscript-time)
        ;; Operators
        (,(rx (or "/" "\|")) . 'tickscript-operator)
        ;; Variable declarations
        ("\\_<\\(?:var\\)\\_>[[:space:]]+\\([[:alpha:]]\\(?:[[:alnum:]]\\|_\\)*\\)" (1 'tickscript-variable nil nil))))

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

(defvar tickscript-list-commands-map
  (let ((map (define-prefix-command 'tickscript-list-commands-map)))
    (define-key map (kbd "t") #'tickscript-list-tasks)
    (define-key map (kbd "r") #'tickscript-list-recordings)
    (define-key map (kbd "p") #'tickscript-list-replays))
  map)

(defvar tickscript-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Indentation
    (define-key map (kbd "<backtab>") #'tickscript-indent-dedent-line)
    ;; Movement
    (define-key map (kbd "<M-down>") #'tickscript-move-line-or-region-down)
    (define-key map (kbd "<M-up>") #'tickscript-move-line-or-region-up)
    ;; Util
    (define-key map (kbd "C-c C-c") #'tickscript-define-task)
    (define-key map (kbd "C-c C-v") #'tickscript-show-task)
    ;; Listing
    (define-key map (kbd "C-c C-l") #'tickscript-list-commands-map)
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
are both properties and nodes.  If TOPLEVEL-ONLY is specified,
only toplevel nodes \"batch\" and \"stream\" are checked."
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
  "Deindent by `tickscript-indent-offset' spaces regardless of
current indentation context."
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

(defun tickscript-move-line-or-region-down (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (tickscript--move-region-vertically beg end 1)
    (tickscript--move-line-vertically 1)))

(defun tickscript-move-line-or-region-up (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (tickscript--move-region-vertically beg end -1)
    (tickscript--move-line-vertically -1)))

(defun tickscript--move-line-vertically (dir)
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun tickscript--move-region-vertically (beg end dir)
  (let* ((point-before-mark (< (point) (mark)))
         (beg (save-excursion
                (goto-char beg)
                (point-at-bol)))
         (end (save-excursion
                (goto-char end)
                (if (bolp)
                    (point)
                  (point-at-bol 2))))
         (region (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line dir)
    (save-excursion
      (insert region))
    (if point-before-mark
        (set-mark (+ (point)
                     (length region)))
      (set-mark (point))
      (goto-char (+ (point)
                    (length region))))
    (setq deactivate-mark nil)))


(defun tickscript--deftask-get-series-name ()
  (if tickscript-series-name
      tickscript-series-name
    (let ((resp (read-string "Series name: ")))
      (setq tickscript-series-name resp)
      (add-file-local-variable 'tickscript-series-name resp)
      resp)))


(defun tickscript--deftask-get-series-type ()
  (if tickscript-series-type
      tickscript-series-type
    (let ((resp (read-string "Series type (batch/stream): ")))
      (unless (member resp '("batch" "stream"))
        (error "Must specify \"batch\" or \"stream\""))
      (setq tickscript-series-type resp)
      (add-file-local-variable 'tickscript-series-type resp)
      resp)))


(defun tickscript--deftask-get-series-dbrp ()
  (if tickscript-series-dbrp
      tickscript-series-dbrp
    (let ((resp (read-string "Series DBRP (database.retention_policy): ")))
      (setq tickscript-series-dbrp resp)
      (add-file-local-variable 'tickscript-series-dbrp resp)
      resp)))


(defun tickscript-define-task ()
  "Use Kapacitor to define the current task.

Prompts for any information needed to define the task, and then
calls Kapacitor to define it.  This information is cached in the
file comments for later re-use."
  (interactive)
  (save-buffer)
  (hack-local-variables)
  (let* ((name (tickscript--deftask-get-series-name))
         (type (tickscript--deftask-get-series-type))
         (dbrp (tickscript--deftask-get-series-dbrp))
         (filename (file-name-nondirectory (buffer-file-name)))
         (cmd (format "%s define %s -type %s -tick %s -dbrp %s"
                      tickscript-kapacitor-prog-name name type filename dbrp))
         (results (shell-command-to-string (format "echo -n \"%s - \" ; RESULT=`%s 2>&1`&& echo -n SUCCESS || echo FAILURE && echo -n $RESULT" cmd cmd))))
    (message results)))


(defun tickscript-show-task ()
  "Use Kapacitor to show the definition of the current task."
  (interactive)
  (let* ((name (tickscript--deftask-get-series-name))
         (task (shell-command-to-string (format "%s show %s"
                                                tickscript-kapacitor-prog-name name))))
    (with-output-to-temp-buffer "*tickscript-task*"
      (switch-to-buffer-other-window "*tickscript-task*")
      (set (make-local-variable 'font-lock-defaults) '(tickscript-font-lock-keywords))
      (font-lock-mode)
      (insert task))))


(defun tickscript--list-things (noun)
  (let ((things
         (shell-command-to-string (format "%s list %s" tickscript-kapacitor-prog-name noun)))
        (buffer-name (format "*tickscript-%s*" noun)))
    (with-output-to-temp-buffer buffer-name
      (switch-to-buffer-other-window buffer-name)
      (set (make-local-variable 'font-lock-defaults) '(tickscript-font-lock-keywords))
      (font-lock-mode)
      (insert things))))

(defun tickscript-list-tasks ()
  "Use Kapacitor to list all defined tasks."
  (interactive)
  (tickscript--list-things "tasks"))

(defun tickscript-list-recordings ()
  "Use Kapacitor to list all recordings."
  (interactive)
  (tickscript--list-things "recordings"))

(defun tickscript-list-replays ()
  "Use Kapacitor to list all replays."
  (interactive)
  (tickscript--list-things "replays"))

;;;###autoload
(define-derived-mode tickscript-mode prog-mode "Tickscript"
  "Major mode for editing TICKscript files

\\{tickscript-mode-map}"
  :syntax-table tickscript-mode-syntax-table

  (set (make-local-variable 'font-lock-defaults) '(tickscript-font-lock-keywords))

  (set (make-local-variable 'comment-start) "// ")

  (set (make-local-variable 'indent-line-function) 'tickscript-indent-line)

  ;; Task definition
  (set (make-local-variable 'tickscript-series-name) nil)
  (set (make-local-variable 'tickscript-series-type) nil)
  (set (make-local-variable 'tickscript-series-dbrp) nil)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tick\\'" . tickscript-mode))

(provide 'tickscript-mode)
;;; tickscript-mode.el ends here
