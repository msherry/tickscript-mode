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
(defvar tickscript-nodes nil)
(defvar tickscript-chaining-methods nil)


(defgroup tickscript nil
  "TICKscript support for Emacs."
  :group 'languages
  :version "0.1")

(setq tickscript-properties
      '("align" "alignGroup" "buffer" "byMeasurement" "cluster" "create" "cron"
        "database" "every" "fill" "flushInterval" "groupBy" "groupByMeasurement"
        "measurement" "offset" "period" "precision" "retentionPolicy" "tag"
        "writeConsistency"))

(setq tickscript-nodes
      '("batch" "stream" "alert" "combine" "default" "delete" "derivative"
        "eval" "flatten" "from" "groupBy" "httpOut" "httpPost" "influxDBOut"
        "influxQL" "join" "k8sAutoscale" "kapacitorLoopback" "log" "noOp"
        "query" "sample" "shift" "stateCount" "stateDuration" "stats" "uDF"
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

;;;###autoload
(define-derived-mode tickscript-mode prog-mode "Tickscript"
  "Major mode for editing TICKscript files

\\{tickscript-mode-map}"
  :syntax-table tickscript-mode-syntax-table
  (setq font-lock-defaults '(tickscript-font-lock-keywords))
  (font-lock-ensure)
  (set (make-local-variable 'comment-start) "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tick\\'" . tickscript-mode))

(provide 'tickscript-mode)
;;; tickscript-mode.el ends here
