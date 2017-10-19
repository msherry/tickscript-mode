;;; tickscript-mode-tests.el --- Tests for tickscript-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Marc Sherry
;; Homepage: https://github.com/msherry/tickscript-mode
;; Version: 0.1
;; Author: Marc Sherry <msherry@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.1"))

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

;;; Usage:

;; From command line:
;;
;; emacs -batch -L . -l ert -l tickscript-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Commentary:
;; Contains ert tests for tickscript-mode.el.  Based on
;; https://github.com/JuliaEditorSupport/julia-emacs/blob/master/julia-mode-tests.el
;;

;;; Code:
(require 'tickscript-mode)
(require 'ert)

(defmacro tickscript--should-indent (from to)
  "Assert that we indent text FROM producing text TO in `tickscript-mode'."
  `(with-temp-buffer
     (let ((tickscript-indent-offset 4))
       (tickscript-mode)
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (equal
                (buffer-substring-no-properties (point-min) (point-max))
                ,to)))))

(defmacro tickscript--should-font-lock (text pos-faces)
  "Assert that the TEXT gets font-locked correctly at each position in POS-FACES."
  `(with-temp-buffer
     (tickscript-mode)
     (insert ,text)
     (when (fboundp #'font-lock-ensure)
       (font-lock-ensure (point-min) (point-max)))
     (with-no-warnings
       (when (fboundp #'font-lock-flush)
         (font-lock-flush)))
     (font-lock-fontify-buffer)
     (dolist (pair ,pos-faces)
       (let ((pos (car pair))
             (face (cdr pair)))
         (should (eq face (get-text-property pos 'face)))))))

(defmacro tickscript--should-cleanup-dot (from to)
  "Assert that we clean up the dot FROM into TO."
  `(with-temp-buffer
     (insert ,from)
     (let ((cleaned (tickscript--cleanup-dot (tickscript--extract-dot-from-buffer))))
       (should (equal cleaned ,to)))))

(ert-deftest tickscript--test-indent-properties ()
  "Properties should be indented under nodes."
  (tickscript--should-indent
   "
batch
|query(SQL)
.period(1d)
.every(1h)
.groupBy(*)
.align()
.fill('null')
.cluster('local')
"
   "
batch
    |query(SQL)
        .period(1d)
        .every(1h)
        .groupBy(*)
        .align()
        .fill('null')
        .cluster('local')
"))

(ert-deftest tickscript--test-indent-var-declaration ()
  "Var declarations should be indented to 0."

  (tickscript--should-indent
   "
var day_median = day_batched
|median('duration')
.as('day_median')
"
   "
var day_median = day_batched
    |median('duration')
        .as('day_median')
"))

(ert-deftest tickscript--test-indent-node-instance ()
  "Previously-defined nodes should be indented to 0."

  (tickscript--should-indent
   "
day_batched
|median('duration')
.as('day_median')
"
   "
day_batched
    |median('duration')
        .as('day_median')
"))

(ert-deftest tickscript--test-indent-udf ()
  "User-defined functions should be indented similarly to chaining functions."

  (tickscript--should-indent
   "
var my_custom = other_thing
@my_udf('duration')
.as('day_udf')
"
   "
var my_custom = other_thing
    @my_udf('duration')
        .as('day_udf')
"))

(ert-deftest tickscript--test-indent-within-parens ()
  "Lines following an unclosed paren should indent to the column after the paren."

  (tickscript--should-indent
   "
var pcts_of_medians = hour_median
|join(day_median, week_median)
.as('hour_median', 'day_median', 'week_median')
|eval(lambda: \"hour_median.value\" / \"day_median.value\",
lambda: \"hour_median.value\" / \"day_median.value\")
.as('pct_of_daily_median', 'pct_of_weekly_median')
"
   "
var pcts_of_medians = hour_median
    |join(day_median, week_median)
        .as('hour_median', 'day_median', 'week_median')
    |eval(lambda: \"hour_median.value\" / \"day_median.value\",
          lambda: \"hour_median.value\" / \"day_median.value\")
        .as('pct_of_daily_median', 'pct_of_weekly_median')
"))

(ert-deftest tickscript--test-indent-comments ()
  "Comments should be indented at the level of the line above
them, unless there is a blank line above, in which case they
should be indented to zero."

  (tickscript--should-indent
   "
// This is a definition
batch
|query(SQL)
.groupBy(*)
// .period(1h)    // commented out
.window(1d)
// .fill('null')

// A new comment
"
   "
// This is a definition
batch
    |query(SQL)
        .groupBy(*)
        // .period(1h)    // commented out
        .window(1d)
        // .fill('null')

// A new comment
"))

(ert-deftest tickscript--test-font-locking ()
  "Test that font locking is applied correctly."
  (tickscript--should-font-lock
   "
var day_batched = batch
    |query(SQL)
        .period(1d)
"
   '((2 . font-lock-keyword-face)
     (6 . tickscript-variable)
     (20 . tickscript-node)
     (30 . tickscript-operator)
     (51 . tickscript-property)))

  (tickscript--should-font-lock
   "
batch
    |query('')
        .groupBy(*)
    |groupBy(*)
"
   '((32 . tickscript-property)
     (47 . tickscript-operator)
     (48 . tickscript-node))
))

(ert-deftest tickscript--test-basic-dot ()
  "Test that we don't break valid DOT."
  (tickscript--should-cleanup-dot
   "
DOT:
digraph medians {
query3 -> median14;
query3 -> mean15;
query3 -> count16;
count16 -> join21;
}
" "digraph medians {
query3 -> median14;
query3 -> mean15;
query3 -> count16;
count16 -> join21;
}
"))

(ert-deftest tickscript--test-dot-needing-cleanup ()
  "Test cleaning the broken DOT given by a running task."
  (tickscript--should-cleanup-dot
   "
DOT:
digraph medians {
graph [throughput=\"0.00 batches/s\"];

query3 [avg_exec_time_ns=\"0s\" batches_queried=\"0\" errors=\"0\" points_queried=\"0\" working_cardinality=\"0\" ];
query3 -> count16 [processed=\"0\"];
query3 -> mean15 [processed=\"0\"];
query3 -> median14 [processed=\"0\"];

count16 [avg_exec_time_ns=\"0s\" errors=\"0\" working_cardinality=\"0\" ];
count16 -> join21 [processed=\"0\"];
}
" "digraph medians {
graph [\"throughput=\\\"0.00 batches\\\/s\\\"\"];

query3 [\"avg_exec_time_ns=\\\"0s\\\" batches_queried=\\\"0\\\" errors=\\\"0\\\" points_queried=\\\"0\\\" working_cardinality=\\\"0\\\" \"];
query3 -> count16 [\"processed=\\\"0\\\"\"];
query3 -> mean15 [\"processed=\\\"0\\\"\"];
query3 -> median14 [\"processed=\\\"0\\\"\"];

count16 [\"avg_exec_time_ns=\\\"0s\\\" errors=\\\"0\\\" working_cardinality=\\\"0\\\" \"];
count16 -> join21 [\"processed=\\\"0\\\"\"];
}
"))


(provide 'tickscript-mode-tests)
;;; tickscript-mode-tests.el ends here
