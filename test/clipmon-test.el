;;; clipmon-test.el --- tests for clipmon.el

;;; Commentary:

; Run standalone with
; $ emacs -batch -L . -l clipmon-test.el -f ert-run-tests-batch
;
; Running 2 tests (2014-12-27 01:01:14-0600)
;    passed  1/2  all-transforms
;    passed  2/2  no-transforms
; Ran 2 tests, 2 results as expected (2014-12-27 01:01:14-0600)


;;; Code:

(require 'ert)
(require 'clipmon)


(ert-deftest no-transforms ()
  "Try with no transforms on text."
  (let ((clipmon-trim-string nil)
        (clipmon-remove-regexp nil)
        (clipmon-prefix nil)
        (clipmon-suffix nil)
        (clipmon-transform-function nil))
    (should
     (equal
      (clipmon--transform-text
       " Marbled murrelets use old-growth forest stands for nesting.[2][3] "
       )
       " Marbled murrelets use old-growth forest stands for nesting.[2][3] "
      ))
    ))


(ert-deftest all-transforms ()
  "Try all text transforms."
  (let ((clipmon-trim-string t)
        (clipmon-remove-regexp "stands \\|\\[[0-9]+\\]\\|\\[citation needed\\]")
        (clipmon-prefix "<<")
        (clipmon-suffix ">>")
        (clipmon-transform-function (lambda (s) (downcase s))))
    (should
     (equal
      (clipmon--transform-text
       " Marbled murrelets use old-growth forest stands for nesting.[2][3] "
       )
       "<<marbled murrelets use old-growth forest for nesting. >>"
      ))
    ))



;;; clipmon-test.el ends here
