;;; clipmon-test.el --- tests for clipmon.el

;;; Commentary:

; Run standalone with
; $ emacs -batch -L . -l clipmon-test.el -f ert-run-tests-batch
;
; Running 2 tests (2014-12-27 01:01:14-0600)
;    passed  1/2  all-transforms
;    passed  2/2  no-transforms
; Ran 2 tests, 2 results as expected (2014-12-27 01:01:14-0600)
;
;
; or internally with (ert-run)
; or (ert-run-tests-interactively "regexp")


;;; Code:

(require 'ert)
(require 'clipmon)


(ert-deftest try-no-transforms ()
  "Try with no transforms on text."

  (let ((clipmon-trim-string nil)
        (clipmon-remove-regexp nil)
        (clipmon-prefix nil)
        (clipmon-suffix nil)
        (clipmon-transform-function nil))

    (should (equal
             (clipmon--transform-text
              " Marbled murrelets use old-growth forest stands for nesting.[2][3] "
              )
             " Marbled murrelets use old-growth forest stands for nesting.[2][3] "
             ))
    ))


(ert-deftest try-all-transforms ()
  "Try all text transforms."

  (let ((clipmon-trim-string t)
        (clipmon-remove-regexp "stands \\|\\[[0-9]+\\]\\|\\[citation needed\\]")
        (clipmon-prefix "<<")
        (clipmon-suffix ">>")
        (clipmon-transform-function (lambda (s) (downcase s))))

    (should (equal
             (clipmon--transform-text
              " Marbled murrelets use old-growth forest stands for nesting.[2][3] "
              )
             "<<marbled murrelets use old-growth forest for nesting. >>"
             ))
    ))


(ert-deftest try-remove-regexp ()
  "Try the remove-regexp for Wikipedia references."

  (let ((clipmon-trim-string nil)
        ; use the default remove-regexp
        (clipmon-prefix nil)
        (clipmon-suffix nil)
        (clipmon-transform-function nil))

    (should (equal
             (clipmon--transform-text
              " Marbled [1 2] murrelets[115] use [old-growth][99] stands [1984] for nesting.[2] "
              )
              " Marbled [1 2] murrelets use [old-growth] stands [1984] for nesting. "
             ))
    ))

;;; clipmon-test.el ends here
