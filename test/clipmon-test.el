;;; clipmon-test.el --- tests for clipmon.el
;;; Commentary:

; Just test the complicated things. 

; Run standalone with
; $ emacs -batch -L . -l clipmon-test.el -f ert-run-tests-batch
;
; Running 2 tests (2014-12-27 01:01:14-0600)
;    passed  1/2  clipmon-test-all-transforms
;    passed  2/2  clipmon-test-no-transforms
; Ran 2 tests, 2 results as expected (2014-12-27 01:01:14-0600)
;
;
; or internally with (ert-run)
; or (ert-run-tests-interactively "regexp")


;;; Code:

(require 'ert)
(require 'clipmon)


(ert-deftest clipmon-test-no-transforms ()
  "Try with no transforms on text."

  (let ((clipmon-transform-trim nil)
        (clipmon-transform-remove nil)
        (clipmon-transform-prefix nil)
        (clipmon-transform-suffix nil)
        (clipmon-transform-function nil))

    (should (equal
             (clipmon--autoinsert-transform-text
               " Marbled murrelets use old-growth forest stands for nesting.[2][3] ")
               " Marbled murrelets use old-growth forest stands for nesting.[2][3] "))
    ))


(ert-deftest clipmon-test-all-transforms ()
  "Try all text transforms."

  (let ((clipmon-transform-trim t)
        (clipmon-transform-remove "stands \\|\\[[0-9]+\\]\\|\\[citation needed\\]")
        (clipmon-transform-prefix "<<")
        (clipmon-transform-suffix ">>")
        (clipmon-transform-function (lambda (s) (downcase s))))

    (should (equal
             (clipmon--autoinsert-transform-text
               " Marbled murrelets use old-growth forest stands for nesting.[2][3] ")
               "<<marbled murrelets use old-growth forest for nesting. >>"))
    ))


(ert-deftest clipmon-test-remove-regexp ()
  "Try the remove-regexp for Wikipedia references."

  (let ((clipmon-transform-trim nil)
        ; use the default remove-regexp
        (clipmon-transform-prefix nil)
        (clipmon-transform-suffix nil)
        (clipmon-transform-function nil))

    (should (equal
             (clipmon--autoinsert-transform-text
               " Marbled [1 2] murrelets[115] use [old-growth][99] stands [1984] for nesting.[2] ")
               " Marbled [1 2] murrelets use [old-growth] stands [1984] for nesting. "))
    ))


(ert-deftest clipmon-test-on-and-off ()
  "Try turning mode and autoinsert on and off."
  (let ((clipmon-autoinsert-sound nil))
    
    ; off
    ; throws an error if it's already stopped
    (ignore-errors (clipmon-stop))
    (should (null clipmon-mode))
    (ignore-errors (clipmon-stop))
    (should (null clipmon-mode))

    ; on
    (clipmon-mode 'toggle)
    (should clipmon-mode)
    
    ; off
    (clipmon-mode 'toggle)
    (should (null clipmon-mode))
    
    ; on
    (clipmon-start)
    (should clipmon-mode)
    (ignore-errors (clipmon-start))
    (should clipmon-mode)
    
    ; off
    (clipmon-mode 0)
    (should (null clipmon-mode))

    
    ; autoinsert

    (ignore-errors (clipmon-autoinsert-stop))
    (should (null clipmon--autoinsert))
    (should (null clipmon-mode))

    ; on
    (clipmon-autoinsert-toggle)
    (should clipmon--autoinsert)
    (should clipmon-mode) ; should automatically turn the mode on

    ; off
    (clipmon-autoinsert-toggle)
    (should (null clipmon--autoinsert))
    (should clipmon-mode) ; should stay on
    
    (clipmon-mode 0)
    (should (null clipmon-mode))
    
    ))


(ert-deftest clipmon-test-timeout ()
  "Let clock timeout."
  (let ((clipmon-timer-interval 0.1) ; secs
        (clipmon-autoinsert-timeout (/ 0.2 60.0)) ; 0.2 secs in mins
        (sleep-amount 0.4) ; secs
        (clipmon-autoinsert-sound nil))
    
    (if clipmon--autoinsert (clipmon-autoinsert-stop))
    (clipmon-autoinsert-start)
    (should clipmon--autoinsert)
    (should clipmon-mode) ; should turn this on also
    (sleep-for sleep-amount) ; wait for timeout
    (should (null clipmon--autoinsert))
    (should clipmon-mode) ; should still be on
    ))

  

;;; clipmon-test.el ends here
