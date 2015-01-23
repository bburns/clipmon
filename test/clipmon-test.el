;;; clipmon-test.el --- tests for clipmon.el
;;; Commentary:

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

  (let ((clipmon-trim-string nil)
        (clipmon-remove-regexp nil)
        (clipmon-prefix nil)
        (clipmon-suffix nil)
        (clipmon-transform-function nil))

    (should (equal
             (clipmon--transform-text
               " Marbled murrelets use old-growth forest stands for nesting.[2][3] ")
               " Marbled murrelets use old-growth forest stands for nesting.[2][3] "))
    ))


(ert-deftest clipmon-test-all-transforms ()
  "Try all text transforms."

  (let ((clipmon-trim-string t)
        (clipmon-remove-regexp "stands \\|\\[[0-9]+\\]\\|\\[citation needed\\]")
        (clipmon-prefix "<<")
        (clipmon-suffix ">>")
        (clipmon-transform-function (lambda (s) (downcase s))))

    (should (equal
             (clipmon--transform-text
               " Marbled murrelets use old-growth forest stands for nesting.[2][3] ")
               "<<marbled murrelets use old-growth forest for nesting. >>"))
    ))


(ert-deftest clipmon-test-remove-regexp ()
  "Try the remove-regexp for Wikipedia references."

  (let ((clipmon-trim-string nil)
        ; use the default remove-regexp
        (clipmon-prefix nil)
        (clipmon-suffix nil)
        (clipmon-transform-function nil))

    (should (equal
             (clipmon--transform-text
               " Marbled [1 2] murrelets[115] use [old-growth][99] stands [1984] for nesting.[2] ")
               " Marbled [1 2] murrelets use [old-growth] stands [1984] for nesting. "))
    ))


(ert-deftest clipmon-test-mode-on-off ()
  "Have you tried turning it off and on again?"
  (let ((clipmon-sound nil))
    
    ; off
    (clipmon-stop)
    (should (null clipmon-mode))
    (clipmon-stop)
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
    (clipmon-start)
    (should clipmon-mode)
    
    ; off
    (clipmon-mode 0)
    (should (null clipmon-mode))
    ))


(ert-deftest clipmon-test-timeout ()
  "Let clock timeout."
  (let ((clipmon-interval 0.1) ; secs
        (clipmon-timeout (/ 0.2 60.0)) ; 0.2 secs in mins
        (sleep-amount 0.4) ; secs
        (clipmon-sound nil))
    
    (clipmon-stop)
    (clipmon-start)
    (should clipmon-mode)
    (sleep-for sleep-amount) ; wait for timeout
    (should (null clipmon-mode))
    ))

  

;;; clipmon-test.el ends here
