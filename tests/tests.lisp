(in-package :glyphs-test)

(in-root-suite)
(defsuite root-suite)
(in-suite root-suite)

(in-readtable glyphs:syntax)

(ƒ addition
   α → (+ α αb))

(ƒ any-cats?
   ~"cat"~ → "yes"
   α → "no")

(ƒ bad-to-good-sad-to-happy
   ~"bad"~ → |"good"|
   ~"sad"~ → |"happy"|
   α → α)

(ƒ facto
   0 → 1
   α → (* α (facto (1- α))))

(deftest function-define ()
  (is (= (+ 3 2) (addition 3 2))))

(deftest regex-matching ()
  (is (equal "yes" (any-cats? "I love cats")))
  (is (equal "no" (any-cats? "I love dogs"))))

(deftest regex-replacement ()
  (is (equal "I'm a good boy" (bad-to-good-sad-to-happy "I'm a bad boy")))
  (is (equal "I'm a happy boy" (bad-to-good-sad-to-happy "I'm a sad boy")))
  (is (equal "I'm a boy" (bad-to-good-sad-to-happy "I'm a boy"))))

(deftest mapped-results ()
  (is (equal '(12 15 18)
             (mapcar (λ α → (+ α αb αc))
                     '(1 2 3) '(4 5 6) '(7 8 9)))))

(deftest facto-test ()
  (is (= 40320 (facto 8))))

(defun run-tests ()
  (root-suite))
