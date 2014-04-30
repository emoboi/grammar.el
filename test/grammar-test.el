(require 'grammar)
(require 'ht)


(ert-deftest s-replace-not-regexp-test ()
  (should 
   (equal
    (s-replace-not-regexp 
     "90121470"  "\\cite{b}"
     "29838685 am a 26648151 in are 90121470.")
    "29838685 am a 26648151 in are \\cite{b}.")))
	

(ert-deftest s-replace-not-regexp-all-ht!-test ()
  (should 
   (equal
    (s-replace-not-regexp-all-ht!
     (ht ( "90121470"  "\\cite{b}" )  ("26648151"   "\\cite{a}")  ("29838685"   "\\cite{a}"))
     "29838685 am a 26648151 in are 90121470."     )
    "\\cite{a} am a \\cite{a} in are \\cite{b}." )))
    
