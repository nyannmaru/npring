;;First edit: Thu Nov 24 15:32:27 2022
;;Buffer specific abbrev definition For: /home/nyan/Desktop/prog/elisp/npring/npring-internal.el
;;Example is below if you wanna add abbrevs for all modes use 'All or nil
;;(npex-put-definitions 'c++-mode `("ab" "abbrev" (bfunc) (afunc) (rfunc)) '(...))
(npex-put-definitions 'ALL
  `("-" "npring-internal--") `("-ap" "npring-internal--acceptp")
  '("cl" "current-length") `("ml" "max-length") '("mi" "mutation-idx") '("oi" "oldest-idx")
  `("cf" "constructor") `("mf" "mutator") `("df" "destructor") `("af" "acceptor")
  `("afs" "acceptor-str")
  `("orf" "(oref nio )" nil (backward-char)))
(npex-put-definitions 'ALL '("letcombo" "(let ((os (oref nio segments)) (ml (oref nio max-length)) (cl (oref nio current-length))
                 (mi (oref nio mutation-idx)) (oi (oref nio oldest-idx))
       (cf (oref nil constructor)) (mf (oref nio mutator)) (df (oref nio destructor))))"))
