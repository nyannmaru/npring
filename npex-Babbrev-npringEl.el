;;First edit: Fri Nov 25 18:33:09 2022
;;Buffer specific abbrev definition For: /home/nyan/.emacs.d/nyanpack/npring/npring.el
;;Example is below if you wanna add abbrevs for all modes use 'All or nil
;;(npex-put-definitions 'c++-mode `("ab" "abbrev" (bfunc) (afunc) (rfunc)) '(...))
(npex-put-definitions 'ALL
  `("-" "npring-")
 `("--" "npring-internal--") `("-ap" "npring-internal--acceptp")
  '("cl" "current-length") `("ml" "max-length") '("mi" "mutation-idx") '("oi" "oldest-idx")
  `("cf" "constructor") `("mf" "mutator") `("df" "destructor") `("af" "acceptor")
  `("afs" "acceptor-str")
  `("orf" "(oref nio )" nil (backward-char)))
(npex-put-definitions 'ALL '("letcombo" "(let ((os (oref nio segments)) (ml (oref nio max-length)) (cl (oref nio current-length))
                 (mi (oref nio mutation-idx)) (oi (oref nio oldest-idx))
       (cf (oref nil constructor)) (mf (oref nio mutator)) (df (oref nio destructor))))"))
(npex-put-definitions 'ALL
  '("emake" (let ((fn (read-string "Enter fname: ")))
	      (format "((and %s (not (functionp %s))) (error \"given %s is not a function\"))" fn fn fn))))
(npex-put-definitions 'ALL '("nc" "(npring--interface-check npring)" nil (newline 1 t)))
