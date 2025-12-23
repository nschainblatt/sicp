(rule (big-shot? ?person ?division)
      (and (supervisor ?x ?person) ;; Require the person to be a supervisor
	   (job ?x (?division . ?type)) ;; of someone in the division
	   (job ?person (?division . ?type)) ;; and be in the division themselves.
	   (or (and (supervisor ?person ?super) ;; And their supervisor
		    (job ?super ?type2) ;; of some job type
		    (not (same (?division . ?type) ?type2))) ;; is not in the same division.
	       (not (supervisor ?person ?super)))))) ;; Or does not have a supervisor at all.
