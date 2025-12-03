;; The reason it is required to force the operator to evaluate is because the procedure expression is just a combination in it's
;; current state. Calling operator on the  expression will return the procedure name (the first item in the combination). Apply
;; only knows how to apply combinations tagged with 'primitive or 'procedure. Calling actual-value forces the combination to be built 
;; into a procedure tagged with 'procedure. Then apply knows how to handle and apply it.
