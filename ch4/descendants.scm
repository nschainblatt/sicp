; Descendants
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)


; Formulate rules such as:
; “If S is the son of f, and f is the
; son of G, then S is the grandson of G” and “If W is the wife
; of M, and S is the son of W, then S is the son of M”


; If S is the son of f, and f is the son of G, then S is the grandson of G.
(rule (grandson-of ?gs ?gp)
      (son-of ?gs ?f)
      (son-of ?f ?gp))

; If W is the wife of M, and S is the son of W, then S is the son of M.
(rule (son-of ?s ?m)
      (wife-of ?w ?m)
      (son-of ?s ?w))
