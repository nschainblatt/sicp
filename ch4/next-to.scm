;; The following rules implement a next-to
;; relation that finds adjacent elements of a list:

;a.
(rule (?x next-to ?y in (?x ?y . ?u)))

;b.
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;; What will the response be to the following queries?

;1
(?x next-to ?y in (1 (2 3) 4))

;a.
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))
(4 next-to () in (1 (2 3) 4))

;b
; Not really sure what this query means. Are we to assume that ?x and ?y are in (?v . ?z) and that every
; element will be part of the output in each possible combination?


;2.
(?x next-to 1 in (2 1 3 1))

;a.
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

;b
; "
