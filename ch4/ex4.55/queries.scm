;; 1. all people supervised by Ben Bitdiddle

; (supervisor ?x (Bitdiddle Ben))

;; Explanation:
; The ?x variable allows matching against anybody.
; The literal (Bitdiddle Ben) limits results to anyone supervised by Ben Bitdiddle.

;; 2. The names and jobs of all people in the accounting division

; (job ?x (accounting . ?type))

;; Explanation:
; The ?x allows matching against anybody.
; The third item in the list uses the ?type variable along with the '.' dotted-tail notation to allow any number of
; job types in the accounting division.
;; This returns the persons name along with their specific accounting job type.

;; 3. the names and addresses of all people who live in Slumerville

; (address ?x (Slummerville . ?rest))

;; Explanation:
; The ?x allows matching against anybody.
; The '.' and ?rest usage allow any address that is in Slumerville.
;; This returns the address (which contains the name of the person it belongs too) of anyone in Slumerville.
