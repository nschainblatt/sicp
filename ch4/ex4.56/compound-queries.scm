; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where)

; b. all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary;
(and (salary ?person ?person-amount)
     (salary (Bitdiddle Ben) ?ben-amount)
     (list-value < ?person-amount ?ben-amount))

; c. all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job.
(and (supervisor ?person ?sup) ;; supervisor with subordinate
     (not (job ?sup (computer . ?type))) ;; supervisor is not in the computer division
     (job ?sup ?type)) ;; retrieve the full job for the supervisor
