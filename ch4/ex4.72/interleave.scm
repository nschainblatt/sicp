;; Exercise 4.72: Why do disjoin and stream-flatmap in-
;; terleave the streams rather than simply append them? Give
;; examples that illustrate why interleaving works beer. (Hint:
;; Why did we use interleave in Section 3.5.3?)

;; The reason those procedures use interleave is to account for infinite loops in one of the streams.
;; Without interleave an infinite loop in one of the streams would prevent all elements from the other stream from being displayed.
;; With interleaved, we are guaranteed to see all of the elements of the non-infinite stream even if the other is infinite.
