;; Exercise 4.72: Why do disjoin and stream-flatmap in-
;; terleave the streams rather than simply append them? Give
;; examples that illustrate why interleaving works beer. (Hint:
;; Why did we use interleave in Section 3.5.3?)

;; The reason those procedures use interleave is to account for infinite loops in one of the streams.
;; As we saw with ex4.71, some rules may produce infinite streams.
;; Without interleave an infinite loop would prevent all assertions (non-rule db items) from being displayed.
;; Since the assertions (non-infinite) and the rules (potentially infinite) are placed as the two streams respecitvely
;; to interleave, we are guranteed to view all assertions in the database even if the rules are infinite.
