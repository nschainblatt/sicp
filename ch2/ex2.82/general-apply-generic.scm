;; The reason the original apply-generic with coercion isn't considered sufficiently general because we will prioritize the first coercion match.
;; This is inside the cond, where if the first coercion procedure we check has a value, we always apply that one. Which may lead to always failing to find a procedure because
;; we don't retry the other types in that cond.
;; For example, if t1->t2 contains a procedure instead of #f, the first cond condition will always evaluate, leaving the second procedure in the cond to never be attempted.
;; (let ((t1->t2 (get type1 type2))
;;       (t2->t1 (get type2 type1)))
;;   (cond (t1->t2
;;           (apply-generic op (t1->t2 a1) a2))
;;         (t2->t1
;;           (apply-generic op a1 (t2->t1 a2)))
;;         (else (error "No method for these types"
;;                      (list op type-tags))))))

;; The new 'generalized' general-apply-generic isn't sufficiently general either mostly because of the same reason. We did add more types to coerce, but each time we do we eliminate,
;; other potential types to try. For example, with the first call, we find a coercion procedure to apply, and call general-apply-generic again with the coerced types, however we also
;; left behind another type coercion possibility in the remaining cond conditions.

(define nil '())

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get type1 type2))
                  (t2->t1 (get type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;; TODO: skip args that have same type as arg we are casting.
(define (find-first-args-coercion args)
  (define (recur sub-args)
    (cond ((null? sub-args) (error "No method for these types" (list args)))
          ((all-args-have-coercion? args (type-tag (car (sub-args)))) (coerce-all args (type-tag (car sub-args)))) ;; NOTE: important for args and subargs here.
          (else (recur (cdr sub-args)))))
  (recur args))

(define (all-args-have-coercion? args type)
  (if (null? args)
    #t
    (let ((t1->t2 (get type (type-tag (car args)))))
      (if (or t1->t2 (equal? type (type-tag (car args)))) ;; procedure exists or same type
        (all-args-have-coercion? (cdr args) type)
        #f))))

(define (coerce-all args type)
  (if (null? args)
    nil
    (let* ((arg (car args))
           (t1->t2 (get type (type-tag arg))))
      (t1->t2 arg))))

;; TODO:
;; - when no proc is found, attempt to coerce all args (if possible) one type at a time, starting with the first
;; - when all args can coerce to the first, then we will try to find an op with those types (this is the reason its not very general, first match will cause other type coercions to
;;     note be attempted)
;; NOTE:
;; Updates include the following:
;;   - if a procedure isn't found for the 'op' and type of 'args', then we try to coerce all arguments to the type of the first argument until the last, stopping on the first
;;     match of updated types. When a match is found, we try again with those types with general-apply-specific. If no match is found we throw an error like before.
;;     This version will only try the first match once, and then since all future calls to general-apply-specific will have all arguments of the same type, if no proc is found then
;;    then a error is thrown.
;;
;;    TODO: test this, but also probably need to update to actually try the other types to coerce, instead of trying the first match, then probably failing immediately after because
;;          all the types are the same on the next call to general-apply-specfic leading to an error.
;;  
(define (general-apply-specific op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (general-apply-specific op (find-first-args-coercion args))))))
