
;; branch of form (a -> b cost)
;; construction
(define  (make-path start end cost) (list start '-> end cost))
;; deconstruction
(define (start-node branch) (if (null? branch) '() (first branch)))
(define (end-node branch) (third branch))
(define (branch-cost branch) (if (null? branch)0 (fourth branch)))

(define (find-path-to node paths)
  (let ((ans (list-transform-positive paths (lambda (br)(equal? node (end-node br))) )))
   (if (null? ans) 
       '()
       (car ans)))
 )

(define (find-paths-from node paths)
   (list-transform-positive paths (lambda (br)(equal? node (start-node br))) ) 
)

(define (augment-path p1 p2)
  (if (null? p1) 
      p2
      (cons (cons (start-node p1) (start-node p2)) (cdr p2)))
)


;; SETI ((a->c) (b->d) (f->g)
;; SETII ((c->e) (d->e) )
(define (calc-node-and-branch-cost  setII-branches setI)
  (cond ((null? setII-branches) '())
        (#t (let* ((br (car setII-branches)) (path (find-path-to (start-node br) setI )) )
              (cons (list (+ (branch-cost path) (branch-cost br)) (end-node br) (augment-path path br) ) (calc-node-and-branch-cost (cdr setII-branches) setI) )
              ) ))
 )

(define (path-equal? p q)
  (and (equal? (start-node p) (start-node q)) (equal? (end-node p) (end-node q)))
)


;: (((12 e (c -> e 4)) (11 e (d -> e 8))))
(define (min-cost-path lols)
  (define (minnie curr elts)
    (cond ((null? elts) curr)
          ((< (caar elts) (car curr)) (minnie (car elts) (cdr elts))  )
          (#t (minnie curr (cdr elts)))
))
  (minnie (car lols) (cdr lols))
)


;Value: (((12 e (c -> e 4)) (11 e (d -> e 8))))
(define (find-minimum-path nodes setI setII)
                                        ; find branches in setII that end in this node
                                        ; find branches in set1 that link to that
                                        ; calc sum
                                        ; return min
 
  (min-cost-path (apply append (map (lambda (n) 
                                      (let ((setII-branches-ending-in-node           
                                             (list-transform-positive setII (lambda (br)(equal? n (end-node br))) )
                                             ))
                                        (calc-node-and-branch-cost setII-branches-ending-in-node setI )
                                        )) nodes)))  )



(define path-delq
   (delete-member-procedure list-deletor path-equal?))


(define (cpath->node cpath)  (second cpath))

;Value: (((12 e (c -> e 4)) (11 e (d -> e 8))))
(define (make-new-costed-path path)
  (let ( (new-cost (car path) ) 
         (to-node (cpath->node path))
         (from-node (car (third path))))
    (make-path from-node to-node new-cost))
)

(define (make-world A B C i ii iii)
  (list A B C i ii iii))

(define (A-Nodes w) (first w))
(define (B-Nodes w) (second w))
(define (I-Paths w) (fourth w))
(define (II-Paths w) (fifth w))


(define (Step2 w)
  (let* ((costed-path (find-minimum-path (second w) (fourth w) (fifth w)))
         (node (cpath->node costed-path))
         (new-path (make-new-costed-path costed-path))
         )
  (make-world 
   (cons node (first w)) 
   (delete node (second w) )
   (third w)
   (cons new-path (I-Paths w))
   (path-delq new-path (fifth w))
   (sixth w)
   )
    )  )


(define (swap-if-shorter R branch ii )
    
  (cond ((null? ii) '() )
        ((and (equal? (end-node (car ii)) R) (< (branch-cost branch ) (branch-cost (car ii)))) (cons branch (swap-if-shorter R branch (cdr ii))))
        (#t (cons (car ii) (swap-if-shorter R branch (cdr ii)))))
            
  
)


(define (consider-the-branches branches w)
  (cond ((null? branches) w)
        (#t  (let* ( (branch (car branches)) (R (end-node branch))) 
               ;(display branch)
               ;(display R)
               (if (member R (B-Nodes w))
                   (consider-the-branches (cdr branches) (make-world (first w) (second w) (third w) (fourth w)   (swap-if-shorter R branch (fifth w)) (sixth w)))
                   (consider-the-branches (cdr branches) (make-world (first w) (cons R (second w)) (delete R (third w)) (fourth w)   (cons branch (fifth w)) (path-delq branch (sixth w))))
                   ))))
)






(define (branches node ii iii)
  (append (find-paths-from node ii) (find-paths-from node iii)))

;; todo clean this up to get the bs and assume the others are Cs 
;; also get rid of mutations
;; remember to access only the paths needed rather than expecting the whole thing to be laid out.
(define (Step1 new-node w)
  (let ((r (branches new-node (fifth w) (sixth w)) ) )
    (consider-the-branches r w)
))


(define (keep-looking p q w)
  (cond ((member q (A-Nodes w))  (I-Paths w)) ;; all done!
        (#t (let ((w1 (step2  (step1 p w))))   
             (display "state: ")
              (display w1)
              (display "\n")
              (keep-looking (car (A-Nodes w1)) q  w1)))
        )
)



;;Find the path of minimum total length between two given nodes P and Q
; Fact: if R is a node on the path from P->Q then P->R is minimal too
; so build up the path till Q is reached
; Move P to A then follow the steps
; Step 1
; Consider all branches r connecting the node just transferred to A (P) 
; with nodes R  in B or C.  If node R is in B check r < P->R path against the path that
; uses the corresponding branch in II.  Swap them if so (moving the other to rejected)
; if node R in C, add it to B and add r to II
; Step 2
; Every node in B is one step away from the paths in I.  Transfer the one with the minimum distance ; from P - from B to A and the branch from II to I
; return to 1 and repeat until Q is moved to A

(define test-world (list '(a) '() '(b c d) '() '() (list 
                                                       (make-path 'a 'b 3) 
                                                       (make-path 'b 'c 3)
                                                       (make-path 'c 'd 3)
                                                       (make-path 'a 'd 13)
                                                       (make-path 'b 'd 23)
                                                       ) ))

(define rosetta-world (list '(a) '() '(b c d e f) '() '() (list 
                                                       (make-path 'a 'b 7) 
                                                       (make-path 'a 'c 9)
                                                       (make-path 'a 'f 14)
                                                       (make-path 'b 'c 10)
                                                       (make-path 'b 'd 15)
                                                       (make-path 'c 'd 11)
                                                       (make-path 'c 'f 2)
                                                       (make-path 'd 'e 6) 
                                                       (make-path 'e 'f 9)  ) ))



