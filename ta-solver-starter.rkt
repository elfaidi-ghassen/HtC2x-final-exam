;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt



;; Data Definitions
(define-struct user (name verified following))
;; User is (make-user String Bool lou)
;; ListOfUser is one of:
;; - empty
;; - (cons User listof User)


#;(define (fn-for-user u0)
    (local [(define (fn-for-user u todo visited)
              (if (member (user-name u) visited)
                  (fn-for-lou todo visited)
                  (fn-for-lou (append (user-following u) todo) (cons user-name visited))
                  ));(user-name u
              

            (define (fn-for-lou todo visited)
              (cond [(empty? todo) (...)]
                    [else
                     (fn-for-user (first todo) (rest todo) visited)
                     ])
              )]
      (fn-for-user u0 empty empty)

      ))

(define NETWORK-F
  (shared((-G- (make-user "Gus" true (list -ML-)))
          (-ML- (make-user "Malek" true (list -N-)))
          (-M- (make-user "Mike" true (list -G- -A- -HN-)))
          (-A- (make-user "Ahmed" true (list -Z- -R- -G-)))
          (-MD- (make-user "Moby" true (list -ML- -N- -G-)))
          (-Z- (make-user "Zaydan" true (list -G- -ML-)))
          (-HN- (make-user "Hind" true (list -I- -N- -G-)))
          (-R- (make-user "Rami" true (list -M- -N- -G-)))
          (-S- (make-user "Sami" true (list -ML- -A- -G-)))
          (-I- (make-user "Ira" true (list -S- -G-)))
          (-H- (make-user "Houcine" true (list -G- -R-)))
          (-N- (make-user "Naceur" true (list -A- -ML- -G-)))
          )
    -G-))


;; functions
;; User -> String
(check-expect (most-followers NETWORK-F) "Gus")

;; (define (most-followers u) "")

(define (most-followers u0)
  ;; rsf: (make-status name nb-followers); result so far accumulator; contains the number of followers
  (local [
          (define (increment-follow name los)
            (cond [(empty? los) empty]
                  [else
                   (if (string=? name (status-name (first los)))
                       (cons
                        (make-status name (add1 (status-fcount (first los)))) 
                        (increment-follow name (rest los)))
                       (cons (first los)(increment-follow name (rest los)))

                       )]))
          (define (max-follow los) ;; los -> status
            (local [(define (max-follow los smax)
                      (cond [(empty? los) smax]
                            [else
                             (if (>= (status-fcount (first los)) (status-fcount smax))
                                 (max-follow (rest los) (first los))
                                 (max-follow (rest los) smax)
                                 )
      
                             ]) 


                      )]
              (max-follow los (make-status "" -1))
              ))

          
          (define-struct status (name fcount))
          (define (fn-for-user u todo visited rsf)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited rsf) 
                (fn-for-lou
                 (append (user-following u) todo)
                 (cons (user-name u) visited)
                 rsf)
                ));(user-name u)
              

          (define (fn-for-lou todo visited rsf)
            (cond [(empty? todo) (status-name(max-follow rsf))]
                  [else
                   (fn-for-user (first todo) (rest todo) visited (increment-follow (user-name (first todo))
                                                                                   (if (member (user-name (first todo)) (map (lambda (s) status-name) rsf))
                                                                                       rsf
                                                                                       (cons (make-status (user-name  (first todo)) 0) rsf)
                                                                                       )))
                   ])
            )]
    (fn-for-user u0 empty empty empty)

    ))








;; Problem II


;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))
;
(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)

;; !!!(listof TA) (listof Slot) -> Schedule or false
;; (define (schedule-tas tas slots) empty) ;stub
 
;; <template> we should <generate> a <n-airy tree> of all possible [valid] choices
;; and do <backtracking search> over it, and since the tree can get very big

(define (schedule-tas tas slots1)  
  (local [(define (fn-for-schedule s tas0 slots0)
            (if (= (length s) (length slots1))
                s
                (fn-for-los (generate-schedules s tas0 slots0) tas0 (rest slots0)) 
                )
            )
          (define (fn-for-los los tas0 slots0)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (fn-for-schedule (first los) tas0 slots0))] 
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los) tas0 slots0)
                         )
                     )
                   ])
            )]
    (fn-for-schedule empty tas slots1)
    ))

          
;; (define (generate-schedules s tas slots) s)
;; Schedule (listof ta) (listof slot) -> (listof Schedule)
;; (list of assignments) (listof ta) (listof slot) -> (listof (listof assignments))
(define (generate-schedules s tas slots)
  (cond [(empty? tas) empty]
        [else
         (local [(define assignment (make-assignment (first tas) (first slots)))
                 (define TA (first tas))
                 (define TA-NAME (ta-name TA))
                 (define SNUMBER (first slots))
                 (define AVAIL (ta-avail TA))
                
                 (define (previous-tasks s name)
                   (map (lambda (a) (assignment-slot a))
                        (filter (lambda (a) (string=? name (ta-name (assignment-ta a)))) s))
                   
                   )
                 ]
           (if (and (member SNUMBER AVAIL)
                    (< (length (previous-tasks s TA-NAME))
                       (ta-max TA)) 

                    )
               (cons (cons assignment  s) (generate-schedules s (rest tas) slots))
               (generate-schedules s (rest tas) slots)
               )) 

            
         
         ])

  )



