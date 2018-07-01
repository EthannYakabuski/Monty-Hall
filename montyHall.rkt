(#%require (only racket/base random))


(define doors '())
(define playerChoice 0)
(define carLocation 0)
(define doorsGraphics (cons '(1) (cons '(2) (cons '(3) '()))))

;main body
(define (montyHall)
   (setDoors)
   (askPlayerToChooseDoor)
   (revealDoor)
  )


;filling/showing doors logic
;-------------------------------------------------------------------------------------
;place two goats and one car into the doors list
(define (setDoors)
  (chooseCarLocation)
  (fillWithGoats)
  )

;chooses the car location
(define (chooseCarLocation)
  (set! carLocation (random 3))
  )

;sets the doors list to the appropriate outcome after deciding car location using above procedure
(define (fillWithGoats)
  (if (= carLocation 0) (set! doors (cons 1 (cons 0 (cons 0 doors)))))
  (if (= carLocation 1) (set! doors (cons 0 (cons 1 (cons 0 doors)))))
  (if (= carLocation 2) (set! doors (cons 0 (cons 0 (cons 1 doors)))))
)

;reveals a door to the user
(define (revealDoor)
  (MH-reveal)
  (ChooseRevealDoor)
  )

;chooses which door to be revealed based on the current state of player choice and car location
;not yet implemented
(define (ChooseRevealDoor)
 doors
 )
;--------------------------------------------------------------------------------------



;receiving input from user logic
;--------------------------------------------------------------------------------------
; { assuming sane input | 1 <= playerChoice <= 3 | playerChoice E I }
(define (askPlayerToChooseDoor)
  (MH-intro)
  (display doorsGraphics)(newline)
  (set! playerChoice (read)) 
  )

;--------------------------------------------------------------------------------------


;MontyHall speaking logic
;--------------------------------------------------------------------------------------
(define (MH-intro)
  (display "It is I the great Monty Hall. I have three doors; behind one is a car of your wildest dreams, but behind the other
  two are livestock. Which door would you like?!")(newline)
  )

(define (MH-reveal)
  (display "You choose a door, but let me ask you, if I were to reveal this door...")
  )

;--------------------------------------------------------------------------------------

;calls the main body procedure to get things started
(montyHall)
