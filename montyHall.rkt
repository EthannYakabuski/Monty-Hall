(#%require (only racket/base random))


(define doors '())
(define playerChoice 0)
(define playerSwitchChoice -1)
(define carLocation 0)
(define goatLocation1 0)
(define goatLocation2 0)
(define goatToShow 0)
(define doorsGraphics (cons '(1) (cons '(2) (cons '(3) '()))))

;main body
(define (montyHall)
   (setDoors)
   (askPlayerToChooseDoor)
   (revealDoor)
   (switchDoors?)
  )


;filling/showing doors logic
;----------------------------------------------------------------------------------------------------------
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
  (setGoatLocations)
)

(define (setGoatLocations)
  (if (= carLocation 0) (setGoats0))
  (if (= carLocation 1) (setGoats1))
  (if (= carLocation 2) (setGoats2))
  )

(define (setGoats0)
  (set! goatLocation1 1)
  (set! goatLocation2 2)
  )

(define (setGoats1)
  (set! goatLocation1 0)
  (set! goatLocation2 2)
  )

(define (setGoats2)
  (set! goatLocation1 0)
  (set! goatLocation2 1)
  )

;reveals a door to the user
(define (revealDoor)
  (MH-reveal)
  (ChooseRevealDoor)
  )

;chooses which door to be revealed based on the current state of player choice and car location
;not yet implemented
(define (ChooseRevealDoor)
   (set! goatToShow (random 2))

   (if (= goatToShow 0) (showGoat1))
   (if (= goatToShow 1) (showGoat2))
 )

(define (showGoat1)
  (cond ( (= (- playerChoice 1) goatLocation1) (showGoat2))
        ( (= 1 1) (setGraphics goatLocation1))))

(define (showGoat2)
  (cond ( (= (- playerChoice 1) goatLocation2) (showGoat1))
        ( (= 1 1) (setGraphics goatLocation2)))
  )

(define (setGraphics x)
  (if (= x 0) (set! doorsGraphics (cons 'Goat (cons '(2) (cons '(3) '())))))
  (if (= x 1) (set! doorsGraphics (cons '(1) (cons 'Goat (cons '(3) '())))))
  (if (= x 2) (set! doorsGraphics (cons '(1) (cons '(2) (cons 'Goat '())))))

  (display doorsGraphics)(newline)
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

(define (switchDoors?)
  (MH-switch)
  (set! playerSwitchChoice (read))
  (if (= playerSwitchChoice 1) (switchDoors))
  )

(define (switchDoors)
  (cond ( (= (- playerChoice 1) carLocation) (specialCaseSwitchDoor))
        ( (= playerChoice 1)
              (if (= carLocation 1) (set! playerChoice 2))
              (if (= carLocation 2) (set! playerChoice 3)))
        ( (= playerChoice 2)
              (if (= carLocation 0) (set! playerChoice 1))
              (if (= carLocation 2) (set! playerChoice 3)))
        ( (= playerChoice 3)
              (if (= carLocation 0) (set! playerChoice 1))
              (if (= carLocation 1) (set! playerChoice 2)))
      
        )
  )

(define (specialCaseSwitchDoor)

  )
  
;--------------------------------------------------------------------------------------


;MontyHall speaking logic
;--------------------------------------------------------------------------------------
(define (MH-intro)
  (display "It is I the great Monty Hall. I have three doors; behind one is a car of your wildest dreams, but behind the other
  two are livestock. Which door would you like?!")(newline)
  )

(define (MH-reveal)
  (display "You choose a door, but let me ask you, if I were to reveal this door...")(newline)
  )

(define (MH-switch)
  (display "Would you want to switch doors? (1) - Yes  /  (2) - No ")(newline)
  )
;--------------------------------------------------------------------------------------

;calls the main body procedure to get things started
(montyHall)
