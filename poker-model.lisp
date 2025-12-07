(clear-all)

(define-model simple-poker
  
  ;; Chunk types
  (chunk-type poker-hand card1 card2 state)
  (chunk-type card rank suit)
  
  ;; Define state chunks
  (define-chunks 
   (start-hand) (waiting) (attending) (done))
  
  ;; Set model parameters
  (sgp :v t
       :esc t
       :lf 0
       :trace-detail high)
  
  ;; Install keyboard
  (install-device '("motor" "keyboard"))
  
  ;; Productions

  ;; Ignore items outside player area
  (p ignore-non-player-item
     =goal>
       isa poker-hand
       state start-hand
       card1 nil
     =visual-location>
       <= screen-y 350
     ==>
     -visual-location>)

  ;; Request to find first card in player area
  (p find-first-card
     =goal>
       isa poker-hand
       state start-hand
       card1 nil
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       > screen-y 350)

  ;; Attend first card when found
  (p attend-first-card
     =goal>
       isa poker-hand
       state start-hand
       card1 nil
     =visual-location>
       > screen-y 350
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)
  
  ;; Encode first card
  (p encode-first-card
     =goal>
       isa poker-hand
       state attending
       card1 nil
     =visual>
       value =card
     ==>
     =goal>
       card1 =card
       state waiting
     -visual>)
  
  ;; Request to find second card in player area
  (p request-second-card
     =goal>
       isa poker-hand
       state waiting
       card1 =c1
       card2 nil
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       :attended nil
       > screen-y 350
     =goal>
       state start-hand)
  
  ;; Attend second card when found in player area
  (p attend-second-card
     =goal>
       isa poker-hand
       state start-hand
       card1 =c1
       card2 nil
     =visual-location>
       > screen-y 350
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)
  
  ;; Encode second card
  (p encode-second-card
     =goal>
       isa poker-hand
       state attending
       card1 =c1
       card2 nil
     =visual>
       value =card
     ==>
     =goal>
       card2 =card
       state done
     -visual>)
  
  ;; Report hand
  (p report-hand
     =goal>
       isa poker-hand
       state done
       card1 =c1
       card2 =c2
     ?manual>
       state free
     ==>
     !output! (Hand received - =c1 =c2)
     +manual>
       cmd press-key
       key space
     -goal>)
  
  ;; Initial goal
  (goal-focus (isa poker-hand state start-hand card1 nil card2 nil))
)
