(clear-all)

(define-model simple-poker

  ;; Chunk types
  (chunk-type poker-hand
    card1 card2          ; hole cards
    stage                ; preflop, flop, turn, river, done
    board-attended       ; count of board cards attended this stage
    state)               ; current action

  ;; Define state and stage chunks
  (define-chunks
    (start-hand) (attending) (waiting) (find-button) (pressing-key) (done)
    (preflop) (flop) (turn) (river))

  ;; Set model parameters
  (sgp :v t
       :esc t
       :lf 0
       :trace-detail high)

  ;; ============================================
  ;; PREFLOP: Attend to hole cards, then click CALL
  ;; ============================================

  ;; Ignore items outside player area (y <= 350)
  (p ignore-non-player-item
     =goal>
       isa poker-hand
       stage preflop
       state start-hand
       card1 nil
     =visual-location>
       <= screen-y 350
     ==>
     -visual-location>)

  ;; Find first hole card (player cards at y=360, search wide range)
  (p find-first-card
     =goal>
       isa poker-hand
       stage preflop
       state start-hand
       card1 nil
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       screen-y highest)

  ;; Attend first hole card
  (p attend-first-card
     =goal>
       isa poker-hand
       stage preflop
       state start-hand
       card1 nil
     =visual-location>
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)

  ;; Encode first hole card
  (p encode-first-card
     =goal>
       isa poker-hand
       stage preflop
       state attending
       card1 nil
     =visual>
       value =card
     ==>
     !output! (Hole card 1 - =card)
     =goal>
       card1 =card
       state waiting
     -visual>)

  ;; Find second hole card
  (p find-second-card
     =goal>
       isa poker-hand
       stage preflop
       state waiting
       card1 =c1
       card2 nil
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       :attended nil
       screen-y highest)

  ;; Attend second hole card
  (p attend-second-card
     =goal>
       isa poker-hand
       stage preflop
       state waiting
       card1 =c1
       card2 nil
     =visual-location>
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)

  ;; Encode second hole card
  (p encode-second-card
     =goal>
       isa poker-hand
       stage preflop
       state attending
       card1 =c1
       card2 nil
     =visual>
       value =card
     ==>
     !output! (Hole card 2 - =card)
     =goal>
       card2 =card
       state find-button
     -visual>)

  ;; Press C to call
  (p press-call
     =goal>
       isa poker-hand
       state find-button
       stage =stage
     ?manual>
       state free
     ==>
     !output! (Pressing C to CALL at stage =stage)
     +manual>
       cmd press-key
       key "c"
     =goal>
       state pressing-key)

  ;; Wait for keypress to complete before continuing
  (p keypress-complete
     =goal>
       isa poker-hand
       state pressing-key
     ?manual>
       state free
     ==>
     !output! (Keypress complete - waiting for new cards)
     =goal>
       state waiting)

  ;; FLOP: Attend to 3 new board cards (y ~ 220)

  ;; After preflop call, wait for flop cards
  (p wait-for-flop
     =goal>
       isa poker-hand
       stage preflop
       state waiting
       card1 =c1
       card2 =c2
       - card2 nil
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       :attended nil
     =goal>
       stage flop)

  ;; Attend flop card
  (p attend-flop-card
     =goal>
       isa poker-hand
       stage flop
       state waiting
       board-attended =count
     =visual-location>
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)

  ;; Encode flop card (not last one)
  (p encode-flop-card
     =goal>
       isa poker-hand
       stage flop
       state attending
       board-attended =count
     =visual>
       value =card
    !bind! =new-count (1+ =count)
    !eval! (< =new-count 3)
     ==>
     !output! (Flop card - =card)
     =goal>
       board-attended =new-count
       state waiting
     -visual>)

  ;; Encode last flop card (3rd) -> find button
  (p encode-last-flop-card
     =goal>
       isa poker-hand
       stage flop
       state attending
       board-attended =count
     =visual>
       value =card
    !bind! =new-count (1+ =count)
    !eval! (>= =new-count 3)
     ==>
     !output! (Last flop card - =card)
     =goal>
       board-attended =new-count
       state find-button
     -visual>)

  ;; Find next flop card
  (p find-next-flop-card
     =goal>
       isa poker-hand
       stage flop
       state waiting
       board-attended =count
    !eval! (< =count 3)
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       :attended nil)

  ;; After flop call, wait for turn card
  (p wait-for-turn
     =goal>
       isa poker-hand
       stage flop
       state waiting
       board-attended =count
    !eval! (>= =count 3)
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       :attended nil
     =goal>
       stage turn
       board-attended 0)

  ;; Attend turn card
  (p attend-turn-card
     =goal>
       isa poker-hand
       stage turn
       state waiting
       board-attended 0
     =visual-location>
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)

  ;; Encode turn card -> find button
  (p encode-turn-card
     =goal>
       isa poker-hand
       stage turn
       state attending
     =visual>
       value =card
     ==>
     !output! (Turn card - =card)
     =goal>
       board-attended 1
       state find-button
     -visual>)
     
  ;; After turn call, wait for river card
  (p wait-for-river
     =goal>
       isa poker-hand
       stage turn
       state waiting
       board-attended 1
     ?visual-location>
       buffer empty
     ==>
     +visual-location>
       :attended nil
     =goal>
       stage river
       board-attended 0)

  ;; Attend river card
  (p attend-river-card
     =goal>
       isa poker-hand
       stage river
       state waiting
       board-attended 0
     =visual-location>
     ?visual>
       state free
     ==>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)

  ;; Encode river card -> find button
  (p encode-river-card
     =goal>
       isa poker-hand
       stage river
       state attending
     =visual>
       value =card
     ==>
     !output! (River card - =card)
     =goal>
       board-attended 1
       state find-button
     -visual>)

  ;; status DONE: After river call

  (p hand-complete
     =goal>
       isa poker-hand
       stage river
       state waiting
       board-attended 1
     ==>
     !output! (Hand complete - all cards seen)
     =goal>
       stage done
       state done)

  ;; Initial goal
  (goal-focus (isa poker-hand
               stage preflop
               state start-hand
               card1 nil
               card2 nil
               board-attended 0))
)
