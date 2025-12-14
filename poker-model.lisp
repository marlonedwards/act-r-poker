(clear-all)

(define-model simple-poker

  ;; Model parameters
  (sgp :esc t :bll 0.5 :ol t :er t :lf 0)
  (sgp :v t :ans 0.2 :mp 10.0 :rt -60)
  (sgp :trace-detail low)
  (sgp :show-focus nil)

  ;; ============================================
  ;; CHUNK TYPES
  ;; ============================================

  (chunk-type poker-hand
    card1 card2              ; hole cards
    rank1 rank2              ; numeric ranks 2-14
    suited                   ; yes/no
    stage                    ; preflop, flop, turn, river, done, showdown
    state                    ; current processing state
    strength                 ; hand strength: high, medium, low
    hand-category            ; premium, strong, playable, marginal, trash
    my-action                ; player's action this round
    opp-action               ; opponent's action
    opp-card1 opp-card2      ; opponent's revealed cards
    opp-cards-attended       ; count of opponent cards attended
    result                   ; win, lose, fold
    learned                  ; flag to prevent double learning
    my-chips opp-chips pot-amount)

  (chunk-type starting-hand
    rank1                    ; high card rank 
    rank2                    ; low card rank
    suited                   ; yes or no
    category                 ; premium, strong, playable, marginal, trash
    action)                  ; recommended action

  (chunk-type opponent-behavior
    stage my-action opp-response frequency)

  (chunk-type hand-outcome
    my-strength my-action result)

  (chunk-type showdown-memory
    opp-card1 opp-card2 opp-action result action-sequence)

  ;; ============================================
  ;; CHUNKS
  ;; ============================================

  (define-chunks
    (waiting) (checking-opponent) (retrieving-hand) (retrieving-opponent) (deciding)
    (pressing-key) (action-done) (attending-showdown) (attending)
    (processing-feedback) (done)
    (preflop) (flop) (turn) (river) (showdown)
    (premium) (strong) (playable) (marginal) (trash) (exploit) (unknown) (probe)
    (yes) (no)
    (medium)
    (fold-action) (call-action) (raise-action)
    (win) (lose) (tie)
    ;; Pre-define card chunks to avoid "Creating chunk with no slots" warnings
    ;; Hearts
    (2H) (3H) (4H) (5H) (6H) (7H) (8H) (9H) (TH) (JH) (QH) (KH) (AH)
    ;; Diamonds
    (2D) (3D) (4D) (5D) (6D) (7D) (8D) (9D) (TD) (JD) (QD) (KD) (AD)
    ;; Clubs
    (2C) (3C) (4C) (5C) (6C) (7C) (8C) (9C) (TC) (JC) (QC) (KC) (AC)
    ;; Spades
    (2S) (3S) (4S) (5S) (6S) (7S) (8S) (9S) (TS) (JS) (QS) (KS) (AS))

  (declare-buffer-usage goal poker-hand :all)
  (declare-buffer-usage imaginal showdown-memory :all)
  (declare-buffer-usage imaginal opponent-behavior :all)
  (declare-buffer-usage imaginal hand-outcome :all)
  (install-device '("motor" "keyboard"))

  ;; ============================================
  ;; PREFLOP HAND RANKINGS
  ;; ============================================

  (add-dm
    ;; === PREMIUM HANDS ===
    (hand-AA isa starting-hand rank1 14 rank2 14 suited no category premium action raise-action)
    (hand-KK isa starting-hand rank1 13 rank2 13 suited no category premium action raise-action)
    (hand-QQ isa starting-hand rank1 12 rank2 12 suited no category premium action raise-action)
    (hand-JJ isa starting-hand rank1 11 rank2 11 suited no category premium action raise-action)
    (hand-TT isa starting-hand rank1 10 rank2 10 suited no category premium action raise-action)
    (hand-AKs isa starting-hand rank1 14 rank2 13 suited yes category premium action raise-action)
    (hand-AKo isa starting-hand rank1 14 rank2 13 suited no category premium action raise-action)
    (hand-AQs isa starting-hand rank1 14 rank2 12 suited yes category premium action raise-action)

    ;; === STRONG HANDS ===
    (hand-AQo isa starting-hand rank1 14 rank2 12 suited no category strong action raise-action)
    (hand-AJs isa starting-hand rank1 14 rank2 11 suited yes category strong action raise-action)
    (hand-ATs isa starting-hand rank1 14 rank2 10 suited yes category strong action raise-action)
    (hand-KQs isa starting-hand rank1 13 rank2 12 suited yes category strong action raise-action)
    (hand-99 isa starting-hand rank1 9 rank2 9 suited no category strong action raise-action)
    (hand-88 isa starting-hand rank1 8 rank2 8 suited no category strong action raise-action)
    (hand-KJs isa starting-hand rank1 13 rank2 11 suited yes category strong action call-action)
    (hand-QJs isa starting-hand rank1 12 rank2 11 suited yes category strong action call-action)

    ;; === PLAYABLE HANDS ===
    (hand-AJo isa starting-hand rank1 14 rank2 11 suited no category playable action call-action)
    (hand-ATo isa starting-hand rank1 14 rank2 10 suited no category playable action call-action)
    (hand-KQo isa starting-hand rank1 13 rank2 12 suited no category playable action call-action)
    (hand-KJo isa starting-hand rank1 13 rank2 11 suited no category playable action call-action)
    (hand-QJo isa starting-hand rank1 12 rank2 11 suited no category playable action call-action)
    (hand-JTs isa starting-hand rank1 11 rank2 10 suited yes category playable action call-action)
    (hand-T9s isa starting-hand rank1 10 rank2 9 suited yes category playable action call-action)
    (hand-77 isa starting-hand rank1 7 rank2 7 suited no category playable action call-action)
    (hand-66 isa starting-hand rank1 6 rank2 6 suited no category playable action call-action)
    (hand-55 isa starting-hand rank1 5 rank2 5 suited no category playable action call-action)
    (hand-A9s isa starting-hand rank1 14 rank2 9 suited yes category playable action call-action)
    (hand-A8s isa starting-hand rank1 14 rank2 8 suited yes category playable action call-action)
    (hand-A7s isa starting-hand rank1 14 rank2 7 suited yes category playable action call-action)
    (hand-A6s isa starting-hand rank1 14 rank2 6 suited yes category playable action call-action)
    (hand-A5s isa starting-hand rank1 14 rank2 5 suited yes category playable action call-action)
    (hand-A4s isa starting-hand rank1 14 rank2 4 suited yes category playable action call-action)
    (hand-A3s isa starting-hand rank1 14 rank2 3 suited yes category playable action call-action)
    (hand-A2s isa starting-hand rank1 14 rank2 2 suited yes category playable action call-action)

    ;; === MARGINAL HANDS ===
    (hand-44 isa starting-hand rank1 4 rank2 4 suited no category marginal action call-action)
    (hand-33 isa starting-hand rank1 3 rank2 3 suited no category marginal action call-action)
    (hand-22 isa starting-hand rank1 2 rank2 2 suited no category marginal action call-action)
    (hand-KTo isa starting-hand rank1 13 rank2 10 suited no category marginal action call-action)
    (hand-QTo isa starting-hand rank1 12 rank2 10 suited no category marginal action call-action)
    (hand-JTo isa starting-hand rank1 11 rank2 10 suited no category marginal action call-action)
    (hand-98s isa starting-hand rank1 9 rank2 8 suited yes category marginal action call-action)
    (hand-87s isa starting-hand rank1 8 rank2 7 suited yes category marginal action call-action)
    (hand-76s isa starting-hand rank1 7 rank2 6 suited yes category marginal action call-action)
    (hand-65s isa starting-hand rank1 6 rank2 5 suited yes category marginal action call-action)
    (hand-54s isa starting-hand rank1 5 rank2 4 suited yes category marginal action call-action)
    )

  ;; ============================================
  ;; PREFLOP LOGIC
  ;; ============================================

  (p preflop-check-opponent
     =goal>
       isa poker-hand
       state waiting
       stage preflop
     ==>
     !output! (Preflop - checking opponent tendencies when WE RAISE)
     =goal>
       state checking-opponent
     +retrieval>
       isa opponent-behavior
       my-action raise-action      ;; What happens when WE raise?
       opp-response fold-action)   ;; Does opponent fold?

  (p preflop-exploit-folder
     =goal>
       isa poker-hand
       state checking-opponent
       stage preflop
     =retrieval>
       isa opponent-behavior
       opp-response fold-action
     ?manual>
       state free
     ==>
     !output! (Opponent tends to fold - RAISING to exploit!)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category exploit
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  ;; When we have NO memory of raising, probe with strong/medium hands to learn
  (p preflop-probe-raise-strong
     =goal>
       isa poker-hand
       state checking-opponent
       stage preflop
       strength high
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (No opponent memory - PROBING with raise on strong hand)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category probe
     +manual>
       cmd press-key
       key "r")

  (p preflop-probe-raise-medium
     =goal>
       isa poker-hand
       state checking-opponent
       stage preflop
       strength medium
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (No opponent memory - PROBING with raise on medium hand)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category probe
     +manual>
       cmd press-key
       key "r")

  (p preflop-no-exploit-check-hand
     =goal>
       isa poker-hand
       state checking-opponent
       stage preflop
       strength low
       rank1 =r1
       rank2 =r2
       suited =s
     ?retrieval>
       buffer failure
     ==>
     !output! (No exploitable opponent and weak hand - looking up hand =r1 =r2 suited =s)
     =goal>
       state retrieving-hand
     +retrieval>
       isa starting-hand
       rank1 =r1
       rank2 =r2
       suited =s)

  (p preflop-opponent-doesnt-fold
     =goal>
       isa poker-hand
       state checking-opponent
       stage preflop
       rank1 =r1
       rank2 =r2
       suited =s
     =retrieval>
       isa opponent-behavior
     - opp-response fold-action
     ==>
     !output! (Opponent doesnt fold - looking up hand =r1 =r2 suited =s)
     =goal>
       state retrieving-hand
     +retrieval>
       isa starting-hand
       rank1 =r1
       rank2 =r2
       suited =s)

  (p preflop-wrong-chunk-check-hand
     =goal>
       isa poker-hand
       state checking-opponent
       stage preflop
       rank1 =r1
       rank2 =r2
       suited =s
     =retrieval>
       category =any-cat
     ==>
     !output! (Retrieved starting-hand chunk - looking up hand =r1 =r2 suited =s)
     =goal>
       state retrieving-hand
     -retrieval>
     +retrieval>
       isa starting-hand
       rank1 =r1
       rank2 =r2
       suited =s)

  (p preflop-hand-found-raise
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       rank2 =r2
       suited =s
     =retrieval>
       isa starting-hand
       rank1 =r1
       rank2 =r2
       suited =s
       category =cat
       action raise-action
     ?manual>
       state free
     ==>
     !output! (Hand is =cat - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category =cat
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p preflop-hand-found-call
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       rank2 =r2
       suited =s
     =retrieval>
       isa starting-hand
       rank1 =r1
       rank2 =r2
       suited =s
       category =cat
       action call-action
     ?manual>
       state free
     ==>
     !output! (Hand is =cat - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       hand-category =cat
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; === Fail-safes for Partial Matching ===

  (p preflop-wrong-hand-strong
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       strength high
     =retrieval>
       isa starting-hand
     - rank1 =r1
     ?manual>
       state free
     ==>
     !output! (Hand not in database but strong - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category unknown
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p preflop-wrong-hand-medium
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       strength medium
     =retrieval>
       isa starting-hand
     - rank1 =r1
     ?manual>
       state free
     ==>
     !output! (Hand not in database but medium - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       hand-category unknown
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p preflop-wrong-hand-weak
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       strength low
     =retrieval>
       isa starting-hand
     - rank1 =r1
     ?manual>
       state free
     ==>
     !output! (Hand not in database and weak - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
       hand-category trash
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  (p preflop-wrong-hand-2-strong
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       rank2 =r2
       strength high
     =retrieval>
       isa starting-hand
       rank1 =r1
     - rank2 =r2
     ?manual>
       state free
     ==>
     !output! (Hand not in database but strong - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category unknown
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p preflop-wrong-hand-2-medium
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       rank2 =r2
       strength medium
     =retrieval>
       isa starting-hand
       rank1 =r1
     - rank2 =r2
     ?manual>
       state free
     ==>
     !output! (Hand not in database but medium - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       hand-category unknown
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p preflop-wrong-hand-2-weak
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       rank1 =r1
       rank2 =r2
       strength low
     =retrieval>
       isa starting-hand
       rank1 =r1
     - rank2 =r2
     ?manual>
       state free
     ==>
     !output! (Hand not in database and weak - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
       hand-category trash
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  (p preflop-unknown-strong
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       strength high
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (Hand not recognized but strong - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category unknown
     +manual>
       cmd press-key
       key "r")

  (p preflop-unknown-medium
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       strength medium
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (Hand not recognized but medium - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       hand-category unknown
     +manual>
       cmd press-key
       key "c")

  (p preflop-unknown-weak
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       strength low
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (Hand not recognized and weak - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
       hand-category trash
     +manual>
       cmd press-key
       key "f")

  ;; ============================================
  ;; POST-FLOP DECISIONS
  ;; ============================================

  (p postflop-retrieve-opponent
     =goal>
       isa poker-hand
       state waiting
       stage =stage
     - stage preflop
     - stage showdown
     - stage done
     ==>
     !output! (Stage =stage - retrieving opponent behavior)
     =goal>
       state retrieving-opponent
     +retrieval>
       isa opponent-behavior
       :recently-retrieved nil   ;; Don't retrieve same chunk twice
     - opp-response nil)         ;; Must have actual opponent response

  (p retrieval-complete
     =goal>
       isa poker-hand
       state retrieving-opponent
     ?retrieval>
       state free
     ==>
     =goal>
       state deciding)

  (p decide-no-memory-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (No opponent memory - strong hand - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  (p decide-no-memory-medium
     =goal>
       isa poker-hand
       state deciding
       strength medium
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (No opponent memory - medium hand - CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c")

  (p decide-no-memory-weak
     =goal>
       isa poker-hand
       state deciding
       strength low
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (No opponent memory - weak hand - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f")

  (p retrieved-wrong-chunk-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
     =retrieval>
       category =any-cat
     ?manual>
       state free
     ==>
     !output! (Retrieved starting-hand chunk - strong hand - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p retrieved-wrong-chunk-medium
     =goal>
       isa poker-hand
       state deciding
       strength medium
     =retrieval>
       category =any-cat
     ?manual>
       state free
     ==>
     !output! (Retrieved starting-hand chunk - medium hand - CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p retrieved-wrong-chunk-weak
     =goal>
       isa poker-hand
       state deciding
       strength low
     =retrieval>
       category =any-cat
     ?manual>
       state free
     ==>
     !output! (Retrieved starting-hand chunk - weak hand - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; Fallback for any other wrong chunk type (imaginal chunks, etc.)
  (p retrieved-unknown-chunk-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
     =retrieval>
     - category =any   ;; Not a starting-hand
     - opp-response =any   ;; Not an opponent-behavior
     ?manual>
       state free
     ==>
     !output! (Retrieved unknown chunk type - strong hand - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p retrieved-unknown-chunk-medium
     =goal>
       isa poker-hand
       state deciding
       strength medium
     =retrieval>
     - category =any
     - opp-response =any
     ?manual>
       state free
     ==>
     !output! (Retrieved unknown chunk type - medium hand - CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p retrieved-unknown-chunk-weak
     =goal>
       isa poker-hand
       state deciding
       strength low
     =retrieval>
     - category =any
     - opp-response =any
     ?manual>
       state free
     ==>
     !output! (Retrieved unknown chunk type - weak hand - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  (p opponent-calls-we-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
     =retrieval>
       isa opponent-behavior
       opp-response call-action
     ?manual>
       state free
     ==>
     !output! (Opponent calls - we are strong - RAISING for value)
     =goal>
       state pressing-key
       my-action raise-action
       opp-action call-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p opponent-calls-we-medium
     =goal>
       isa poker-hand
       state deciding
       strength medium
     =retrieval>
       isa opponent-behavior
       opp-response call-action
     ?manual>
       state free
     ==>
     !output! (Opponent calls - we are medium - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       opp-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p opponent-calls-we-weak
     =goal>
       isa poker-hand
       state deciding
       strength low
     =retrieval>
       isa opponent-behavior
       opp-response call-action
     ?manual>
       state free
     ==>
     !output! (Opponent calls - we are weak - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
       opp-action call-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  (p opponent-folds-exploit
     =goal>
       isa poker-hand
       state deciding
     =retrieval>
       isa opponent-behavior
       opp-response fold-action
     ?manual>
       state free
     ==>
     !output! (Opponent tends to fold - RAISING to exploit)
     =goal>
       state pressing-key
       my-action raise-action
       opp-action fold-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p opponent-raises-we-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
     =retrieval>
       isa opponent-behavior
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (Opponent raises - we are strong - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       opp-action raise-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p opponent-raises-we-not-strong
     =goal>
       isa poker-hand
       state deciding
     - strength high
     =retrieval>
       isa opponent-behavior
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (Opponent raises - we are not strong - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
       opp-action raise-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; ============================================
  ;; ACTION COMPLETION
  ;; ============================================

  (p keypress-complete
     =goal>
       isa poker-hand
       state pressing-key
       stage =stage
     - stage showdown
     - stage done
     - my-action fold-action
     ?manual>
       state free
     ==>
     !output! (Action complete at =stage)
     =goal>
       state action-done)

  (p fold-complete
     =goal>
       isa poker-hand
       state pressing-key
       my-action fold-action
     ?manual>
       state free
     ==>
     !output! (Folded - hand complete)
     =goal>
       stage done
       state processing-feedback
       result lose)

  ;; ============================================
  ;; SHOWDOWN
  ;; ============================================

  (p showdown-start
     =goal>
       isa poker-hand
       stage showdown
       state waiting
       opp-card1 nil
     ==>
     !output! (Showdown - looking for opponent cards)
     =goal>
       state attending-showdown
       opp-cards-attended 0
     +visual-location>
       screen-y lowest
       :attended nil)

  (p attend-opponent-card
     =goal>
       isa poker-hand
       stage showdown
       state attending-showdown
       opp-cards-attended =count
     !eval! (< =count 2)
     =visual-location>
     ?visual>
       state free
     ==>
     !output! (Attending to opponent card)
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
       state attending)

  (p encode-opponent-card-1
     =goal>
       isa poker-hand
       stage showdown
       state attending
       opp-cards-attended 0
       opp-card1 nil
     =visual>
       value =card
     ==>
     !output! (Opponent card 1 - =card)
     =goal>
       opp-card1 =card
       opp-cards-attended 1
       state attending-showdown
     -visual>
     +visual-location>
       :attended nil
       screen-y lowest)

  (p encode-opponent-card-2
     =goal>
       isa poker-hand
       stage showdown
       state attending
       opp-cards-attended 1
       opp-card1 =oc1
     - opp-card1 nil
       opp-card2 nil
     =visual>
       value =card
     ==>
     !output! (Opponent card 2 - =card)
     =goal>
       opp-card2 =card
       opp-cards-attended 2
       stage done
       state processing-feedback
     -visual>)

  (p no-opponent-cards-visible
     =goal>
       isa poker-hand
       stage showdown
       state attending-showdown
     ?visual-location>
       buffer failure
     ==>
     !output! (No opponent cards visible)
     =goal>
       stage done
       state processing-feedback)

  ;; ============================================
  ;; LEARNING (FIXED)
  ;; ============================================

  ;; IMPORTANT: Learn opponent-behavior in ALL cases so we can exploit tendencies
  ;; The key insight: we need to learn "when I do X, opponent does Y"

  (p learn-from-win-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       my-action =my-act
       opp-action =opp-act
       opp-card1 =oc1
       opp-card2 =oc2
     - opp-card1 nil
     - opp-card2 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning - WON at showdown - opponent =opp-act when I =my-act)
     =goal>
       state done
       learned yes
     ;; Store OPPONENT-BEHAVIOR so we learn their tendencies for exploitation
     +imaginal>
       isa opponent-behavior
       my-action =my-act
       opp-response =opp-act)

  (p learn-from-win-no-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       my-action =my-act
       opp-action =opp-act
       opp-card1 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning - WON opponent folded)
     =goal>
       state done
       learned yes
     +imaginal>
       isa opponent-behavior
       my-action =my-act
       opp-response =opp-act)

  (p learn-from-loss-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       my-action =my-act
       opp-action =opp-act
       opp-card1 =oc1
       opp-card2 =oc2
     - opp-card1 nil
     - opp-card2 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning - LOST at showdown - opponent =opp-act when I =my-act)
     =goal>
       state done
       learned yes
     ;; Store OPPONENT-BEHAVIOR so we learn their tendencies for exploitation
     +imaginal>
       isa opponent-behavior
       my-action =my-act
       opp-response =opp-act)

  ;; FIXED: Added '- my-action fold-action' and 'learned nil' to prevent double learning
  (p learn-from-loss-no-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       my-action =my-act
       opp-action =opp-act
       opp-card1 nil
     - my-action fold-action
     ?imaginal>
       state free
     ==>
     !output! (Learning - LOST)
     =goal>
       state done
       learned yes
     +imaginal>
       isa opponent-behavior
       my-action =my-act
       opp-response =opp-act)

  (p learn-from-fold-weak
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       my-action fold-action
       hand-category trash
     ?imaginal>
       state free
     ==>
     !output! (Learning - Good fold with trash hand)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-outcome
       my-strength low
       my-action fold-action
       result win)

  ;; Catch-all for folds with non-trash hands (marginal/unknown)
  (p learn-from-fold-other
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       my-action fold-action
       hand-category =cat
     - hand-category trash
     ?imaginal>
       state free
     ==>
     !output! (Learning - Folded with =cat hand)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-outcome
       my-strength medium
       my-action fold-action
       result lose)

  ;; CLEAR productions - each tests for a UNIQUE slot that only exists in that chunk type
  ;; This prevents partial matching from causing the wrong production to fire

  (p clear-showdown-memory
     =goal>
       isa poker-hand
       state done
     =imaginal>
       isa showdown-memory
       opp-card1 =oc1      ;; Only showdown-memory has opp-card1
     ==>
     !output! (Stored showdown memory)
     -imaginal>)

  (p clear-opponent-behavior
     =goal>
       isa poker-hand
       state done
     =imaginal>
       isa opponent-behavior
       opp-response =any-response   ;; Only opponent-behavior has opp-response
     ==>
     !output! (Stored opponent behavior)
     -imaginal>)

  (p clear-hand-outcome
     =goal>
       isa poker-hand
       state done
     =imaginal>
       isa hand-outcome
       my-strength =any-strength    ;; Only hand-outcome has my-strength
     ==>
     !output! (Stored hand outcome)
     -imaginal>)

  ;; ============================================
  ;; INITIAL GOAL
  ;; ============================================

  (goal-focus (isa poker-hand
               stage preflop
               state waiting
               rank1 nil rank2 nil suited nil
               strength nil hand-category nil
               my-action nil opp-action nil
               opp-card1 nil opp-card2 nil
               opp-cards-attended 0
               result nil
               learned nil
               my-chips nil opp-chips nil pot-amount nil))
)