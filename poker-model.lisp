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
    opp-action               ; opponent's action this round
    ;; Action history for pattern detection (Improvement #5)
    opp-preflop-action       ; what opponent did preflop
    opp-flop-action          ; what opponent did on flop
    opp-turn-action          ; what opponent did on turn
    my-preflop-action        ; what we did preflop
    my-flop-action           ; what we did on flop
    my-turn-action           ; what we did on turn
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
    stage my-action opp-response)

  (chunk-type hand-outcome
    my-strength my-action result)

  ;; ============================================
  ;; CHUNKS
  ;; ============================================

  (define-chunks
    (waiting) (checking-opponent) (retrieving-hand) (retrieving-opponent) (deciding)
    (pressing-key) (action-done) (attending-showdown) (attending)
    (processing-feedback) (done) (learning-stage)
    (preflop) (flop) (turn) (river) (showdown)
    (premium) (strong) (playable) (marginal) (trash) (exploit) (unknown) (probe)
    (yes) (no)
    (high) (medium) (low)  ;; Hand strength values
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

  ;; IMPROVEMENT #1: Stage-Specific Opponent Modeling
  (p preflop-check-opponent
     =goal>
       isa poker-hand
       state waiting
       stage preflop
     ==>
     !output! (Preflop - checking PREFLOP-SPECIFIC opponent tendencies when WE RAISE)
     =goal>
       state checking-opponent
     +retrieval>
       isa opponent-behavior
       stage preflop              ;; Stage-specific!
       my-action raise-action     ;; What happens when WE raise?
       opp-response fold-action)  ;; Does opponent fold?

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

  ;; Alternative: Defend big blind with weak hands (heads-up standard play)
  ;; This competes with preflop-unknown-weak - noise/utility will decide
  (p preflop-defend-weak
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       strength low
       pot-amount =pot
     !eval! (and (numberp =pot) (>= =pot 30))  ;; Pot has decent odds
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (Hand weak but defending big blind - CALLING)
     =goal>
       state pressing-key
       my-action call-action
       hand-category marginal
     +manual>
       cmd press-key
       key "c")

  ;; ============================================
  ;; POST-FLOP DECISIONS
  ;; ============================================

  ;; IMPROVEMENT #1: Stage-Specific Opponent Modeling
  ;; First try to retrieve stage-specific behavior
  (p postflop-retrieve-opponent-stage-specific
     =goal>
       isa poker-hand
       state waiting
       stage =stage
     - stage preflop
     - stage showdown
     - stage done
     ==>
     !output! (Stage =stage - retrieving STAGE-SPECIFIC opponent behavior)
     =goal>
       state retrieving-opponent
     +retrieval>
       isa opponent-behavior
       stage =stage              ;; Match current stage!
       :recently-retrieved nil
     - opp-response nil)

  (p retrieval-complete
     =goal>
       isa poker-hand
       state retrieving-opponent
     ?retrieval>
       state free
     ==>
     =goal>
       state deciding)

  ;; ============================================
  ;; IMPROVEMENT #5: Action Sequence Pattern Detection
  ;; ============================================
  ;; These detect weakness/strength patterns based on action history

  ;; Pattern: Opponent raised preflop but checked flop = weakness
  ;; They were aggressive preflop but gave up - exploit by raising
  (p pattern-opp-raised-preflop-checked-flop
     =goal>
       isa poker-hand
       state deciding
       stage flop
       opp-preflop-action raise-action
       opp-action call-action          ;; Current action is passive (check = call in our model)
     - strength low                     ;; Don't bluff with nothing
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent raised preflop but passive on flop - ATTACKING weakness)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  ;; Pattern: Opponent called preflop, called flop, now at turn
  ;; They're probably on a draw or weak hand - apply pressure
  (p pattern-opp-called-twice-apply-pressure
     =goal>
       isa poker-hand
       state deciding
       stage turn
       opp-preflop-action call-action
       opp-flop-action call-action
       strength high                    ;; Only do this with strong hands
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent called twice - they likely have draw/weak - RAISING for value)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  ;; Pattern: Opponent raised preflop AND raised flop = very strong
  ;; They're showing aggression - slow down unless we're strong
  (p pattern-opp-raised-twice-strong-range
     =goal>
       isa poker-hand
       state deciding
       stage turn
       opp-preflop-action raise-action
       opp-flop-action raise-action
     - strength high                    ;; We're not strong
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent raised twice - they are strong - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f")

  ;; Pattern: Opponent raised preflop AND raised flop, we are strong = call
  (p pattern-opp-raised-twice-we-strong
     =goal>
       isa poker-hand
       state deciding
       stage turn
       opp-preflop-action raise-action
       opp-flop-action raise-action
       strength high                    ;; We ARE strong
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent raised twice but we are strong - CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c")

  ;; Pattern: We raised preflop and opponent just called = continuation bet territory
  ;; When we showed strength preflop and they were passive, continue the aggression
  (p pattern-we-raised-opp-called-cbet
     =goal>
       isa poker-hand
       state deciding
       stage flop
       my-preflop-action raise-action
       opp-preflop-action call-action
     - strength low                     ;; Have something to back it up
     ?manual>
       state free
     ==>
     !output! (PATTERN - We raised preflop they called - CONTINUATION BET)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  ;; Pattern: River after passive play by opponent = value bet strong hands
  (p pattern-passive-opponent-river-value
     =goal>
       isa poker-hand
       state deciding
       stage river
       opp-preflop-action call-action
       opp-flop-action call-action
       opp-turn-action call-action
       strength high
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent passive all streets we strong - VALUE BET river)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  ;; IMPROVEMENT #2: Chip-Aware Decisions
  ;; Short stack with any playable hand = PUSH (all-in mentality)
  (p short-stack-push-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
       my-chips =chips
     !eval! (and (numberp =chips) (< =chips 200))  ;; < 10 BB
     ?manual>
       state free
     ==>
     !output! (SHORT STACK =chips - PUSHING strong hand)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  (p short-stack-push-medium
     =goal>
       isa poker-hand
       state deciding
       strength medium
       my-chips =chips
     !eval! (and (numberp =chips) (< =chips 150))  ;; < 7.5 BB = very short
     ?manual>
       state free
     ==>
     !output! (VERY SHORT STACK =chips - PUSHING medium hand)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  ;; Opponent short stack = apply pressure (they can't call light)
  (p pressure-short-opponent
     =goal>
       isa poker-hand
       state deciding
       opp-chips =opp
     - strength low
     !eval! (and (numberp =opp) (< =opp 150))  ;; Opponent < 7.5 BB
     ?manual>
       state free
     ==>
     !output! (OPPONENT SHORT =opp - RAISING to pressure)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

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

  ;; IMPROVEMENT #3: Pot Odds Integration
  ;; Big pot = call with medium hands (getting good odds)
  (p decide-no-memory-medium-big-pot
     =goal>
       isa poker-hand
       state deciding
       strength medium
       pot-amount =pot
     !eval! (and (numberp =pot) (> =pot 100))  ;; Pot > 5 BB
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (Big pot =pot - CALLING for odds with medium hand)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c")

  ;; Small pot = can fold medium hands (not worth it)
  (p decide-no-memory-medium-small-pot
     =goal>
       isa poker-hand
       state deciding
       strength medium
       pot-amount =pot
     !eval! (and (numberp =pot) (< =pot 50))  ;; Pot < 2.5 BB
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (Small pot =pot - FOLDING medium hand)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f")

  ;; Default medium hand behavior (moderate pot)
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

  ;; Fallback for hand-outcome chunks retrieved by mistake
  ;; hand-outcome has my-strength slot (not category or opp-response)
  (p retrieved-hand-outcome-chunk-strong
     =goal>
       isa poker-hand
       state deciding
       strength high
     =retrieval>
       isa hand-outcome
       my-strength =any-str  ;; Test a slot to avoid warning
     ?manual>
       state free
     ==>
     !output! (Retrieved hand-outcome chunk - strong hand - RAISING)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p retrieved-hand-outcome-chunk-medium
     =goal>
       isa poker-hand
       state deciding
       strength medium
     =retrieval>
       isa hand-outcome
       my-strength =any-str  ;; Test a slot to avoid warning
     ?manual>
       state free
     ==>
     !output! (Retrieved hand-outcome chunk - medium hand - CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p retrieved-hand-outcome-chunk-weak
     =goal>
       isa poker-hand
       state deciding
       strength low
     =retrieval>
       isa hand-outcome
       my-strength =any-str  ;; Test a slot to avoid warning
     ?manual>
       state free
     ==>
     !output! (Retrieved hand-outcome chunk - weak hand - FOLDING)
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

  ;; IMPROVEMENT #3: Pot-aware decisions vs calling station
  (p opponent-calls-we-medium-big-pot
     =goal>
       isa poker-hand
       state deciding
       strength medium
       pot-amount =pot
     !eval! (and (numberp =pot) (> =pot 100))
     =retrieval>
       isa opponent-behavior
       opp-response call-action
     ?manual>
       state free
     ==>
     !output! (Opponent calls - big pot =pot - RAISING for value)
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

  ;; Big pot with weak hand vs calling station - might call for odds
  (p opponent-calls-we-weak-big-pot
     =goal>
       isa poker-hand
       state deciding
       strength low
       pot-amount =pot
     !eval! (and (numberp =pot) (> =pot 150))  ;; Very big pot
     =retrieval>
       isa opponent-behavior
       opp-response call-action
     ?manual>
       state free
     ==>
     !output! (Opponent calls - big pot =pot - CALLING for odds even with weak hand)
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

  ;; IMPROVEMENT #4: Per-Stage Learning
  ;; Learn opponent behavior after EACH betting round, not just at end of hand
  (p learn-stage-opponent-behavior
     =goal>
       isa poker-hand
       state learning-stage
       stage =stage
       my-action =my-act
       opp-action =opp-act
     - opp-action nil
     ?imaginal>
       state free
     ==>
     !output! (LEARNING at =stage - when I =my-act opponent =opp-act)
     =goal>
       state waiting
     +imaginal>
       isa opponent-behavior
       stage =stage
       my-action =my-act
       opp-response =opp-act)

  (p clear-stage-learning
     =goal>
       isa poker-hand
       state waiting
     =imaginal>
       isa opponent-behavior
       stage =any-stage
     - stage nil
     ==>
     !output! (Stored stage-specific opponent behavior)
     -imaginal>)

  ;; IMPORTANT: Learn opponent-behavior in ALL cases so we can exploit tendencies
  ;; The key insight: we need to learn "when I do X, opponent does Y"

  (p learn-from-win-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       stage =stage
       my-action =my-act
       opp-action =opp-act
       opp-card1 =oc1
       opp-card2 =oc2
     - opp-card1 nil
     - opp-card2 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning - WON at showdown - opponent =opp-act when I =my-act at =stage)
     =goal>
       state done
       learned yes
     ;; Store OPPONENT-BEHAVIOR with stage for stage-specific modeling
     +imaginal>
       isa opponent-behavior
       stage =stage
       my-action =my-act
       opp-response =opp-act)

  (p learn-from-win-no-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       stage =stage
       my-action =my-act
       opp-action =opp-act
       opp-card1 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning - WON opponent folded at =stage when I =my-act)
     =goal>
       state done
       learned yes
     +imaginal>
       isa opponent-behavior
       stage =stage
       my-action =my-act
       opp-response =opp-act)

  (p learn-from-loss-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       stage =stage
       my-action =my-act
       opp-action =opp-act
       opp-card1 =oc1
       opp-card2 =oc2
     - opp-card1 nil
     - opp-card2 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning - LOST at showdown - opponent =opp-act when I =my-act at =stage)
     =goal>
       state done
       learned yes
     ;; Store OPPONENT-BEHAVIOR with stage
     +imaginal>
       isa opponent-behavior
       stage =stage
       my-action =my-act
       opp-response =opp-act)

  ;; FIXED: Added '- my-action fold-action' and 'learned nil' to prevent double learning
  (p learn-from-loss-no-showdown
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       stage =stage
       my-action =my-act
       opp-action =opp-act
       opp-card1 nil
     - my-action fold-action
     ?imaginal>
       state free
     ==>
     !output! (Learning - LOST at =stage)
     =goal>
       state done
       learned yes
     +imaginal>
       isa opponent-behavior
       stage =stage
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
               ;; Action history (Improvement #5)
               opp-preflop-action nil opp-flop-action nil opp-turn-action nil
               my-preflop-action nil my-flop-action nil my-turn-action nil
               opp-card1 nil opp-card2 nil
               opp-cards-attended 0
               result nil
               learned nil
               my-chips nil opp-chips nil pot-amount nil))
)