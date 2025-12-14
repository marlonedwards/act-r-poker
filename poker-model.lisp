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
    opp-rank1 opp-rank2      ; opponent's card ranks (for memory)
    opp-suited               ; opponent's cards suited?
    opp-cards-attended       ; count of opponent cards attended
    result                   ; win, lose, fold
    learned                  ; flag to prevent double learning (opponent-behavior)
    learned-outcome          ; flag to prevent double hand-outcome learning
    learned-showdown         ; flag to prevent double opponent-showdown learning
    ;; Position tracking
    position                 ; sb (small blind) or bb (big blind)
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
    stage                    ; what stage this happened
    my-strength              ; our hand strength
    my-action                ; what we did
    opp-action               ; what opponent did
    result)                  ; outcome: win, lose, tie

  ;; Opponent showdown memory - remember what hands they show down
  (chunk-type opponent-showdown
    rank1 rank2              ; opponent's hole card ranks
    suited                   ; yes/no
    stage                    ; how far the hand went
    opp-action-pattern       ; were they aggressive or passive?
    result)                  ; did they win or lose?

  ;; ============================================
  ;; CHUNKS
  ;; ============================================

  (define-chunks
    (waiting) (checking-opponent) (retrieving-hand) (retrieving-opponent) (deciding)
    (pressing-key) (action-done) (attending-showdown) (attending)
    (processing-feedback) (done) (learning-stage) (checking-history)
    (preflop) (flop) (turn) (river) (showdown)
    (premium) (strong) (playable) (marginal) (trash) (exploit) (unknown) (probe)
    (yes) (no)
    (strength-high) (strength-medium) (strength-low)  ;; Hand strength values
    (fold-action) (call-action) (raise-action)
    (win) (lose) (tie)
    (sb) (bb)              ;; Position values: small blind, big blind
    (aggressive) (passive) ;; Opponent action patterns
    ;; Pre-define card chunks to avoid "Creating chunk with no slots" warnings
    ;; Hearts
    (2H) (3H) (4H) (5H) (6H) (7H) (8H) (9H) (TH) (JH) (QH) (KH) (AH)
    ;; Diamonds
    (2D) (3D) (4D) (5D) (6D) (7D) (8D) (9D) (TD) (JD) (QD) (KD) (AD)
    ;; Clubs
    (2C) (3C) (4C) (5C) (6C) (7C) (8C) (9C) (TC) (JC) (QC) (KC) (AC)
    ;; Spades
    (2S) (3S) (4S) (5S) (6S) (7S) (8S) (9S) (TS) (JS) (QS) (KS) (AS))

  ;; Buffer usage declarations - using :all since clear productions now test all slots
  (declare-buffer-usage goal poker-hand :all)
  (declare-buffer-usage imaginal opponent-behavior :all)
  (declare-buffer-usage imaginal hand-outcome :all)
  (declare-buffer-usage imaginal opponent-showdown :all)
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-low
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
  ;; POSITION-AWARE PREFLOP DECISIONS
  ;; ============================================
  ;; SB (small blind) acts first preflop - should be tighter
  ;; BB (big blind) acts last preflop - can defend wider

  ;; SB with marginal hand = fold (out of position, tighter range)
  (p preflop-sb-fold-marginal
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       position sb
       strength strength-low
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (SB position with weak hand - FOLDING out of position)
     =goal>
       state pressing-key
       my-action fold-action
       hand-category trash
     +manual>
       cmd press-key
       key "f")

  ;; SB with medium hand = raise or fold (don't limp from SB)
  (p preflop-sb-raise-medium
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       position sb
       strength strength-medium
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (SB position with medium hand - RAISING to take initiative)
     =goal>
       state pressing-key
       my-action raise-action
       hand-category playable
     +manual>
       cmd press-key
       key "r")

  ;; BB defend with wider range (already invested, closing the action)
  (p preflop-bb-defend-medium
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       position bb
       strength strength-medium
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (BB position with medium hand - CALLING to defend blind)
     =goal>
       state pressing-key
       my-action call-action
       hand-category playable
     +manual>
       cmd press-key
       key "c")

  ;; BB can defend weak hands with good pot odds
  (p preflop-bb-defend-weak
     =goal>
       isa poker-hand
       state retrieving-hand
       stage preflop
       position bb
       strength strength-low
       pot-amount =pot
     !eval! (and (numberp =pot) (>= =pot 30))
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (BB defending weak hand with pot odds =pot)
     =goal>
       state pressing-key
       my-action call-action
       hand-category marginal
     +manual>
       cmd press-key
       key "c")

  ;; ============================================
  ;; HAND-OUTCOME HISTORY CHECK
  ;; ============================================
  ;; Before making medium-strength decisions, check past outcomes

  ;; When deciding with medium hand, first check our history
  (p check-medium-hand-history
     =goal>
       isa poker-hand
       state deciding
       strength strength-medium
       stage =stage
     - stage preflop
     ?retrieval>
       buffer empty
     ==>
     !output! (Medium hand - checking outcome history for =stage)
     =goal>
       state checking-history
     +retrieval>
       isa hand-outcome
       stage =stage
       my-strength strength-medium
     - result nil)

  ;; History shows calling medium hands wins - call
  (p history-medium-call-wins
     =goal>
       isa poker-hand
       state checking-history
       strength strength-medium
     =retrieval>
       isa hand-outcome
       my-strength strength-medium
       my-action call-action
       result win
     ?manual>
       state free
     ==>
     !output! (History - calling medium hands has won - CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; History shows folding medium hands was right - fold
  (p history-medium-fold-wins
     =goal>
       isa poker-hand
       state checking-history
       strength strength-medium
     =retrieval>
       isa hand-outcome
       my-strength strength-medium
       my-action fold-action
       result win
     ?manual>
       state free
     ==>
     !output! (History - folding medium hands was correct - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; History shows raising medium hands loses - be cautious, call
  (p history-medium-raise-loses
     =goal>
       isa poker-hand
       state checking-history
       strength strength-medium
     =retrieval>
       isa hand-outcome
       my-strength strength-medium
       my-action raise-action
       result lose
     ?manual>
       state free
     ==>
     !output! (History - raising medium hands lost - being cautious CALLING)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; No history found - fall back to default deciding state
  (p history-check-failed
     =goal>
       isa poker-hand
       state checking-history
     ?retrieval>
       buffer failure
     ==>
     !output! (No relevant history found - using default strategy)
     =goal>
       state deciding)

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
  ;; CALLING STATION EXPLOITATION
  ;; ============================================
  ;; When opponent ONLY calls (never raises, never folds), adjust strategy:
  ;; - Value bet more with medium+ hands (they call with worse)
  ;; - Don't bluff (they never fold)
  ;; - Don't fold weak hands postflop (no threat, free showdowns)

  ;; Against passive opponent (only calls), value bet medium hands
  (p calling-station-value-bet-medium
     =goal>
       isa poker-hand
       state deciding
       strength strength-medium
       stage =stage
     - stage preflop
       opp-preflop-action call-action   ;; They just called preflop
     =retrieval>
       isa opponent-behavior
       opp-response call-action         ;; And they call now too
     ?manual>
       state free
     ==>
     !output! (EXPLOIT - Opponent is passive caller - VALUE BETTING medium hand)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  ;; Against passive opponent, don't fold weak hands postflop - check it down
  (p calling-station-check-down-weak
     =goal>
       isa poker-hand
       state deciding
       strength strength-low
       stage =stage
     - stage preflop
       opp-preflop-action call-action   ;; Opponent passive preflop
     =retrieval>
       isa opponent-behavior
       opp-response call-action         ;; And they call now too
     ?manual>
       state free
     ==>
     !output! (EXPLOIT - Opponent is passive caller - CHECKING DOWN weak hand for showdown)
     =goal>
       state pressing-key
       my-action call-action            ;; Check = call in our model
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; ============================================
  ;; AGGRESSIVE OPPONENT EXPLOITATION
  ;; ============================================
  ;; When opponent raises frequently with random hands, adjust strategy:
  ;; - Call raises with medium hands (their raises don't mean strength)
  ;; - Don't fold to aggression unless truly weak

  ;; Against constantly raising opponent, call down with medium hands
  (p maniac-call-down-medium
     =goal>
       isa poker-hand
       state deciding
       strength strength-medium
       stage =stage
     - stage preflop
       opp-preflop-action raise-action  ;; They raised preflop
     =retrieval>
       isa opponent-behavior
       opp-response raise-action        ;; And raising again
     ?manual>
       state free
     ==>
     !output! (EXPLOIT - Opponent is aggressive raiser - CALLING with medium hand)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; Against maniac who raised preflop and flop, still call with strong hands
  (p maniac-call-down-strong
     =goal>
       isa poker-hand
       state deciding
       strength strength-high
       stage =stage
     - stage preflop
       opp-preflop-action raise-action  ;; They raised preflop
       opp-flop-action raise-action     ;; They raised flop too
     =retrieval>
       isa opponent-behavior
       opp-response raise-action        ;; And still raising
     ?manual>
       state free
     ==>
     !output! (EXPLOIT - Opponent is maniac raising every street - CALLING with strong hand to trap)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

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
     - strength strength-low                     ;; Don't bluff with nothing
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
       strength strength-high                    ;; Only do this with strong hands
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

  ;; Pattern: Opponent raised preflop AND raised flop = possibly strong
  ;; BUT only fold with truly weak hands - medium hands should call vs maniacs
  (p pattern-opp-raised-twice-we-weak
     =goal>
       isa poker-hand
       state deciding
       stage turn
       opp-preflop-action raise-action
       opp-flop-action raise-action
       strength strength-low                     ;; Only fold WEAK hands
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent raised twice and we are weak - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f")

  ;; Pattern: Opponent raised twice but we have medium - call to see if they're bluffing
  (p pattern-opp-raised-twice-we-medium
     =goal>
       isa poker-hand
       state deciding
       stage turn
       opp-preflop-action raise-action
       opp-flop-action raise-action
       strength strength-medium                  ;; Medium hands call
     ?manual>
       state free
     ==>
     !output! (PATTERN - Opponent raised twice but we are medium - CALLING to catch bluff)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c")

  ;; Pattern: Opponent raised preflop AND raised flop, we are strong = call
  (p pattern-opp-raised-twice-we-strong
     =goal>
       isa poker-hand
       state deciding
       stage turn
       opp-preflop-action raise-action
       opp-flop-action raise-action
       strength strength-high                    ;; We ARE strong
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
     - strength strength-low                     ;; Have something to back it up
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
       strength strength-high
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
       strength strength-high
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
       strength strength-medium
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
     - strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-medium
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-high
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
       strength strength-medium
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
       strength strength-medium
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
       strength strength-low
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
       strength strength-low
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
       strength strength-high
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

  ;; FIXED: Split into medium (call) and weak (fold) to not over-fold vs aggressive opponents
  (p opponent-raises-we-medium
     =goal>
       isa poker-hand
       state deciding
       strength strength-medium
     =retrieval>
       isa opponent-behavior
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (Opponent raises - we are medium - CALLING to catch bluffs)
     =goal>
       state pressing-key
       my-action call-action
       opp-action raise-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; Only fold truly weak hands to raises
  (p opponent-raises-we-weak
     =goal>
       isa poker-hand
       state deciding
       strength strength-low
     =retrieval>
       isa opponent-behavior
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (Opponent raises - we are weak - FOLDING)
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
       my-action =any-my-act
       opp-response =any-opp-resp
     - stage nil
     ==>
     !output! (Stored stage-specific opponent behavior - =any-stage =any-my-act =any-opp-resp)
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
       stage =stage
       my-action fold-action
       opp-action =opp-act
       hand-category trash
     ?imaginal>
       state free
     ==>
     !output! (Learning - Good fold with trash hand at =stage)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-outcome
       stage =stage
       my-strength strength-low
       my-action fold-action
       opp-action =opp-act
       result win)

  ;; Catch-all for folds with non-trash hands (marginal/unknown)
  (p learn-from-fold-other
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       stage =stage
       my-action fold-action
       opp-action =opp-act
       hand-category =cat
     - hand-category trash
     ?imaginal>
       state free
     ==>
     !output! (Learning - Folded with =cat hand at =stage)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-outcome
       stage =stage
       my-strength strength-medium
       my-action fold-action
       opp-action =opp-act
       result lose)

  ;; ============================================
  ;; HAND-OUTCOME LEARNING FOR SHOWDOWNS
  ;; ============================================
  ;; Store what happened when we played certain hands - used by check-medium-hand-history

  (p learn-hand-outcome-win-high
     =goal>
       isa poker-hand
       state done
       learned yes
       learned-outcome nil      ;; Only learn once
       result win
       stage =stage
       strength strength-high
       my-action =my-act
       opp-action =opp-act
     ?imaginal>
       state free
     ==>
     !output! (Learning hand outcome - WON with high hand at =stage using =my-act)
     =goal>
       learned-outcome yes      ;; Mark as learned
     +imaginal>
       isa hand-outcome
       stage =stage
       my-strength strength-high
       my-action =my-act
       opp-action =opp-act
       result win)

  (p learn-hand-outcome-win-medium
     =goal>
       isa poker-hand
       state done
       learned yes
       learned-outcome nil      ;; Only learn once
       result win
       stage =stage
       strength strength-medium
       my-action =my-act
       opp-action =opp-act
     ?imaginal>
       state free
     ==>
     !output! (Learning hand outcome - WON with medium hand at =stage using =my-act)
     =goal>
       learned-outcome yes      ;; Mark as learned
     +imaginal>
       isa hand-outcome
       stage =stage
       my-strength strength-medium
       my-action =my-act
       opp-action =opp-act
       result win)

  (p learn-hand-outcome-lose-high
     =goal>
       isa poker-hand
       state done
       learned yes
       learned-outcome nil      ;; Only learn once
       result lose
       stage =stage
       strength strength-high
       my-action =my-act
       opp-action =opp-act
     - my-action fold-action
     ?imaginal>
       state free
     ==>
     !output! (Learning hand outcome - LOST with high hand at =stage using =my-act)
     =goal>
       learned-outcome yes      ;; Mark as learned
     +imaginal>
       isa hand-outcome
       stage =stage
       my-strength strength-high
       my-action =my-act
       opp-action =opp-act
       result lose)

  (p learn-hand-outcome-lose-medium
     =goal>
       isa poker-hand
       state done
       learned yes
       learned-outcome nil      ;; Only learn once
       result lose
       stage =stage
       strength strength-medium
       my-action =my-act
       opp-action =opp-act
     - my-action fold-action
     ?imaginal>
       state free
     ==>
     !output! (Learning hand outcome - LOST with medium hand at =stage using =my-act)
     =goal>
       learned-outcome yes      ;; Mark as learned
     +imaginal>
       isa hand-outcome
       stage =stage
       my-strength strength-medium
       my-action =my-act
       opp-action =opp-act
       result lose)

  ;; ============================================
  ;; OPPONENT SHOWDOWN MEMORY
  ;; ============================================
  ;; Remember what hands opponent showed down - helps predict their range

  (p learn-opponent-showdown-win
     =goal>
       isa poker-hand
       state done
       result win
       learned-showdown nil      ;; Only learn once
       opp-rank1 =or1
       opp-rank2 =or2
       opp-suited =os
       opp-action =opp-act
     - opp-rank1 nil
     - opp-rank2 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning opponent showdown - they had =or1 =or2 suited =os and LOST)
     =goal>
       learned-showdown yes      ;; Mark as learned
     +imaginal>
       isa opponent-showdown
       rank1 =or1
       rank2 =or2
       suited =os
       opp-action-pattern =opp-act
       result lose)

  (p learn-opponent-showdown-lose
     =goal>
       isa poker-hand
       state done
       result lose
       learned-showdown nil      ;; Only learn once
       opp-rank1 =or1
       opp-rank2 =or2
       opp-suited =os
       opp-action =opp-act
     - opp-rank1 nil
     - opp-rank2 nil
     ?imaginal>
       state free
     ==>
     !output! (Learning opponent showdown - they had =or1 =or2 suited =os and WON)
     =goal>
       learned-showdown yes      ;; Mark as learned
     +imaginal>
       isa opponent-showdown
       rank1 =or1
       rank2 =or2
       suited =os
       opp-action-pattern =opp-act
       result win)

  ;; ============================================
  ;; CLEAR PRODUCTIONS
  ;; ============================================
  ;; Each tests for a UNIQUE slot that only exists in that chunk type
  ;; This prevents partial matching from causing the wrong production to fire

  (p clear-opponent-behavior
     =goal>
       isa poker-hand
       state done
     =imaginal>
       isa opponent-behavior
       stage =any-stage            ;; Use stage slot
       my-action =any-my-act       ;; Use my-action slot
       opp-response =any-response  ;; Only opponent-behavior has opp-response
     ==>
     !output! (Stored opponent behavior - stage =any-stage my-action =any-my-act opp-response =any-response)
     -imaginal>)

  (p clear-hand-outcome
     =goal>
       isa poker-hand
       state done
     =imaginal>
       isa hand-outcome
       stage =any-stage            ;; Use stage slot
       my-strength =any-strength   ;; Only hand-outcome has my-strength
       my-action =any-my-act       ;; Use my-action slot
       opp-action =any-opp-act     ;; Use opp-action slot
       result =any-result          ;; Use result slot
     ==>
     !output! (Stored hand outcome - =any-strength =any-my-act vs =any-opp-act result =any-result)
     -imaginal>)

  (p clear-opponent-showdown
     =goal>
       isa poker-hand
       state done
     =imaginal>
       isa opponent-showdown
       rank1 =any-r1               ;; Use rank1 slot
       rank2 =any-r2               ;; Use rank2 slot
       suited =any-suited          ;; Use suited slot
       opp-action-pattern =any     ;; Only opponent-showdown has opp-action-pattern
       result =any-result          ;; Use result slot
     ==>
     !output! (Stored opponent showdown - ranks =any-r1 =any-r2 suited =any-suited result =any-result)
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
               ;; Opponent card ranks for memory
               opp-rank1 nil opp-rank2 nil opp-suited nil
               opp-cards-attended 0
               result nil
               learned nil
               learned-outcome nil  ;; Flag for hand-outcome learning
               learned-showdown nil ;; Flag for opponent-showdown learning
               ;; Position tracking
               position nil
               my-chips nil opp-chips nil pot-amount nil))
)