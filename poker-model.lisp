(clear-all)

(define-model cognitive-poker

  ;; ============================================
  ;; MODEL PARAMETERS - INSTANCE-BASED LEARNING
  ;; ============================================
  ;; This model learns through DECLARATIVE MEMORY:
  ;; - Stores outcomes of past hands
  ;; - Retrieves similar past experiences when deciding
  ;; - Successful patterns become more retrievable over time
  ;; - NO hardcoded strategy - learns from experience

  (sgp :esc t                 ;; Enable subsymbolic computations
       :bll 0.6               ;; Base-level learning - higher = faster adaptation
       :ol t                  ;; Optimized learning
       :er t                  ;; Enable randomness
       :lf 0.4                ;; Latency factor
       :ans 0.25              ;; Activation noise - lower = more consistent retrieval
       :mp 2.0                ;; Partial matching (retrieve similar memories)
       :rt -2.0               ;; Retrieval threshold (lower = more retrieval)
       :mas 5.0               ;; Max associative strength
       :ga 1.0                ;; Goal activation spread
       :imaginal-activation 1.0)  ;; Imaginal spreads activation

  (sgp :v t :trace-detail low :show-focus nil)

  ;; ============================================
  ;; CHUNK TYPES
  ;; ============================================

  ;; hand-memory: stores what happened in past hands
  ;; This is what the model learns from
  (chunk-type hand-memory
    strength           ;; strength-high / strength-medium / strength-low
    action             ;; raise-action / call-action / fold-action
    outcome)           ;; win / lose

  ;; opponent-pattern: stores how opponent responds
  ;; This is opponent modeling
  ;; Uses pattern-type slot to distinguish from hand-memory during retrieval
  (chunk-type opponent-pattern
    pattern-type       ;; always 'opponent' - distinguishes from hand-memory
    opp-response)      ;; call-action / raise-action / fold-action

  ;; poker-hand: current game state (used in goal buffer)
  ;; Only slots actually used by productions are included
  ;; NOTE: Avoid reserved words like 'position' (used by vision module)
  (chunk-type poker-hand
    stage              ;; preflop / flop / turn / river / showdown / done
    state              ;; waiting / checking-opponent / retrieving-win / etc.
    strength           ;; strength-high / strength-medium / strength-low
    seat               ;; sb (small blind) / bb (big blind) - renamed from 'position'
    pot-situation      ;; odds-good / odds-neutral / odds-bad - renamed from 'pot-odds'
    stack-size         ;; stack-deep / stack-medium / stack-short - renamed from 'stack-status'
    my-action          ;; raise-action / call-action / fold-action / check-action
    opp-action         ;; opponent's action for learning
    opp-card1          ;; opponent card at showdown (for visual attention)
    opp-card2          ;; opponent card at showdown (for visual attention)
    opp-cards-attended ;; count of opponent cards attended
    result             ;; win / lose / tie
    learned)           ;; yes / nil - whether we stored this hand's memory

  ;; ============================================
  ;; CHUNKS
  ;; ============================================

  (define-chunks
    ;; States
    (waiting) (checking-opponent) (retrieving-win) (retrieving-lose) (deciding)
    (pressing-key) (action-done)
    (attending-showdown) (attending) (processing-feedback) (done)
    (learning-stage) (storing-memory) (storing-opponent)
    ;; Stages
    (preflop) (flop) (turn) (river) (showdown)
    ;; Categories
    (premium) (strong) (playable) (marginal) (trash) (unknown)
    (yes) (no)
    ;; Strengths
    (strength-high) (strength-medium) (strength-low)
    ;; Actions
    (fold-action) (call-action) (raise-action) (check-action)
    ;; Outcomes
    (win) (lose) (tie)
    ;; Positions
    (sb) (bb)
    ;; Pot odds
    (odds-good) (odds-neutral) (odds-bad)
    ;; Stack status
    (stack-deep) (stack-medium) (stack-short)
    ;; Pattern type marker (distinguishes opponent-pattern from hand-memory)
    (opponent))

  ;; NOTE: declare-buffer-usage removed - was causing warnings and is optional
  ;; The model works without explicit buffer declarations

  (install-device '("motor" "keyboard"))

  ;; ============================================
  ;; DECISION PHASE 0: Check opponent pattern first
  ;; ============================================
  ;; Before deciding based on hand, check what opponent usually does

  (p check-opponent-first
     =goal>
       isa poker-hand
       state waiting
       stage =stage
     - stage showdown
     - stage done
     ==>
     !output! (Checking opponent pattern...)
     =goal>
       state checking-opponent
     +retrieval>
       isa opponent-pattern
       pattern-type opponent   ;; This distinguishes from hand-memory chunks
       :recently-retrieved nil)

  ;; ============================================
  ;; OPPONENT IS A FOLDER - Exploit by raising!
  ;; ============================================
  ;; If opponent folds a lot, raise with strong/medium hands
  ;; BUT be cautious with weak hands - if they don't fold, we lose big
  ;; This prevents "tilt" behavior against tight opponents

  ;; Strong hands: Always raise vs folders (value + they might call with worse)
  (p opponent-folds-exploit-strong
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-high
     =retrieval>
       isa opponent-pattern
       opp-response fold-action
     ?manual>
       state free
     ==>
     !output! (EXPLOIT - Opponent folds and we are STRONG - RAISING!)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  ;; Medium hands: Raise vs folders (good exploitation)
  (p opponent-folds-exploit-medium
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-medium
     =retrieval>
       isa opponent-pattern
       opp-response fold-action
     ?manual>
       state free
     ==>
     !output! (EXPLOIT - Opponent folds and we are MEDIUM - RAISING to steal!)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  ;; Weak hands vs folders: CHECK instead of raising
  ;; Reasoning: If they fold, we win anyway. If they DON'T fold, we're not committed.
  ;; This prevents over-exploiting and "tilting" with trash hands.
  (p opponent-folds-exploit-weak
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
     =retrieval>
       isa opponent-pattern
       opp-response fold-action
     ?manual>
       state free
     ==>
     !output! (CAUTION - Opponent usually folds but we are WEAK - CHECK to see what happens)
     =goal>
       state pressing-key
       my-action check-action
     +manual>
       cmd press-key
       key "k"
     -retrieval>)

  ;; ============================================
  ;; OPPONENT IS AGGRESSIVE - Be cautious
  ;; ============================================
  ;; If opponent raises a lot, tighten up

  (p opponent-raises-strong
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-high
     =retrieval>
       isa opponent-pattern
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (CAUTION - Opponent aggressive but we strong - CALLING to trap)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; Against raisers with MEDIUM hand: Use pot odds to decide!
  ;; Good odds = call (opponent might be bluffing)
  ;; Bad odds = fold (not worth the risk)
  (p opponent-raises-medium-good-odds
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-medium
       pot-situation odds-good    ;; Good pot odds - worth calling
     =retrieval>
       isa opponent-pattern
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (BLUFF CATCH - Opponent raises but pot odds good - CALLING with medium!)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p opponent-raises-medium-neutral-odds
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-medium
       pot-situation odds-neutral  ;; Neutral odds - marginal call
     =retrieval>
       isa opponent-pattern
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (MARGINAL - Opponent raises neutral odds - CALLING with medium)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p opponent-raises-medium-bad-odds
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-medium
       pot-situation odds-bad      ;; Bad pot odds - fold
     =retrieval>
       isa opponent-pattern
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (CAUTION - Opponent raises bad odds - FOLDING medium)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; Against raisers with WEAK hand: Usually fold but call with great odds
  (p opponent-raises-weak-good-odds
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
       pot-situation odds-good     ;; Great pot odds - bluff catch
     =retrieval>
       isa opponent-pattern
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (BLUFF CATCH - Opponent raises but great odds - CALLING weak!)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p opponent-raises-weak-fold
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
     - pot-situation odds-good     ;; Not great odds - fold
     =retrieval>
       isa opponent-pattern
       opp-response raise-action
     ?manual>
       state free
     ==>
     !output! (CAUTION - Opponent raises and we weak - FOLDING)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; ============================================
  ;; OPPONENT CALLS or NO PATTERN - Normal decision
  ;; ============================================
  ;; Fall back to strength-based decision

  ;; Against callers: CHECK medium hands (don't inflate pot)
  ;; This is better than raising - they won't fold anyway
  (p opponent-calls-medium-check
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-medium
     =retrieval>
       isa opponent-pattern
       opp-response call-action
     ==>
     !output! (Opponent calls - CHECK with medium hand - dont inflate pot!)
     =goal>
       state pressing-key
       my-action check-action
     +manual>
       cmd press-key
       key "k"
     -retrieval>)

  ;; Against callers: RAISE strong hands for value (they will call with worse)
  (p opponent-calls-strong-raise
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-high
     =retrieval>
       isa opponent-pattern
       opp-response call-action
     ==>
     !output! (Opponent calls - RAISE strong hand for VALUE!)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  ;; Against callers: CHECK or FOLD weak hands (can't bluff them)
  (p opponent-calls-weak-check
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
       pot-situation =odds
     - pot-situation odds-bad      ;; If odds are ok, check to see free cards
     =retrieval>
       isa opponent-pattern
       opp-response call-action
     ==>
     !output! (Opponent calls - CHECK weak hand - cant bluff - see free cards)
     =goal>
       state pressing-key
       my-action check-action
     +manual>
       cmd press-key
       key "k"
     -retrieval>)

  ;; Against callers with bad pot odds: FOLD weak hands
  (p opponent-calls-weak-fold
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
       pot-situation odds-bad      ;; Bad odds = not worth continuing
     =retrieval>
       isa opponent-pattern
       opp-response call-action
     ==>
     !output! (Opponent calls + bad odds - FOLD weak hand)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; ============================================
  ;; OPPONENT CHECKS BACK - Similar to calling (passive)
  ;; ============================================

  (p opponent-checks-medium-check
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-medium
     =retrieval>
       isa opponent-pattern
       opp-response check-action
     ==>
     !output! (Opponent checks - CHECK medium hand too)
     =goal>
       state pressing-key
       my-action check-action
     +manual>
       cmd press-key
       key "k"
     -retrieval>)

  (p opponent-checks-strong-raise
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-high
     =retrieval>
       isa opponent-pattern
       opp-response check-action
     ==>
     !output! (Opponent checks - RAISE strong for value!)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p opponent-checks-weak-check
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
     =retrieval>
       isa opponent-pattern
       opp-response check-action
     ==>
     !output! (Opponent checks - CHECK weak hand - free cards!)
     =goal>
       state pressing-key
       my-action check-action
     +manual>
       cmd press-key
       key "k"
     -retrieval>)

  (p no-opponent-pattern-strong-medium
     =goal>
       isa poker-hand
       state checking-opponent
       strength =str
     - strength strength-low
     ?retrieval>
       buffer failure
     ==>
     !output! (No opponent pattern - normal play with =str)
     =goal>
       state retrieving-win
     +retrieval>
       isa hand-memory
       strength =str
       outcome win
       :recently-retrieved nil)

  (p no-opponent-pattern-weak
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
     ?retrieval>
       buffer failure
     ==>
     !output! (No opponent pattern - cautious with weak)
     =goal>
       state retrieving-lose
     +retrieval>
       isa hand-memory
       strength strength-low
       outcome lose
       :recently-retrieved nil)

  ;; Handle case where wrong chunk type is retrieved (partial matching issue)
  ;; Catch-all: If retrieval returns something without a valid opp-response
  ;; This handles partial matching returning wrong chunk types
  (p wrong-retrieval-fallback-strong-medium
     =goal>
       isa poker-hand
       state checking-opponent
       strength =str
     - strength strength-low
     =retrieval>
     - opp-response fold-action   ;; Not a fold pattern
     - opp-response call-action   ;; Not a call pattern
     - opp-response raise-action  ;; Not a raise pattern
     - opp-response check-action  ;; Not a check pattern
     ==>
     !output! (Retrieved chunk without valid opp-response - fallback to normal play)
     =goal>
       state retrieving-win
     -retrieval>
     +retrieval>
       isa hand-memory
       strength =str
       outcome win
       :recently-retrieved nil)

  (p wrong-retrieval-fallback-weak
     =goal>
       isa poker-hand
       state checking-opponent
       strength strength-low
     =retrieval>
     - opp-response fold-action
     - opp-response call-action
     - opp-response raise-action
     - opp-response check-action
     ==>
     !output! (Retrieved chunk without valid opp-response - fallback weak)
     =goal>
       state retrieving-lose
     -retrieval>
     +retrieval>
       isa hand-memory
       strength strength-low
       outcome lose
       :recently-retrieved nil)

  ;; ============================================
  ;; FOUND WINNING MEMORY - Do that action again!
  ;; ============================================

  (p found-winning-memory-raise
     =goal>
       isa poker-hand
       state retrieving-win
     =retrieval>
       isa hand-memory
       action raise-action
       outcome win
     ?manual>
       state free
     ==>
     !output! (MEMORY - Won before by RAISING - doing it again!)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p found-winning-memory-call
     =goal>
       isa poker-hand
       state retrieving-win
     =retrieval>
       isa hand-memory
       action call-action
       outcome win
     ?manual>
       state free
     ==>
     !output! (MEMORY - Won before by CALLING - doing it again!)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  (p found-winning-memory-check
     =goal>
       isa poker-hand
       state retrieving-win
     =retrieval>
       isa hand-memory
       action check-action
       outcome win
     ?manual>
       state free
     ==>
     !output! (MEMORY - Won before by CHECKING - pot control!)
     =goal>
       state pressing-key
       my-action check-action
     +manual>
       cmd press-key
       key "k"
     -retrieval>)

  (p found-winning-memory-fold
     =goal>
       isa poker-hand
       state retrieving-win
     =retrieval>
       isa hand-memory
       action fold-action
       outcome win
     ?manual>
       state free
     ==>
     ;; This shouldn't happen (can't win by folding) but handle it
     !output! (MEMORY - Strange win-fold memory - calling instead)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; ============================================
  ;; NO WINNING MEMORY - Try to find a LOSING memory to avoid
  ;; ============================================

  (p no-winning-memory-try-losing
     =goal>
       isa poker-hand
       state retrieving-win
       strength =str
     ?retrieval>
       buffer failure
     ==>
     !output! (No winning memory - searching for losing memory to avoid)
     =goal>
       state retrieving-lose
     +retrieval>
       isa hand-memory
       strength =str
       outcome lose
       :recently-retrieved nil)

  ;; ============================================
  ;; FOUND LOSING MEMORY - Avoid that action!
  ;; For weak hands: any loss means FOLD (don't try other actions)
  ;; For strong/medium: try a different action
  ;; ============================================

  ;; WEAK HANDS: Lost with weak = just fold, don't try other actions
  (p found-losing-memory-weak
     =goal>
       isa poker-hand
       state retrieving-lose
       strength strength-low
     =retrieval>
       isa hand-memory
       outcome lose
     ?manual>
       state free
     ==>
     !output! (MEMORY - Lost with weak hand before - FOLDING this time)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f"
     -retrieval>)

  ;; STRONG/MEDIUM: Lost by raising, try calling
  (p found-losing-memory-raised
     =goal>
       isa poker-hand
       state retrieving-lose
     - strength strength-low
     =retrieval>
       isa hand-memory
       action raise-action
       outcome lose
     ?manual>
       state free
     ==>
     !output! (MEMORY - Lost before by RAISING - trying CALL instead)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c"
     -retrieval>)

  ;; STRONG/MEDIUM: Lost by calling, try raising
  (p found-losing-memory-called
     =goal>
       isa poker-hand
       state retrieving-lose
     - strength strength-low
     =retrieval>
       isa hand-memory
       action call-action
       outcome lose
     ?manual>
       state free
     ==>
     !output! (MEMORY - Lost before by CALLING - trying RAISE instead)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  (p found-losing-memory-checked
     =goal>
       isa poker-hand
       state retrieving-lose
     - strength strength-low
     =retrieval>
       isa hand-memory
       action check-action
       outcome lose
     ?manual>
       state free
     ==>
     !output! (MEMORY - Lost before by CHECKING - trying RAISE instead)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r"
     -retrieval>)

  ;; ============================================
  ;; NO MEMORY AT ALL - Default exploration
  ;; ============================================
  ;; When no memories exist, use simple exploration strategy

  (p no-memory-strong-default
     =goal>
       isa poker-hand
       state retrieving-lose
       strength strength-high
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (NO MEMORY - Strong hand - exploring with RAISE)
     =goal>
       state pressing-key
       my-action raise-action
     +manual>
       cmd press-key
       key "r")

  (p no-memory-medium-default
     =goal>
       isa poker-hand
       state retrieving-lose
       strength strength-medium
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (NO MEMORY - Medium hand - exploring with CALL)
     =goal>
       state pressing-key
       my-action call-action
     +manual>
       cmd press-key
       key "c")

  (p no-memory-weak-default
     =goal>
       isa poker-hand
       state retrieving-lose
       strength strength-low
     ?retrieval>
       buffer failure
     ?manual>
       state free
     ==>
     !output! (NO MEMORY - Weak hand - exploring with FOLD)
     =goal>
       state pressing-key
       my-action fold-action
     +manual>
       cmd press-key
       key "f")

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
  ;; PER-STAGE LEARNING - Store opponent patterns!
  ;; Python sets state to learning-stage after opponent acts
  ;; ============================================

  ;; Only store opponent pattern when WE RAISED
  ;; If we called, opponent's "call" is just a check - not informative
  ;; This prevents memory pollution with fake "call" patterns
  (p stage-learning-store-opponent-fold
     =goal>
       isa poker-hand
       state learning-stage
       my-action raise-action    ;; ONLY learn when we raised!
       opp-action fold-action    ;; Opponent folded to our raise
     ?imaginal>
       state free
     ==>
     !output! (LEARNING OPPONENT - After our RAISE they FOLDED!)
     =goal>
       state storing-opponent
     +imaginal>
       isa opponent-pattern
       pattern-type opponent
       opp-response fold-action)

  ;; Opponent called our raise - store that pattern
  (p stage-learning-store-opponent-call
     =goal>
       isa poker-hand
       state learning-stage
       my-action raise-action    ;; ONLY learn when we raised!
       opp-action call-action    ;; Opponent called our raise
     ?imaginal>
       state free
     ==>
     !output! (LEARNING OPPONENT - After our RAISE they CALLED!)
     =goal>
       state storing-opponent
     +imaginal>
       isa opponent-pattern
       pattern-type opponent
       opp-response call-action)

  ;; Opponent raised over our raise - store that pattern
  (p stage-learning-store-opponent-raise
     =goal>
       isa poker-hand
       state learning-stage
       my-action raise-action    ;; ONLY learn when we raised!
       opp-action raise-action   ;; Opponent re-raised
     ?imaginal>
       state free
     ==>
     !output! (LEARNING OPPONENT - After our RAISE they RE-RAISED!)
     =goal>
       state storing-opponent
     +imaginal>
       isa opponent-pattern
       pattern-type opponent
       opp-response raise-action)

  ;; Skip learning when we called (opponent's action isn't informative)
  (p stage-learning-skip-called
     =goal>
       isa poker-hand
       state learning-stage
       my-action call-action    ;; We called - don't learn opponent pattern
     ==>
     !output! (STAGE - We called so opponent response not informative)
     =goal>
       state action-done)

  ;; ============================================
  ;; LEARN FROM CHECK INTERACTIONS
  ;; ============================================

  ;; Opponent checked back after we checked - they're passive
  (p stage-learning-store-opponent-check
     =goal>
       isa poker-hand
       state learning-stage
       my-action check-action   ;; We checked
       opp-action check-action  ;; Opponent checked back
     ?imaginal>
       state free
     ==>
     !output! (LEARNING OPPONENT - After our CHECK they CHECKED back - passive!)
     =goal>
       state storing-opponent
     +imaginal>
       isa opponent-pattern
       pattern-type opponent
       opp-response check-action)

  ;; Opponent raised after we checked - they're aggressive
  (p stage-learning-store-opponent-bets
     =goal>
       isa poker-hand
       state learning-stage
       my-action check-action   ;; We checked
       opp-action raise-action  ;; Opponent bet/raised
     ?imaginal>
       state free
     ==>
     !output! (LEARNING OPPONENT - After our CHECK they BET - aggressive!)
     =goal>
       state storing-opponent
     +imaginal>
       isa opponent-pattern
       pattern-type opponent
       opp-response raise-action)

  ;; Opponent called after we checked - treat as passive (shouldn't happen but handle it)
  (p stage-learning-store-opponent-call-after-check
     =goal>
       isa poker-hand
       state learning-stage
       my-action check-action   ;; We checked
       opp-action call-action   ;; Opponent "called" (weird but handle it)
     ?imaginal>
       state free
     ==>
     !output! (LEARNING OPPONENT - After our CHECK they called - passive)
     =goal>
       state storing-opponent
     +imaginal>
       isa opponent-pattern
       pattern-type opponent
       opp-response call-action)

  (p stage-learning-commit-opponent
     =goal>
       isa poker-hand
       state storing-opponent
     =imaginal>
       isa opponent-pattern
       opp-response =resp
     ==>
     !output! (Stored opponent pattern - they tend to =resp)
     =goal>
       state action-done
     -imaginal>)

  (p stage-learning-skip
     =goal>
       isa poker-hand
       state learning-stage
       opp-action nil
     ==>
     !output! (STAGE - No opponent action to note)
     =goal>
       state action-done)

  ;; ============================================
  ;; SHOWDOWN HANDLING
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
     !output! (Attending opponent card)
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

  (p no-opponent-cards
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
  ;; LEARNING FROM OUTCOMES
  ;; SELECTIVE MEMORY - Only store SURPRISING outcomes
  ;; This prevents memory flooding during losing streaks
  ;; ============================================
  ;; Winning with strong = expected, don't clutter memory
  ;; Losing with weak = expected, don't clutter memory
  ;; Winning with weak = SURPRISING, store it!
  ;; Losing with strong = SURPRISING, store it!
  ;; Medium hands = always informative, store them
  ;; ============================================

  ;; SURPRISING WIN: Weak hand won - worth remembering!
  (p learn-surprising-win-weak
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       strength strength-low
       my-action =act
     ?imaginal>
       state free
     ==>
     !output! (SURPRISING - Won with WEAK hand by =act - definitely storing!)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-memory
       strength strength-low
       action =act
       outcome win)

  ;; SURPRISING LOSS: Strong hand lost - worth remembering!
  (p learn-surprising-loss-strong
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       strength strength-high
       my-action =act
     - my-action fold-action
     ?imaginal>
       state free
     ==>
     !output! (SURPRISING - Lost with STRONG hand by =act - definitely storing!)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-memory
       strength strength-high
       action =act
       outcome lose)

  ;; MEDIUM HANDS: Always informative - store both wins and losses
  (p learn-medium-win
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       strength strength-medium
       my-action =act
     ?imaginal>
       state free
     ==>
     !output! (LEARNING - Medium hand WON by =act - storing!)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-memory
       strength strength-medium
       action =act
       outcome win)

  (p learn-medium-loss
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       strength strength-medium
       my-action =act
     - my-action fold-action
     ?imaginal>
       state free
     ==>
     !output! (LEARNING - Medium hand LOST by =act - storing!)
     =goal>
       state done
       learned yes
     +imaginal>
       isa hand-memory
       strength strength-medium
       action =act
       outcome lose)

  ;; EXPECTED WIN: Strong hand won - skip, not informative
  (p skip-expected-win-strong
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result win
       strength strength-high
     ==>
     !output! (EXPECTED - Strong hand won - no need to store)
     =goal>
       state done
       learned yes)

  ;; EXPECTED LOSS: Weak hand lost - skip, not informative
  (p skip-expected-loss-weak
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       strength strength-low
     - my-action fold-action
     ==>
     !output! (EXPECTED - Weak hand lost - no need to store)
     =goal>
       state done
       learned yes)

  ;; NO learn-from-fold production!
  ;; Cognitively plausible: when you fold, you get no feedback
  ;; You never see if folding was right or wrong
  ;; Model only learns from showdowns where it observes the outcome

  (p skip-fold-learning
     =goal>
       isa poker-hand
       state processing-feedback
       learned nil
       result lose
       my-action fold-action
     ==>
     !output! (FOLDED - No feedback to learn from)
     =goal>
       state done
       learned yes)  ;; Mark as learned so we move on, but store nothing

  ;; ============================================
  ;; CLEAR IMAGINAL - Commits memory to DM
  ;; ============================================

  (p commit-memory
     =goal>
       isa poker-hand
       state done
       learned yes
     =imaginal>
       isa hand-memory
       outcome =out
     ==>
     !output! (Memory committed to DM - outcome was =out)
     -imaginal>)

  ;; ============================================
  ;; INITIAL GOAL
  ;; ============================================

  (goal-focus (isa poker-hand
               stage preflop
               state waiting
               strength nil
               my-action nil
               opp-action nil
               opp-card1 nil
               opp-card2 nil
               opp-cards-attended 0
               result nil
               learned nil))

  ;; ============================================
  ;; SEED MEMORIES - Prior knowledge from experience/teaching
  ;; ============================================
  ;; These represent knowledge gained from:
  ;; - Reading poker books
  ;; - Advice from experienced players
  ;; - Watching others play
  ;;
  ;; When BLL is enabled, use :references (list of past retrieval times)
  ;; instead of :base-level. More references = higher activation.
  ;; Times are negative (seconds before now).

  (add-dm
    ;; Strong hands - raising wins (high confidence)
    (win-strong-raise-1 isa hand-memory
      strength strength-high action raise-action outcome win)
    (win-strong-raise-2 isa hand-memory
      strength strength-high action raise-action outcome win)
    (win-strong-raise-3 isa hand-memory
      strength strength-high action raise-action outcome win)

    ;; Medium hands - balanced: raise, call, and check options
    (win-medium-raise-1 isa hand-memory
      strength strength-medium action raise-action outcome win)
    (win-medium-raise-2 isa hand-memory
      strength strength-medium action raise-action outcome win)
    ;; Call memory - useful vs callers
    (win-medium-call-1 isa hand-memory
      strength strength-medium action call-action outcome win)
    ;; Check memories - pot control, cheap showdowns
    (win-medium-check-1 isa hand-memory
      strength strength-medium action check-action outcome win)
    (win-medium-check-2 isa hand-memory
      strength strength-medium action check-action outcome win)

    ;; Weak hands - checking and folding save money
    (lose-weak-raise-1 isa hand-memory
      strength strength-low action raise-action outcome lose)
    ;; Check can sometimes win with weak hands (free showdown)
    (win-weak-check-1 isa hand-memory
      strength strength-low action check-action outcome win)
    ;; Fold saves money with weak hands
    (win-weak-fold-1 isa hand-memory
      strength strength-low action fold-action outcome win))

  ;; Set activation via references (simulated past retrievals)
  ;; When BLL is enabled, :references is the correct way to set base-level

  ;; Strong hands: strong prior (encourage raising)
  (sdp win-strong-raise-1 :references 3)
  (sdp win-strong-raise-2 :references 3)
  (sdp win-strong-raise-3 :references 3)

  ;; Medium hands: balanced between raise, call, check
  (sdp win-medium-raise-1 :references 2)
  (sdp win-medium-raise-2 :references 2)
  (sdp win-medium-call-1 :references 2)
  (sdp win-medium-check-1 :references 2)
  (sdp win-medium-check-2 :references 2)

  ;; Weak hands: check and fold are good options
  (sdp lose-weak-raise-1 :references 1)
  (sdp win-weak-check-1 :references 2)
  (sdp win-weak-fold-1 :references 2)
)
