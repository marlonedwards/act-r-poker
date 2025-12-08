# ACT-R Poker Final Project
Goal: Model human poker player decision-making during online games.

### File Structure & Running
- `poker.lisp` - Environment: window, cards, game logic
- `poker-model.lisp` - visual attention and actions

```lisp
;; Run a single hand
(poker-trial)
(pt)

;; Test the table layout
(test-layout)
(tl)
```

## Model
The model is currently deterministic against the opponent, it always calls regardless of hand strength.
```
PREFLOP:
  Find first hole card (highest y) -> Attend -> Encode
  Find second hole card -> Attend -> Encode
  Press C to call

FLOP:
  Find board card -> Attend -> Encode (repeat 3x)
  Press C to call

TURN:
  Find new card -> Attend -> Encode
  Press C to call

RIVER:
  Find new card -> Attend -> Encode
  Press C to call
  -> Hand complete
```

## Card Encoding

- Ranks: A K Q J T 9 8 7 6 5 4 3 2
- Suits: H (Hearts), D (Diamonds), C (Clubs), S (Spades)

### AI Acknowledgement
I used Claude Sonnet 4.5 to help me implement the Fisher-Yates shuffle, create-poker-window routines, and debug text on model load and run. 
