# ACT-R Poker Cognitive Model

A cognitive model of poker decision-making built using the ACT-R cognitive architecture. The model simulates how a human player learns and adapts in heads-up Texas Hold'em through instance-based learning and opponent modeling.

## Overview

This project implements a cognitively plausible poker player that:

- Learns from experience using ACT-R's declarative memory system
- Models opponent behavior by storing patterns of their responses
- Makes decisions based on hand strength, pot odds, stack sizes, and past outcomes
- Adapts strategy over time through selective memory of surprising outcomes
- Explores different actions when no relevant memories exist

The model does not use hardcoded optimal strategy. Instead, it learns through trial and error, storing memories of wins and losses and retrieving similar past experiences when making decisions.

## File Structure

- `poker.py` - Python environment that manages game state, deals cards, evaluates hands, and interfaces with the ACT-R model
- `poker-model.lisp` - ACT-R cognitive model defining productions for decision-making and memory-based learning
- `poker_viz.py` - Visualization module for analyzing learning progression with matplotlib charts

## How It Works

### Decision Flow

1. **Check Opponent Patterns**: Before deciding, the model retrieves memories of how the opponent has responded in the past
2. **Exploit or Adapt**: If opponent folds often, exploit by raising. If opponent is aggressive, play cautiously
3. **Memory-Based Decisions**: When no strong opponent pattern exists, retrieve memories of similar past hands
4. **Exploration**: When no memories exist, use default exploration (raise strong, call medium, fold weak)

### Learning Mechanisms

**Instance-Based Learning**: The model stores outcomes of hands in declarative memory. Successful patterns become more retrievable over time through ACT-R's base-level learning.

**Selective Memory**: Only surprising outcomes are stored to prevent memory flooding:
- Winning with weak hands (surprising, stored)
- Losing with strong hands (surprising, stored)
- Medium hand outcomes (always informative, stored)
- Expected outcomes like strong-hand wins are skipped

**Opponent Modeling**: The model stores opponent response patterns (fold, call, raise) and retrieves them to inform exploitation strategy.

### ACT-R Buffer Usage

The model uses two primary buffers:

**Goal Buffer**: Holds the `poker-hand` chunk containing current game state:
- Stage (preflop, flop, turn, river, showdown)
- Processing state (waiting, checking-opponent, retrieving-win, etc.)
- Hand strength, position, pot odds, stack size
- Current and opponent actions
- Result tracking

**Imaginal Buffer**: Used to construct new memories before committing to declarative memory:
- `opponent-pattern` chunks storing how opponent responded to our actions
- `hand-memory` chunks storing outcomes (strength + action + result)

When a production creates a chunk in the imaginal buffer and later clears it (`-imaginal>`), that chunk is automatically committed to declarative memory where it can be retrieved in future hands.

### Hand Strength Evaluation

The Python environment evaluates hand strength as high, medium, or low based on:
- Preflop: Starting hand charts (pairs, suited connectors, high cards)
- Postflop: Full poker hand evaluation (pairs, straights, flushes, etc.) plus draw potential

### Opponent Types

Five opponent types are available for testing:

| Type | Behavior | Optimal Counter-Strategy |
|------|----------|-------------------------|
| `always-call` | Calls every bet, checks when we check | Value bet strong hands, never bluff |
| `always-fold` | Folds to any raise, calls otherwise | Raise everything to steal pots |
| `tight` | Folds weak, calls medium, raises strong | Bluff more, fold to their raises |
| `aggressive` | Raises 60%, calls 40% | Trap with strong hands, fold weak |
| `random` | Fold 20%, call 50%, raise 30% | Play straightforward by hand strength |

**Detailed Behaviors:**

```
always-call:
  if player_checks: check
  else: call

always-fold:
  if player_raises: fold
  else: call

tight:
  hand_strength = evaluate(cards, board)
  if weak: fold
  if medium: call
  if strong AND player_didnt_raise: raise
  if strong AND player_raised: call

aggressive:
  60% chance: raise
  40% chance: call

random:
  20% chance: fold
  50% chance: call
  30% chance: raise
```

## Running the Model

### Prerequisites

- ACT-R 7.x installed and running
- Python 3.x with access to the ACT-R Python interface

### Basic Usage

```bash
# Run 20 hands against always-call opponent (default)
python poker.py

# Run N hands against a specific opponent
python poker.py 50 tight

# Run learning validation (500 hands with progress tracking)
python poker.py validate
python poker.py validate tight

# Run comprehensive validation against all opponent types
python poker.py validate-all

# Compare performance against all opponents
python poker.py compare 50
```

### Interactive Play

```bash
# Play a single hand with visual window
python poker.py person

# Play against specific opponent
python poker.py person aggressive

# Play multiple hands interactively
python poker.py person-session 10
python poker.py person-session 10 tight
```

### Python API

```python
import poker

# Run a session
results = poker.run_session(n_hands=50, opponent_type='tight')

# Run learning validation with block tracking
data = poker.validate_learning(n_hands=500, block_size=50)

# Compare all opponents
summary = poker.compare_opponents(n_hands=20)

# Debug learning with memory traces
poker.quick_debug(n_hands=10)
```

### Visualization

The `poker_viz.py` module provides matplotlib charts for analyzing learning:

```bash
# Run session and visualize
python poker_viz.py 100 always-call

# Save visualization to file
python poker_viz.py 500 tight results.png
```

```python
import poker
import poker_viz

# Run session and analyze
results = poker.run_session(n_hands=100, opponent_type='always-call')
poker_viz.run_analysis(results, opponent_type='always-call', show_plot=True)
```

The visualization shows four panels:
1. **Cumulative Profit/Loss** - Chip count over time with profit/loss shading
2. **Rolling Win/Fold Rates** - 20-hand moving average of win and fold percentages
3. **Expected Value (EV)** - Average chips won/lost per hand with trend line
4. **Actions by Stage** - Bar chart of raise/call/check/fold counts at each betting round

## Model Parameters

Key ACT-R parameters in `poker-model.lisp`:

| Parameter | Value | Purpose |
|-----------|-------|---------|
| `:bll` | 0.6 | Base-level learning decay rate |
| `:ans` | 0.25 | Activation noise for retrieval variability |
| `:mp` | 2.0 | Partial matching for similar memory retrieval |
| `:rt` | -2.0 | Retrieval threshold |

## Output Interpretation

During execution, the model outputs trace messages:
- `EXPLOIT` - Acting on learned opponent pattern
- `MEMORY` - Retrieving past hand outcome
- `LEARNING` - Storing new memory
- `SURPRISING` - Storing unexpected outcome
- `NO MEMORY` - Using default exploration

## Future Work

- **Visual Attention**: Extend the model to attend to community cards during postflop stages rather than relying on Python-provided strength evaluation
- **Multi-Street Memory**: Store and retrieve memories specific to each betting round (flop, turn, river) separately
- **Bet Sizing**: Add variable bet sizing instead of fixed blind-based amounts
- **Position-Aware Strategy**: Develop distinct strategies for small blind vs big blind positions
- **Bluff Detection**: Model opponent hand strength inference from betting patterns
- **Tilt Modeling**: Add emotional state tracking that affects decision quality after bad beats
- **Multi-Table**: Extend to track opponent patterns across multiple concurrent games
- **Human Data Fitting**: Calibrate model parameters against human poker playing data

## Acknowledgments

Claude Sonnet 4.5 was used to implement the Fisher-Yates shuffle, help port the interface to Python, and write debug routines and code documentation.
