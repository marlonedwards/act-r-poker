# poker.py - Poker experiment interface for ACT-R model
# Based on friedman.py pattern

import os
import sys
import random

# actr.py file is in tutorial/python/
python_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'python'))
if python_dir not in sys.path:
    sys.path.insert(0, python_dir)

import actr

# Model path constant
MODEL_PATH = "ACT-R:tutorial;poker;poker-model.lisp"

def ensure_model_loaded():
    """Ensure the poker model is loaded and ready"""
    if not actr.current_model():
        actr.load_act_r_model(MODEL_PATH)
    return actr.current_model() is not None

def reload_model():
    """Reload the model fresh"""
    actr.load_act_r_model(MODEL_PATH)

# Load the poker model at startup
actr.load_act_r_model(MODEL_PATH)

# Card constants
RANKS = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
SUITS = ['H', 'D', 'C', 'S']

# Game state
class PokerGame:
    def __init__(self):
        self.deck = []
        self.player_chips = 1000
        self.opponent_chips = 1000
        self.pot = 0
        self.big_blind = 20
        self.player_cards = []
        self.opponent_cards = []
        self.board = []
        self.stage = 'preflop'
        self.response = ''
        self.game_over = False
        self.window = None
        # Track sequence of actions for learning
        self.action_history = []  # List of (stage, player_action, opponent_action)

    def reset_deck(self):
        """Create and shuffle a new deck"""
        self.deck = [r + s for r in RANKS for s in SUITS]
        random.shuffle(self.deck)

    def deal_card(self):
        """Deal one card from deck"""
        if not self.deck:
            self.reset_deck()
        return self.deck.pop()

    def reset_hand(self):
        """Reset for a new hand"""
        self.reset_deck()
        self.player_cards = []
        self.opponent_cards = []
        self.board = []
        self.stage = 'preflop'
        self.response = ''
        self.game_over = False
        self.pot = 0
        self.action_history = []

    def record_action(self, stage, player_action, opponent_action):
        """Record an action in the history"""
        self.action_history.append({
            'stage': stage,
            'player': player_action,
            'opponent': opponent_action,
            'pot': self.pot,
            'board': list(self.board)  # Copy current board state
        })

    def get_action_sequence_string(self):
        """Get a string representation of the action sequence"""
        if not self.action_history:
            return "no-actions"
        parts = []
        for action in self.action_history:
            parts.append(f"{action['stage'][0]}:{action['player']}/{action['opponent']}")
        return "-".join(parts)

# Global game instance
game = PokerGame()

# Opponent types - parameterized strategies
class Opponent:
    """Base opponent class"""
    def decide(self, stage, board, pot, player_action):
        return 'call'

class AlwaysCallOpponent(Opponent):
    """Opponent that always calls"""
    def decide(self, stage, board, pot, player_action):
        return 'call'

class AlwaysFoldOpponent(Opponent):
    """Opponent that always folds to any bet/raise"""
    def decide(self, stage, board, pot, player_action):
        # player_action is the key pressed: 'r', 'c', or 'f'
        if player_action == 'r':  # 'r' = raise
            return 'fold'
        return 'call'

class TightOpponent(Opponent):
    """Opponent that only plays strong hands"""
    def __init__(self):
        self.hand_strength = 'low'

    def evaluate_hand(self, cards, board):
        """Simple hand strength evaluation"""
        if not cards:
            return 'low'
        r1 = card_rank(cards[0])
        r2 = card_rank(cards[1]) if len(cards) > 1 else 0

        # Check for pairs with board
        if board:
            for bc in board:
                if card_rank(bc) == r1 or card_rank(bc) == r2:
                    return 'high'

        # High cards or pair
        if r1 == r2:
            return 'high' if r1 >= 10 else 'medium'
        if r1 >= 12 and r2 >= 12:
            return 'high'
        if r1 >= 10 and r2 >= 10:
            return 'medium'
        return 'low'

    def decide(self, stage, board, pot, player_action):
        self.hand_strength = self.evaluate_hand(game.opponent_cards, board)
        if self.hand_strength == 'low':
            return 'fold'
        elif self.hand_strength == 'medium':
            return 'call'
        else:
            # player_action is key: 'r', 'c', 'f'
            return 'raise' if player_action != 'r' else 'call'

class AggressiveOpponent(Opponent):
    """Opponent that raises frequently"""
    def decide(self, stage, board, pot, player_action):
        if random.random() < 0.6:
            return 'raise'
        return 'call'

class RandomOpponent(Opponent):
    """Opponent with random strategy"""
    def decide(self, stage, board, pot, player_action):
        actions = ['fold', 'call', 'raise']
        weights = [0.2, 0.5, 0.3]
        return random.choices(actions, weights=weights)[0]

# Available opponent types
OPPONENT_TYPES = {
    'always-call': AlwaysCallOpponent,
    'always-fold': AlwaysFoldOpponent,
    'tight': TightOpponent,
    'aggressive': AggressiveOpponent,
    'random': RandomOpponent
}

# Current opponent (can be changed as parameter)
current_opponent = AlwaysCallOpponent()

def set_opponent(opponent_type):
    """Set the opponent type for experiments"""
    global current_opponent
    if opponent_type in OPPONENT_TYPES:
        current_opponent = OPPONENT_TYPES[opponent_type]()
        print(f"Opponent set to: {opponent_type}")
    else:
        print(f"Unknown opponent type: {opponent_type}")
        print(f"Available types: {list(OPPONENT_TYPES.keys())}")

def card_rank(card):
    """Get numeric rank of card (A=14, K=13, etc.)"""
    rank_map = {'A': 14, 'K': 13, 'Q': 12, 'J': 11, 'T': 10,
                '9': 9, '8': 8, '7': 7, '6': 6, '5': 5,
                '4': 4, '3': 3, '2': 2}
    return rank_map.get(card[0], 0)

def evaluate_hand_strength(hole_cards, board):
    """Evaluate hand strength: high, medium, low"""
    if not hole_cards:
        return 'low'

    r1 = card_rank(hole_cards[0])
    r2 = card_rank(hole_cards[1]) if len(hole_cards) > 1 else 0
    is_pair = r1 == r2
    is_suited = len(hole_cards) > 1 and hole_cards[0][1] == hole_cards[1][1]

    # Check for pairs with board
    board_matches = 0
    if board:
        for bc in board:
            bcr = card_rank(bc)
            if bcr == r1 or bcr == r2:
                board_matches += 1

    if board_matches >= 2:
        return 'high'
    if board_matches == 1:
        return 'medium'

    # Preflop evaluation
    if is_pair and r1 >= 10:
        return 'high'
    if r1 >= 14 and r2 >= 12:
        return 'high'
    if is_pair:
        return 'medium'
    if is_suited and min(r1, r2) >= 10:
        return 'medium'
    if r1 >= 10 and r2 >= 10:
        return 'medium'

    return 'low'

# Layout coordinates matching poker.lisp
LAYOUT = {
    'opponent-card1': (250, 80),
    'opponent-card2': (350, 80),
    'opponent-chips': (500, 80),
    'pot': (350, 150),
    'flop1': (250, 220),
    'flop2': (330, 220),
    'flop3': (410, 220),
    'turn': (490, 220),
    'river': (570, 220),
    'player-card1': (250, 360),
    'player-card2': (350, 360),
    'player-chips': (500, 360),
    'feedback': (350, 280)
}

def respond_to_key_press(model, key):
    """Monitor callback for key presses"""
    global game
    game.response = key.lower()

def add_card_to_window(window, card, location):
    """Add a card to the window at the specified location"""
    x, y = LAYOUT[location]
    actr.add_text_to_exp_window(window, card, x=x, y=y,
                                 width=60, height=40,
                                 font_size=32, color='black')

def add_text_to_window(window, text, location, color='black', font_size=24):
    """Add text to window at location"""
    x, y = LAYOUT[location]
    actr.add_text_to_exp_window(window, text, x=x, y=y,
                                 width=100, height=30,
                                 font_size=font_size, color=color)

def display_chips(window):
    """Display current chip counts"""
    add_text_to_window(window, str(game.player_chips), 'player-chips')
    add_text_to_window(window, str(game.opponent_chips), 'opponent-chips')
    add_text_to_window(window, f"Pot:{game.pot}", 'pot', font_size=20)

def post_blinds():
    """Post blinds at start of hand"""
    game.pot = int(1.5 * game.big_blind)
    game.player_chips -= game.big_blind
    game.opponent_chips -= game.big_blind // 2

def show_feedback(window, result, outcome_text, opp_action='call'):
    """Show feedback to model after hand completes

    This updates the model's goal buffer with the result so it can learn.
    Based on how 1hit-blackjack provides feedback to the model.
    """
    color = 'green' if result == 'win' else ('red' if result == 'lose' else 'gray')
    actr.add_text_to_exp_window(window, outcome_text,
                                 x=300, y=280,
                                 width=200, height=40,
                                 font_size=28, color=color)

    # Map result and opponent action to model chunks
    result_chunk = 'win' if result == 'win' else 'lose'
    opp_chunk = f"{opp_action}-action"

    # Update goal buffer with result for learning
    # Set state to processing-feedback to trigger learning productions
    actr.mod_focus('stage', 'done', 'state', 'processing-feedback',
                   'result', result_chunk, 'opp-action', opp_chunk)

def determine_winner(player_cards, opponent_cards, board):
    """Determine hand winner (simplified)"""
    player_strength = evaluate_hand_strength(player_cards, board)
    opponent_strength = evaluate_hand_strength(opponent_cards, board)

    strength_rank = {'high': 3, 'medium': 2, 'low': 1}

    if strength_rank[player_strength] > strength_rank[opponent_strength]:
        return 'player'
    elif strength_rank[opponent_strength] > strength_rank[player_strength]:
        return 'opponent'
    else:
        # Tie - compare high cards
        p_high = max(card_rank(player_cards[0]), card_rank(player_cards[1]))
        o_high = max(card_rank(opponent_cards[0]), card_rank(opponent_cards[1]))
        if p_high > o_high:
            return 'player'
        elif o_high > p_high:
            return 'opponent'
        return 'tie'

def run_hand(visible=False):
    """Run a single poker hand"""
    global game

    # Ensure model is loaded before any ACT-R operations
    if not ensure_model_loaded():
        print("Warning: Could not load model!")
        reload_model()

    game.reset_hand()
    post_blinds()

    # Create window
    game.window = actr.open_exp_window("Poker Table", visible=visible,
                                        width=750, height=480)

    # Register key handler
    actr.add_command("poker-response", respond_to_key_press,
                     "Poker key response handler")
    actr.monitor_command("output-key", "poker-response")

    actr.install_device(game.window)

    # Deal hole cards
    game.player_cards = [game.deal_card(), game.deal_card()]
    game.opponent_cards = [game.deal_card(), game.deal_card()]

    # Show player cards
    add_card_to_window(game.window, game.player_cards[0], 'player-card1')
    add_card_to_window(game.window, game.player_cards[1], 'player-card2')

    # Show opponent as hidden
    actr.add_text_to_exp_window(game.window, "XX", x=250, y=80,
                                 width=60, height=40, font_size=32, color='gray')
    actr.add_text_to_exp_window(game.window, "XX", x=350, y=80,
                                 width=60, height=40, font_size=32, color='gray')

    display_chips(game.window)

    # Evaluate initial hand strength for preflop and signal to model
    initial_strength = evaluate_hand_strength(game.player_cards, [])

    # Calculate rank1, rank2, suited for preflop hand lookup
    r1 = card_rank(game.player_cards[0])
    r2 = card_rank(game.player_cards[1])
    # Model expects rank1 >= rank2 (high card first)
    rank1 = max(r1, r2)
    rank2 = min(r1, r2)
    # Check if suited
    is_suited = game.player_cards[0][1] == game.player_cards[1][1]
    suited_str = 'yes' if is_suited else 'no'

    # DEBUG: Show strength being passed to model
    print(f"  [DEBUG] Preflop strength: {initial_strength} for {game.player_cards} (ranks: {rank1}/{rank2}, suited: {suited_str})")
    # Set goal for model with all game state - model no longer uses visual attention for this
    # Reset learned flag to nil so model can learn this hand
    # Reset result, opp-action, opp-card1, opp-card2 for new hand
    actr.mod_focus('stage', 'preflop', 'state', 'waiting', 'strength', initial_strength,
                   'card1', game.player_cards[0], 'card2', game.player_cards[1],
                   'rank1', rank1, 'rank2', rank2, 'suited', suited_str,
                   'my-chips', game.player_chips, 'opp-chips', game.opponent_chips,
                   'pot-amount', game.pot,
                   'learned', 'nil', 'result', 'nil', 'opp-action', 'nil',
                   'opp-card1', 'nil', 'opp-card2', 'nil', 'my-action', 'nil',
                   'hand-category', 'nil')

    hand_result = None
    player_folded = False
    opponent_folded = False
    opponent_action = 'call'  # Track last opponent action for learning

    # Game loop through stages
    stages = ['preflop', 'flop', 'turn', 'river']

    for stage_idx, stage in enumerate(stages):
        game.stage = stage
        game.response = ''

        # Deal community cards for this stage
        if stage == 'flop':
            game.board = [game.deal_card(), game.deal_card(), game.deal_card()]
            add_card_to_window(game.window, game.board[0], 'flop1')
            add_card_to_window(game.window, game.board[1], 'flop2')
            add_card_to_window(game.window, game.board[2], 'flop3')
            # Evaluate hand strength with flop
            flop_strength = evaluate_hand_strength(game.player_cards, game.board)
            print(f"  [DEBUG] Flop strength: {flop_strength} | Board: {game.board}")
            # Signal model with updated game state
            actr.mod_focus('stage', 'flop', 'state', 'waiting', 'strength', flop_strength,
                           'my-chips', game.player_chips, 'opp-chips', game.opponent_chips,
                           'pot-amount', game.pot)
        elif stage == 'turn':
            game.board.append(game.deal_card())
            add_card_to_window(game.window, game.board[3], 'turn')
            # Evaluate hand strength with turn
            turn_strength = evaluate_hand_strength(game.player_cards, game.board)
            print(f"  [DEBUG] Turn strength: {turn_strength} | Board: {game.board}")
            # Signal model with updated game state
            actr.mod_focus('stage', 'turn', 'state', 'waiting', 'strength', turn_strength,
                           'my-chips', game.player_chips, 'opp-chips', game.opponent_chips,
                           'pot-amount', game.pot)
        elif stage == 'river':
            game.board.append(game.deal_card())
            add_card_to_window(game.window, game.board[4], 'river')
            # Evaluate hand strength with river
            river_strength = evaluate_hand_strength(game.player_cards, game.board)
            print(f"  [DEBUG] River strength: {river_strength} | Board: {game.board}")
            # Signal model with updated game state
            actr.mod_focus('stage', 'river', 'state', 'waiting', 'strength', river_strength,
                           'my-chips', game.player_chips, 'opp-chips', game.opponent_chips,
                           'pot-amount', game.pot)

        # Wait for model response
        actr.run(10)

        player_action = game.response
        # DEBUG: Show what action the model took
        action_map = {'f': 'FOLD', 'c': 'CALL', 'r': 'RAISE', '': 'NO RESPONSE'}
        print(f"  [DEBUG] {stage}: Model action = {action_map.get(player_action, player_action)}")

        if player_action == 'f':
            # Record fold action
            game.record_action(stage, 'fold', None)
            player_folded = True
            hand_result = 'fold'
            break

        # Get opponent response
        opponent_action = current_opponent.decide(stage, game.board,
                                                   game.pot, player_action)

        # Record the action pair
        game.record_action(stage, player_action, opponent_action)

        if opponent_action == 'fold':
            opponent_folded = True
            hand_result = 'win'
            break

        # Update pot for calls/raises
        if player_action == 'r':
            game.pot += game.big_blind * 2
            game.player_chips -= game.big_blind
        elif player_action == 'c':
            game.pot += game.big_blind
            game.player_chips -= game.big_blind // 2

    # Determine outcome
    opponent_action_for_learning = 'call'  # Default
    went_to_showdown = False

    if player_folded:
        hand_result = 'fold'
        game.opponent_chips += game.pot
        outcome_text = "You folded"
        feedback_result = 'lose'
        opponent_action_for_learning = 'call'  # Opponent would have called
    elif opponent_folded:
        hand_result = 'win'
        game.player_chips += game.pot
        outcome_text = "Opponent folded - You win!"
        feedback_result = 'win'
        opponent_action_for_learning = 'fold'
    else:
        # SHOWDOWN - both players called to the end
        went_to_showdown = True
        opponent_action_for_learning = opponent_action if opponent_action else 'call'

        # Reveal opponent cards at showdown
        actr.clear_exp_window(game.window)

        # Re-display the board
        if game.board:
            add_card_to_window(game.window, game.board[0], 'flop1')
            add_card_to_window(game.window, game.board[1], 'flop2')
            add_card_to_window(game.window, game.board[2], 'flop3')
            if len(game.board) > 3:
                add_card_to_window(game.window, game.board[3], 'turn')
            if len(game.board) > 4:
                add_card_to_window(game.window, game.board[4], 'river')

        # Show player's cards
        add_card_to_window(game.window, game.player_cards[0], 'player-card1')
        add_card_to_window(game.window, game.player_cards[1], 'player-card2')

        # REVEAL opponent cards (they were hidden, now visible)
        add_card_to_window(game.window, game.opponent_cards[0], 'opponent-card1')
        add_card_to_window(game.window, game.opponent_cards[1], 'opponent-card2')

        # Signal model that showdown is happening - it will use visual attention here
        actr.mod_focus('stage', 'showdown', 'state', 'waiting')

        # Let model attend to opponent's revealed cards
        actr.run(2.0)

        # Determine winner
        winner = determine_winner(game.player_cards, game.opponent_cards, game.board)
        if winner == 'player':
            hand_result = 'win'
            game.player_chips += game.pot
            outcome_text = "You win!"
            feedback_result = 'win'
        elif winner == 'opponent':
            hand_result = 'lose'
            game.opponent_chips += game.pot
            outcome_text = "Opponent wins"
            feedback_result = 'lose'
        else:
            hand_result = 'tie'
            game.player_chips += game.pot // 2
            game.opponent_chips += game.pot // 2
            outcome_text = "Tie - split pot"
            feedback_result = 'tie'

    # Show feedback to model and update goal for learning
    actr.clear_exp_window(game.window)
    show_feedback(game.window, feedback_result, outcome_text, opponent_action_for_learning)

    # Let model process feedback
    actr.run_full_time(1.0)

    # Cleanup
    actr.remove_command_monitor("output-key", "poker-response")
    actr.remove_command("poker-response")

    return {
        'result': hand_result,
        'player_chips': game.player_chips,
        'opponent_chips': game.opponent_chips,
        'player_cards': game.player_cards,
        'opponent_cards': game.opponent_cards,
        'board': game.board,
        'final_stage': game.stage,
        'player_action': game.response,
        'hand_strength': evaluate_hand_strength(game.player_cards, game.board),
        'went_to_showdown': went_to_showdown,
        'action_sequence': game.get_action_sequence_string(),
        'action_history': list(game.action_history)  # Full action history
    }


def run_session(n_hands=10, opponent_type='always-call',
                win_limit=2000, lose_limit=100, visible=False):
    """Run a session of poker hands

    Args:
        n_hands: Maximum number of hands to play
        opponent_type: Type of opponent ('always-call', 'always-fold', 'tight', etc.)
        win_limit: Stop if player reaches this many chips
        lose_limit: Stop if player falls below this many chips
        visible: Whether to show the window
    """
    global game

    set_opponent(opponent_type)

    results = []
    wins = 0
    losses = 0
    folds = 0

    game.player_chips = 1000
    game.opponent_chips = 1000

    print(f"\n=== Poker Session: {n_hands} hands vs {opponent_type} ===\n")

    # Reset ONCE at start of session to clear any previous state
    # but preserve memory across hands within the session
    actr.reset()
    if not actr.current_model():
        reload_model()

    for hand_num in range(n_hands):
        # Check chip limits
        if game.player_chips >= win_limit:
            print(f"\nPlayer reached win limit ({win_limit} chips)!")
            break
        if game.player_chips <= lose_limit:
            print(f"\nPlayer fell below lose limit ({lose_limit} chips)!")
            break
        if game.player_chips < game.big_blind:
            print(f"\nPlayer cannot afford blind!")
            break

        # DON'T reset model between hands - we need memory to persist!
        # Just reset the goal state for the new hand
        # The mod_focus in run_hand() will set up the new hand state

        print(f"Hand {hand_num + 1}: ", end='')

        result = run_hand(visible=visible)
        results.append(result)

        if result['result'] == 'win':
            wins += 1
            print(f"WIN  - {result['player_cards']} vs {result['opponent_cards']} "
                  f"Board: {result['board']} - Chips: {result['player_chips']}")
        elif result['result'] == 'lose':
            losses += 1
            print(f"LOSE - {result['player_cards']} vs {result['opponent_cards']} "
                  f"Board: {result['board']} - Chips: {result['player_chips']}")
        elif result['result'] == 'fold':
            folds += 1
            strength = result['hand_strength']
            print(f"FOLD ({strength}) - {result['player_cards']} "
                  f"- Chips: {result['player_chips']}")
        else:
            print(f"TIE  - Chips: {result['player_chips']}")

    # Summary
    print(f"\n=== Session Summary ===")
    print(f"Hands played: {len(results)}")
    print(f"Wins: {wins}, Losses: {losses}, Folds: {folds}")
    print(f"Final chips: {game.player_chips}")
    print(f"Net profit: {game.player_chips - 1000}")

    if results:
        win_rate = wins / len(results) * 100
        print(f"Win rate: {win_rate:.1f}%")

    return results


def run_experiment(n_sessions=10, n_hands=50, opponent_type='always-call'):
    """Run multiple sessions and aggregate results

    Args:
        n_sessions: Number of sessions to run
        n_hands: Hands per session
        opponent_type: Type of opponent to play against
    """
    all_results = []
    total_profit = 0
    total_wins = 0
    total_losses = 0
    total_folds = 0
    total_hands = 0

    print(f"\n{'='*50}")
    print(f"POKER EXPERIMENT: {n_sessions} sessions x {n_hands} hands")
    print(f"Opponent: {opponent_type}")
    print(f"{'='*50}\n")

    for session in range(n_sessions):
        print(f"\n--- Session {session + 1}/{n_sessions} ---")
        actr.reset()

        game.player_chips = 1000
        game.opponent_chips = 1000

        results = run_session(n_hands=n_hands, opponent_type=opponent_type,
                              visible=False)
        all_results.extend(results)

        for r in results:
            if r['result'] == 'win':
                total_wins += 1
            elif r['result'] == 'lose':
                total_losses += 1
            elif r['result'] == 'fold':
                total_folds += 1

        total_profit += (game.player_chips - 1000)
        total_hands += len(results)

    # Final summary
    print(f"\n{'='*50}")
    print(f"EXPERIMENT SUMMARY")
    print(f"{'='*50}")
    print(f"Total hands: {total_hands}")
    print(f"Total wins: {total_wins} ({total_wins/total_hands*100:.1f}%)")
    print(f"Total losses: {total_losses} ({total_losses/total_hands*100:.1f}%)")
    print(f"Total folds: {total_folds} ({total_folds/total_hands*100:.1f}%)")
    print(f"Average profit per session: {total_profit/n_sessions:.0f} chips")

    return all_results


def compare_opponents(n_hands=20):
    """Compare model performance against different opponent types"""
    opponent_types = ['always-call', 'always-fold', 'tight', 'aggressive', 'random']

    results_summary = {}

    print("\n" + "="*60)
    print("OPPONENT COMPARISON EXPERIMENT")
    print("="*60)

    for opp_type in opponent_types:
        print(f"\n>>> Testing against: {opp_type}")
        actr.reset()
        game.player_chips = 1000
        game.opponent_chips = 1000

        results = run_session(n_hands=n_hands, opponent_type=opp_type, visible=False)

        wins = sum(1 for r in results if r['result'] == 'win')
        folds = sum(1 for r in results if r['result'] == 'fold')

        results_summary[opp_type] = {
            'hands': len(results),
            'wins': wins,
            'folds': folds,
            'final_chips': game.player_chips,
            'profit': game.player_chips - 1000
        }

    # Print comparison
    print("\n" + "="*60)
    print("COMPARISON RESULTS")
    print("="*60)
    print(f"{'Opponent':<15} {'Hands':<8} {'Wins':<8} {'Folds':<8} {'Profit':<10}")
    print("-"*60)

    for opp_type, data in results_summary.items():
        print(f"{opp_type:<15} {data['hands']:<8} {data['wins']:<8} "
              f"{data['folds']:<8} {data['profit']:<10}")

    return results_summary


def person(opponent_type='always-call'):
    """Run interactive game for human player - full hand with visual stepping

    Similar to friedman.py person() function, this allows you to visually
    step through the experiment in ACT-R.

    Controls:
        'f' - Fold
        'c' - Call/Check
        'r' - Raise
    """
    global game

    if not actr.visible_virtuals_available():
        print("No visible virtual windows available")
        print("Make sure ACT-R environment is running with visible windows enabled")
        return None

    set_opponent(opponent_type)
    game.reset_hand()
    post_blinds()

    game.window = actr.open_exp_window("Poker Table - Human Player", visible=True,
                                        width=750, height=480)

    actr.add_command("poker-response", respond_to_key_press,
                     "Poker key response handler")
    actr.monitor_command("output-key", "poker-response")

    # Deal hole cards
    game.player_cards = [game.deal_card(), game.deal_card()]
    game.opponent_cards = [game.deal_card(), game.deal_card()]

    # Show player cards
    add_card_to_window(game.window, game.player_cards[0], 'player-card1')
    add_card_to_window(game.window, game.player_cards[1], 'player-card2')

    # Show opponent as hidden
    actr.add_text_to_exp_window(game.window, "XX", x=250, y=80,
                                 width=60, height=40, font_size=32, color='gray')
    actr.add_text_to_exp_window(game.window, "XX", x=350, y=80,
                                 width=60, height=40, font_size=32, color='gray')

    display_chips(game.window)

    hand_result = None
    player_folded = False
    opponent_folded = False
    opponent_action = 'call'

    stages = ['preflop', 'flop', 'turn', 'river']

    for stage_idx, stage in enumerate(stages):
        game.stage = stage
        game.response = ''

        # Deal community cards for this stage
        if stage == 'flop':
            game.board = [game.deal_card(), game.deal_card(), game.deal_card()]
            add_card_to_window(game.window, game.board[0], 'flop1')
            add_card_to_window(game.window, game.board[1], 'flop2')
            add_card_to_window(game.window, game.board[2], 'flop3')
        elif stage == 'turn':
            game.board.append(game.deal_card())
            add_card_to_window(game.window, game.board[3], 'turn')
        elif stage == 'river':
            game.board.append(game.deal_card())
            add_card_to_window(game.window, game.board[4], 'river')

        # Show current stage and hand strength
        strength = evaluate_hand_strength(game.player_cards, game.board)
        print(f"\n=== {stage.upper()} ===")
        print(f"Your cards: {game.player_cards}")
        if game.board:
            print(f"Board: {game.board}")
        print(f"Hand strength: {strength}")
        print(f"Pot: {game.pot}")
        print("Press 'f' to fold, 'c' to call, 'r' to raise")

        # Wait for human input
        while game.response == '':
            actr.process_events()

        player_action = game.response.lower()
        print(f"You chose: {player_action}")

        if player_action == 'f':
            game.record_action(stage, 'fold', None)
            player_folded = True
            hand_result = 'fold'
            break

        # Get opponent response
        opponent_action = current_opponent.decide(stage, game.board,
                                                   game.pot, player_action)
        print(f"Opponent: {opponent_action}")

        game.record_action(stage, player_action, opponent_action)

        if opponent_action == 'fold':
            opponent_folded = True
            hand_result = 'win'
            break

        # Update pot
        if player_action == 'r':
            game.pot += game.big_blind * 2
            game.player_chips -= game.big_blind
        elif player_action == 'c':
            game.pot += game.big_blind
            game.player_chips -= game.big_blind // 2

        # Brief pause to show opponent action
        start_time = actr.get_time(False)
        while (actr.get_time(False) - start_time) < 500:
            actr.process_events()

    # Determine outcome
    if player_folded:
        game.opponent_chips += game.pot
        outcome_text = "You folded - Opponent wins"
        feedback_result = 'lose'
        print(f"\n{outcome_text}")
    elif opponent_folded:
        game.player_chips += game.pot
        outcome_text = "Opponent folded - You win!"
        feedback_result = 'win'
        print(f"\n{outcome_text}")
    else:
        # SHOWDOWN
        print("\n=== SHOWDOWN ===")
        print(f"Your cards: {game.player_cards}")
        print(f"Opponent cards: {game.opponent_cards}")
        print(f"Board: {game.board}")

        # Reveal opponent cards
        actr.clear_exp_window(game.window)

        # Re-display board
        if game.board:
            add_card_to_window(game.window, game.board[0], 'flop1')
            add_card_to_window(game.window, game.board[1], 'flop2')
            add_card_to_window(game.window, game.board[2], 'flop3')
            if len(game.board) > 3:
                add_card_to_window(game.window, game.board[3], 'turn')
            if len(game.board) > 4:
                add_card_to_window(game.window, game.board[4], 'river')

        # Show both players' cards
        add_card_to_window(game.window, game.player_cards[0], 'player-card1')
        add_card_to_window(game.window, game.player_cards[1], 'player-card2')
        add_card_to_window(game.window, game.opponent_cards[0], 'opponent-card1')
        add_card_to_window(game.window, game.opponent_cards[1], 'opponent-card2')

        winner = determine_winner(game.player_cards, game.opponent_cards, game.board)

        if winner == 'player':
            hand_result = 'win'
            game.player_chips += game.pot
            outcome_text = "You win!"
            feedback_result = 'win'
        elif winner == 'opponent':
            hand_result = 'lose'
            game.opponent_chips += game.pot
            outcome_text = "Opponent wins"
            feedback_result = 'lose'
        else:
            hand_result = 'tie'
            game.player_chips += game.pot // 2
            game.opponent_chips += game.pot // 2
            outcome_text = "Tie - split pot"
            feedback_result = 'tie'

        print(f"\n{outcome_text}")
        print(f"Your strength: {evaluate_hand_strength(game.player_cards, game.board)}")
        print(f"Opponent strength: {evaluate_hand_strength(game.opponent_cards, game.board)}")

    # Show feedback
    color = 'green' if feedback_result == 'win' else ('red' if feedback_result == 'lose' else 'gray')
    actr.add_text_to_exp_window(game.window, outcome_text,
                                 x=300, y=280,
                                 width=200, height=40,
                                 font_size=28, color=color)

    print(f"\nFinal chips: {game.player_chips}")
    print(f"Net: {game.player_chips - 1000}")

    # Wait for user to see result
    print("\nPress any key to close...")
    game.response = ''
    while game.response == '':
        actr.process_events()

    actr.remove_command_monitor("output-key", "poker-response")
    actr.remove_command("poker-response")

    return {
        'result': hand_result,
        'player_chips': game.player_chips,
        'opponent_chips': game.opponent_chips,
        'player_cards': game.player_cards,
        'opponent_cards': game.opponent_cards,
        'board': game.board
    }


def person_session(n_hands=5, opponent_type='always-call'):
    """Run multiple hands interactively

    Similar to run_session but for human player with visual display.
    """
    global game

    if not actr.visible_virtuals_available():
        print("No visible virtual windows available")
        return None

    game.player_chips = 1000
    game.opponent_chips = 1000

    results = []

    print(f"\n{'='*50}")
    print(f"INTERACTIVE POKER SESSION")
    print(f"Playing {n_hands} hands against {opponent_type}")
    print(f"{'='*50}")

    for hand_num in range(n_hands):
        print(f"\n>>> HAND {hand_num + 1} of {n_hands}")
        print(f"Your chips: {game.player_chips}")

        if game.player_chips < game.big_blind:
            print("You're out of chips!")
            break

        result = person(opponent_type)
        if result:
            results.append(result)
        else:
            break

    # Summary
    print(f"\n{'='*50}")
    print("SESSION SUMMARY")
    print(f"{'='*50}")
    wins = sum(1 for r in results if r['result'] == 'win')
    losses = sum(1 for r in results if r['result'] == 'lose')
    folds = sum(1 for r in results if r['result'] == 'fold')
    print(f"Hands: {len(results)}")
    print(f"Wins: {wins}, Losses: {losses}, Folds: {folds}")
    print(f"Final chips: {game.player_chips}")
    print(f"Net profit: {game.player_chips - 1000}")

    return results


if __name__ == "__main__":
    # Default experiment
    n_hands = 20
    opponent = 'always-call'

    if len(sys.argv) > 1:
        n_hands = int(sys.argv[1])
    if len(sys.argv) > 2:
        opponent = sys.argv[2]

    print(f"Running poker experiment: {n_hands} hands vs {opponent}")
    run_session(n_hands=n_hands, opponent_type=opponent)
