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
        # Position tracking: 'sb' (small blind) or 'bb' (big blind)
        # In heads-up, SB acts first preflop, BB acts first postflop
        self.position = 'bb'  # Start as big blind (default)
        self.hand_number = 0  # Track hand number for alternating positions

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
        # Cognitive process tracking
        self.cognitive_trace = []  # List of (stage, retrieval_type, retrieval_result, production)
        # Alternate position each hand
        self.hand_number += 1
        self.position = 'sb' if self.hand_number % 2 == 1 else 'bb'

    def record_cognitive_event(self, stage, retrieval_type, retrieval_result, production):
        """Record a cognitive event for analysis.

        Args:
            stage: 'preflop', 'flop', 'turn', 'river'
            retrieval_type: 'opponent_pattern' or 'hand_memory'
            retrieval_result: 'success', 'failure', 'wrong_type'
            production: Name of production that fired
        """
        self.cognitive_trace.append({
            'stage': stage,
            'retrieval_type': retrieval_type,
            'retrieval_result': retrieval_result,
            'production': production
        })

    def get_pot_odds(self):
        """Calculate pot odds category for decision making

        Returns: 'odds-good' (>2:1), 'odds-neutral' (1:1-2:1), 'odds-bad' (<1:1)
        Based on pot size vs bet size (big blind)
        """
        if self.pot <= 0:
            return 'odds-neutral'

        # Pot odds = pot / bet_to_call
        # Simplified: use big_blind as standard bet size
        bet_to_call = self.big_blind
        pot_odds_ratio = self.pot / bet_to_call

        if pot_odds_ratio >= 3:
            return 'odds-good'      # Great odds, worth calling/betting
        elif pot_odds_ratio >= 1.5:
            return 'odds-neutral'   # Marginal
        else:
            return 'odds-bad'       # Poor odds

    def get_stack_status(self):
        """Get stack size status relative to blinds

        Returns: 'stack-deep' (>50bb), 'stack-medium' (20-50bb), 'stack-short' (<20bb)
        """
        bb_count = self.player_chips / self.big_blind
        if bb_count >= 50:
            return 'stack-deep'
        elif bb_count >= 20:
            return 'stack-medium'
        else:
            return 'stack-short'

    def get_opponent_card_info(self):
        """Get opponent card ranks and suited status for memory storage"""
        if len(self.opponent_cards) < 2:
            return None, None, None
        r1 = card_rank(self.opponent_cards[0])
        r2 = card_rank(self.opponent_cards[1])
        # Model expects rank1 >= rank2 (high card first)
        opp_rank1 = max(r1, r2)
        opp_rank2 = min(r1, r2)
        # Check if suited
        opp_suited = 'yes' if self.opponent_cards[0][1] == self.opponent_cards[1][1] else 'no'
        return opp_rank1, opp_rank2, opp_suited

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
    """Opponent that always calls (passive calling station)"""
    def decide(self, stage, board, pot, player_action):
        # If player checks, we check back (nothing to call)
        # If player raises, we call
        if player_action == 'k':
            return 'check'
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
        self.hand_strength = 'strength-low'

    def evaluate_hand(self, cards, board):
        """Simple hand strength evaluation"""
        if not cards:
            return 'strength-low'
        r1 = card_rank(cards[0])
        r2 = card_rank(cards[1]) if len(cards) > 1 else 0

        # Check for pairs with board
        if board:
            for bc in board:
                if card_rank(bc) == r1 or card_rank(bc) == r2:
                    return 'strength-high'

        # High cards or pair
        if r1 == r2:
            return 'strength-high' if r1 >= 10 else 'medium'
        if r1 >= 12 and r2 >= 12:
            return 'strength-high'
        if r1 >= 10 and r2 >= 10:
            return 'strength-medium'
        return 'strength-low'

    def decide(self, stage, board, pot, player_action):
        self.hand_strength = self.evaluate_hand(game.opponent_cards, board)
        if self.hand_strength == 'strength-low':
            return 'fold'
        elif self.hand_strength == 'strength-medium':
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

def card_suit(card):
    """Get suit of card"""
    return card[1] if len(card) > 1 else None


# ============================================
# FULL POKER HAND EVALUATOR
# ============================================

# Hand rankings (higher = better)
HAND_RANKS = {
    'high_card': 1,
    'pair': 2,
    'two_pair': 3,
    'three_of_a_kind': 4,
    'straight': 5,
    'flush': 6,
    'full_house': 7,
    'four_of_a_kind': 8,
    'straight_flush': 9,
    'royal_flush': 10
}


def get_all_cards(hole_cards, board):
    """Combine hole cards and board into a single list"""
    all_cards = list(hole_cards) if hole_cards else []
    if board:
        all_cards.extend(board)
    return all_cards


def count_ranks(cards):
    """Count occurrences of each rank"""
    counts = {}
    for card in cards:
        r = card_rank(card)
        counts[r] = counts.get(r, 0) + 1
    return counts


def count_suits(cards):
    """Count occurrences of each suit"""
    counts = {}
    for card in cards:
        s = card_suit(card)
        counts[s] = counts.get(s, 0) + 1
    return counts


def has_flush(cards):
    """Check if there's a flush (5+ cards of same suit)"""
    suit_counts = count_suits(cards)
    for suit, count in suit_counts.items():
        if count >= 5:
            # Return the flush cards sorted by rank
            flush_cards = [c for c in cards if card_suit(c) == suit]
            flush_cards.sort(key=lambda c: card_rank(c), reverse=True)
            return flush_cards[:5]
    return None


def has_straight(cards):
    """Check if there's a straight (5 consecutive ranks)"""
    ranks = sorted(set(card_rank(c) for c in cards), reverse=True)

    # Check for A-2-3-4-5 (wheel)
    if 14 in ranks and 2 in ranks and 3 in ranks and 4 in ranks and 5 in ranks:
        return [5, 4, 3, 2, 14]  # Wheel, 5-high straight

    # Check for regular straights
    for i in range(len(ranks) - 4):
        if ranks[i] - ranks[i+4] == 4:
            # Found 5 consecutive ranks
            straight_high = ranks[i]
            return list(range(straight_high, straight_high - 5, -1))

    return None


def has_straight_flush(cards):
    """Check for straight flush (straight + flush in same suit)"""
    suit_counts = count_suits(cards)
    for suit, count in suit_counts.items():
        if count >= 5:
            suited_cards = [c for c in cards if card_suit(c) == suit]
            straight = has_straight(suited_cards)
            if straight:
                return straight
    return None


def evaluate_made_hand(cards):
    """
    Evaluate the best 5-card hand from available cards.
    Returns (hand_type, hand_rank, kickers)
    """
    if len(cards) < 5:
        # Not enough cards for a full hand evaluation
        return ('high_card', 1, sorted([card_rank(c) for c in cards], reverse=True))

    rank_counts = count_ranks(cards)
    ranks_by_count = {}
    for rank, count in rank_counts.items():
        if count not in ranks_by_count:
            ranks_by_count[count] = []
        ranks_by_count[count].append(rank)

    # Sort each count group by rank (highest first)
    for count in ranks_by_count:
        ranks_by_count[count].sort(reverse=True)

    # Check for straight flush / royal flush
    sf = has_straight_flush(cards)
    if sf:
        if sf[0] == 14:  # A-high straight flush = royal flush
            return ('royal_flush', 10, sf)
        return ('straight_flush', 9, sf)

    # Check for four of a kind
    if 4 in ranks_by_count:
        quad_rank = ranks_by_count[4][0]
        kicker = max(r for r in rank_counts.keys() if r != quad_rank)
        return ('four_of_a_kind', 8, [quad_rank, kicker])

    # Check for full house
    if 3 in ranks_by_count and (2 in ranks_by_count or len(ranks_by_count.get(3, [])) > 1):
        trip_rank = ranks_by_count[3][0]
        if len(ranks_by_count.get(3, [])) > 1:
            pair_rank = ranks_by_count[3][1]  # Second trips as pair
        else:
            pair_rank = ranks_by_count[2][0]
        return ('full_house', 7, [trip_rank, pair_rank])

    # Check for flush
    flush = has_flush(cards)
    if flush:
        return ('flush', 6, [card_rank(c) for c in flush])

    # Check for straight
    straight = has_straight(cards)
    if straight:
        return ('straight', 5, straight)

    # Check for three of a kind
    if 3 in ranks_by_count:
        trip_rank = ranks_by_count[3][0]
        kickers = sorted([r for r in rank_counts.keys() if r != trip_rank], reverse=True)[:2]
        return ('three_of_a_kind', 4, [trip_rank] + kickers)

    # Check for two pair
    if 2 in ranks_by_count and len(ranks_by_count[2]) >= 2:
        pairs = ranks_by_count[2][:2]
        kicker = max(r for r in rank_counts.keys() if r not in pairs)
        return ('two_pair', 3, pairs + [kicker])

    # Check for one pair
    if 2 in ranks_by_count:
        pair_rank = ranks_by_count[2][0]
        kickers = sorted([r for r in rank_counts.keys() if r != pair_rank], reverse=True)[:3]
        return ('pair', 2, [pair_rank] + kickers)

    # High card
    kickers = sorted(rank_counts.keys(), reverse=True)[:5]
    return ('high_card', 1, kickers)


def count_flush_draw(cards):
    """Count cards toward a flush (returns max suited count)"""
    suit_counts = count_suits(cards)
    return max(suit_counts.values()) if suit_counts else 0


def count_straight_draw(cards):
    """
    Check for straight draws.
    Returns: 'oesd' (open-ended), 'gutshot', or None
    """
    ranks = sorted(set(card_rank(c) for c in cards))

    # Check for open-ended straight draw (4 consecutive)
    for i in range(len(ranks) - 3):
        if ranks[i+3] - ranks[i] == 3:
            # 4 consecutive cards
            # Check if it's open-ended (not at edges)
            if ranks[i] > 2 and ranks[i+3] < 14:
                return 'oesd'

    # Check for gutshot (4 cards with one gap)
    for i in range(len(ranks) - 3):
        if ranks[i+3] - ranks[i] == 4:
            # Could be a gutshot (one gap in 5 cards)
            return 'gutshot'

    # Check for wheel draw (A-2-3-4 or similar)
    wheel_ranks = [14, 2, 3, 4, 5]
    wheel_count = sum(1 for r in ranks if r in wheel_ranks)
    if wheel_count >= 4:
        return 'oesd' if wheel_count == 4 else None

    return None


def evaluate_hand_strength(hole_cards, board):
    """Evaluate hand strength: high, medium, low

    Uses full poker hand evaluation when board is available.
    Considers made hands, draws, and position.
    """
    if not hole_cards:
        return 'strength-low'

    r1 = card_rank(hole_cards[0])
    r2 = card_rank(hole_cards[1]) if len(hole_cards) > 1 else 0
    high_card = max(r1, r2)
    low_card = min(r1, r2)
    is_pair = r1 == r2
    is_suited = len(hole_cards) > 1 and hole_cards[0][1] == hole_cards[1][1]
    gap = high_card - low_card

    # === POSTFLOP: Use full hand evaluation ===
    if board and len(board) >= 3:
        all_cards = get_all_cards(hole_cards, board)
        hand_type, hand_rank, kickers = evaluate_made_hand(all_cards)

        # Check for draws (important for decision making)
        flush_count = count_flush_draw(all_cards)
        straight_draw = count_straight_draw(all_cards)

        # HIGH: Strong made hands
        if hand_rank >= 6:  # Flush or better
            return 'strength-high'
        if hand_type == 'straight':
            return 'strength-high'
        if hand_type == 'three_of_a_kind':
            return 'strength-high'
        if hand_type == 'two_pair':
            # Two pair strength depends on the pairs
            if kickers[0] >= 10:  # Top pair is T or higher
                return 'strength-high'
            return 'strength-medium'

        # MEDIUM: Decent made hands or strong draws
        if hand_type == 'pair':
            pair_rank = kickers[0]
            # Top pair or overpair
            board_ranks = [card_rank(c) for c in board]
            max_board = max(board_ranks)
            if pair_rank >= max_board:  # Overpair or top pair
                if pair_rank >= 10:
                    return 'strength-high'
                return 'strength-medium'
            # Second pair or pocket pair below top card
            if pair_rank >= max_board - 2:  # Within 2 ranks of top board card
                return 'strength-medium'
            # Any pair with decent kicker (using hole cards)
            if high_card >= 10:  # Our high card is T+
                return 'strength-medium'
            # Small pairs are still playable in heads-up
            if pair_rank >= 5:
                return 'strength-medium'
            return 'strength-low'

        # Strong draws count as medium
        if flush_count >= 4:  # Flush draw
            return 'strength-medium'
        if straight_draw == 'oesd':  # Open-ended straight draw
            return 'strength-medium'

        # High card hands
        if hand_type == 'high_card':
            # Check if we have overcards to the board
            board_high = max(card_rank(c) for c in board)
            if high_card > board_high and low_card > board_high:
                return 'strength-medium'  # Two overcards
            if high_card > board_high and high_card >= 12:
                return 'strength-medium'  # One strong overcard
            # Gutshot with overcards
            if straight_draw == 'gutshot' and high_card > board_high:
                return 'strength-medium'
            return 'strength-low'

        return 'strength-low'

    # === PREFLOP EVALUATION (more generous for heads-up) ===

    # HIGH: Premium hands
    if is_pair and r1 >= 9:  # 99+ is high
        return 'strength-high'
    if high_card == 14 and low_card >= 10:  # AT+ is high
        return 'strength-high'
    if high_card == 14 and is_suited:  # Any suited Ace is high
        return 'strength-high'
    if high_card >= 13 and low_card >= 12:  # KQ+ is high
        return 'strength-high'

    # MEDIUM: Playable hands
    if is_pair:  # Any pair is medium
        return 'strength-medium'
    if high_card == 14:  # Any Ace is at least medium
        return 'strength-medium'
    if high_card == 13 and low_card >= 8:  # K8+ is medium
        return 'strength-medium'
    if high_card == 13 and is_suited:  # Any suited King is medium
        return 'strength-medium'
    if high_card >= 10 and low_card >= 10:  # Two Broadway cards
        return 'strength-medium'
    if is_suited and gap <= 2 and low_card >= 5:  # Suited connectors 56s+
        return 'strength-medium'
    if is_suited and high_card >= 10:  # Suited with a Broadway card
        return 'strength-medium'
    if high_card == 12 and low_card >= 8:  # Q8+ is medium
        return 'strength-medium'

    # LOW: Marginal/trash - but still check for some playability
    if high_card >= 9 and low_card >= 7:  # Connected high cards
        return 'strength-medium'

    return 'strength-low'

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
    """Post blinds at start of hand based on position.

    In heads-up poker:
    - Small blind (SB) posts half the big blind
    - Big blind (BB) posts the full big blind
    - Position alternates each hand
    """
    sb_amount = game.big_blind // 2  # 10
    bb_amount = game.big_blind       # 20

    if game.position == 'sb':
        # Player is small blind, opponent is big blind
        game.player_chips -= sb_amount
        game.opponent_chips -= bb_amount
    else:
        # Player is big blind, opponent is small blind
        game.player_chips -= bb_amount
        game.opponent_chips -= sb_amount

    game.pot = sb_amount + bb_amount  # 30 total

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

def compare_hands(hand1_eval, hand2_eval):
    """
    Compare two hand evaluations.
    Returns: 1 if hand1 wins, -1 if hand2 wins, 0 if tie
    """
    type1, rank1, kickers1 = hand1_eval
    type2, rank2, kickers2 = hand2_eval

    # Compare hand ranks first
    if rank1 > rank2:
        return 1
    elif rank2 > rank1:
        return -1

    # Same hand rank - compare kickers
    for k1, k2 in zip(kickers1, kickers2):
        if k1 > k2:
            return 1
        elif k2 > k1:
            return -1

    return 0  # Tie


def determine_winner(player_cards, opponent_cards, board):
    """Determine hand winner using full poker hand evaluation"""
    # Get all cards for each player
    player_all = get_all_cards(player_cards, board)
    opponent_all = get_all_cards(opponent_cards, board)

    # Evaluate both hands
    player_eval = evaluate_made_hand(player_all)
    opponent_eval = evaluate_made_hand(opponent_all)

    # Compare
    result = compare_hands(player_eval, opponent_eval)

    if result > 0:
        return 'player'
    elif result < 0:
        return 'opponent'
    return 'tie'


def get_hand_description(cards):
    """Get a human-readable description of the best hand"""
    hand_type, rank, kickers = evaluate_made_hand(cards)

    rank_names = {14: 'A', 13: 'K', 12: 'Q', 11: 'J', 10: 'T',
                  9: '9', 8: '8', 7: '7', 6: '6', 5: '5', 4: '4', 3: '3', 2: '2'}

    if hand_type == 'royal_flush':
        return 'Royal Flush'
    elif hand_type == 'straight_flush':
        return f'Straight Flush ({rank_names.get(kickers[0], kickers[0])}-high)'
    elif hand_type == 'four_of_a_kind':
        return f'Four {rank_names.get(kickers[0], kickers[0])}s'
    elif hand_type == 'full_house':
        return f'Full House ({rank_names.get(kickers[0], kickers[0])}s full of {rank_names.get(kickers[1], kickers[1])}s)'
    elif hand_type == 'flush':
        return f'Flush ({rank_names.get(kickers[0], kickers[0])}-high)'
    elif hand_type == 'straight':
        return f'Straight ({rank_names.get(kickers[0], kickers[0])}-high)'
    elif hand_type == 'three_of_a_kind':
        return f'Three {rank_names.get(kickers[0], kickers[0])}s'
    elif hand_type == 'two_pair':
        return f'Two Pair ({rank_names.get(kickers[0], kickers[0])}s and {rank_names.get(kickers[1], kickers[1])}s)'
    elif hand_type == 'pair':
        return f'Pair of {rank_names.get(kickers[0], kickers[0])}s'
    else:
        return f'High Card ({rank_names.get(kickers[0], kickers[0])})' if kickers else 'High Card'

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

    # Get initial game state info
    pot_odds = game.get_pot_odds()
    stack_status = game.get_stack_status()
    position = game.position  # 'sb' or 'bb'

    # DEBUG: Show state being passed to model
    print(f"  [DEBUG] Preflop strength: {initial_strength} for {game.player_cards}")
    print(f"  [DEBUG] Position: {position}, Pot odds: {pot_odds}, Stack: {stack_status}")

    # Set goal for model with all relevant state
    # NOTE: Using 'seat' instead of 'position' (reserved), 'pot-situation' instead of 'pot-odds'
    actr.mod_focus('stage', 'preflop', 'state', 'waiting', 'strength', initial_strength,
                   'my-action', 'nil', 'opp-action', 'nil',
                   'seat', position, 'pot-situation', pot_odds, 'stack-size', stack_status,
                   'opp-card1', 'nil', 'opp-card2', 'nil', 'opp-cards-attended', 0,
                   'result', 'nil', 'learned', 'nil')

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
            pot_odds = game.get_pot_odds()
            print(f"  [DEBUG] Flop strength: {flop_strength} | Board: {game.board} | Pot odds: {pot_odds}")
            # Signal model with updated game state
            actr.mod_focus('stage', 'flop', 'state', 'waiting', 'strength', flop_strength,
                          'pot-situation', pot_odds)
        elif stage == 'turn':
            game.board.append(game.deal_card())
            add_card_to_window(game.window, game.board[3], 'turn')
            # Evaluate hand strength with turn
            turn_strength = evaluate_hand_strength(game.player_cards, game.board)
            pot_odds = game.get_pot_odds()
            print(f"  [DEBUG] Turn strength: {turn_strength} | Board: {game.board} | Pot odds: {pot_odds}")
            # Signal model with updated game state
            actr.mod_focus('stage', 'turn', 'state', 'waiting', 'strength', turn_strength,
                          'pot-situation', pot_odds)
        elif stage == 'river':
            game.board.append(game.deal_card())
            add_card_to_window(game.window, game.board[4], 'river')
            # Evaluate hand strength with river
            river_strength = evaluate_hand_strength(game.player_cards, game.board)
            pot_odds = game.get_pot_odds()
            print(f"  [DEBUG] River strength: {river_strength} | Board: {game.board} | Pot odds: {pot_odds}")
            # Signal model with updated game state
            actr.mod_focus('stage', 'river', 'state', 'waiting', 'strength', river_strength,
                          'pot-situation', pot_odds)

        # Wait for model response
        actr.run(10)

        player_action = game.response
        # DEBUG: Show what action the model took
        action_map = {'f': 'FOLD', 'c': 'CALL', 'r': 'RAISE', 'k': 'CHECK', '': 'NO RESPONSE'}
        print(f"  [DEBUG] {stage}: Model action = {action_map.get(player_action, player_action)}")

        if player_action == 'f':
            # Record fold action
            game.record_action(stage, 'fold', None)
            player_folded = True
            hand_result = 'fold'
            break

        if player_action == 'k':
            # Check - no money added, opponent gets to act
            opponent_action = current_opponent.decide(stage, game.board,
                                                       game.pot, 'k')
            # Record with opponent's actual response
            game.record_action(stage, 'check', opponent_action)

            if opponent_action == 'raise':
                # Opponent bets, we need to respond - for now auto-call
                # TODO: Could let model respond to bet
                # Opponent puts in a bet
                game.pot += game.big_blind
                game.opponent_chips -= game.big_blind
                # We auto-call their bet
                game.pot += game.big_blind
                game.player_chips -= game.big_blind

            # CRITICAL: Tell model what opponent did so it can learn!
            opp_action_chunk = f"{opponent_action}-action"
            actr.mod_focus('opp-action', opp_action_chunk, 'my-action', 'check-action',
                           'state', 'learning-stage')
            actr.run(0.5)

            # Continue to next stage
            continue

        # Get opponent response (for call/raise)
        opponent_action = current_opponent.decide(stage, game.board,
                                                   game.pot, player_action)

        # Record the action pair
        game.record_action(stage, player_action, opponent_action)

        # Tell model what opponent did so it can learn per-stage
        opp_action_chunk = f"{opponent_action}-action"
        my_action_chunk = {'r': 'raise-action', 'c': 'call-action', 'f': 'fold-action', 'k': 'check-action'}.get(player_action, 'call-action')
        actr.mod_focus('opp-action', opp_action_chunk, 'my-action', my_action_chunk,
                       'state', 'learning-stage')
        # Let model learn this stage's interaction
        actr.run(0.5)

        if opponent_action == 'fold':
            opponent_folded = True
            hand_result = 'win'
            break

        # Update pot for player calls/raises
        if player_action == 'r':
            game.pot += game.big_blind
            game.player_chips -= game.big_blind
        elif player_action == 'c':
            game.pot += game.big_blind // 2
            game.player_chips -= game.big_blind // 2

        # Update pot for opponent calls/raises (they must pay too!)
        if opponent_action == 'call':
            # Opponent matches our bet
            game.pot += game.big_blind
            game.opponent_chips -= game.big_blind
        elif opponent_action == 'raise':
            # Opponent raises - they put in more
            game.pot += game.big_blind * 2
            game.opponent_chips -= game.big_blind * 2

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

        # Get opponent card info for display
        print(f"  [DEBUG] Showdown - Opponent cards: {game.opponent_cards}")

        # Signal model that showdown is happening
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
        'action_history': list(game.action_history),  # Full action history
        'position': game.position,  # Track position for analysis
        'cognitive_trace': list(game.cognitive_trace) if hasattr(game, 'cognitive_trace') else []
    }


def run_session(n_hands=10, opponent_type='always-call',
                win_limit=10000, lose_limit=100, visible=False,
                starting_chips=5000):
    """Run a session of poker hands

    Args:
        n_hands: Maximum number of hands to play
        opponent_type: Type of opponent ('always-call', 'always-fold', 'tight', etc.)
        win_limit: Stop if player reaches this many chips (default 10000 for longer sessions)
        lose_limit: Stop if player falls below this many chips
        visible: Whether to show the window
        starting_chips: Starting chip amount (default 5000 for longer sessions)
    """
    global game

    set_opponent(opponent_type)

    results = []
    wins = 0
    losses = 0
    folds = 0

    game.player_chips = starting_chips
    game.opponent_chips = starting_chips
    game.hand_number = 0  # Reset hand number for position tracking

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

        pos = result.get('position', '??').upper()
        if result['result'] == 'win':
            wins += 1
            print(f"WIN  [{pos}] - {result['player_cards']} vs {result['opponent_cards']} "
                  f"Board: {result['board']} - Chips: {result['player_chips']}")
        elif result['result'] == 'lose':
            losses += 1
            print(f"LOSE [{pos}] - {result['player_cards']} vs {result['opponent_cards']} "
                  f"Board: {result['board']} - Chips: {result['player_chips']}")
        elif result['result'] == 'fold':
            folds += 1
            strength = result['hand_strength']
            print(f"FOLD [{pos}] ({strength}) - {result['player_cards']} "
                  f"- Chips: {result['player_chips']}")
        else:
            print(f"TIE  [{pos}] - Chips: {result['player_chips']}")

    # Summary
    print(f"\n=== Session Summary ===")
    print(f"Hands played: {len(results)}")
    print(f"Wins: {wins}, Losses: {losses}, Folds: {folds}")
    print(f"Final chips: {game.player_chips}")
    print(f"Net profit: {game.player_chips - starting_chips}")

    if results:
        win_rate = wins / len(results) * 100
        print(f"Win rate: {win_rate:.1f}%")

    return results


def run_experiment(n_sessions=10, n_hands=50, opponent_type='always-call', starting_chips=5000):
    """Run multiple sessions and aggregate results

    Args:
        n_sessions: Number of sessions to run
        n_hands: Hands per session
        opponent_type: Type of opponent to play against
        starting_chips: Starting chip amount for each session
    """
    all_results = []
    total_profit = 0
    total_wins = 0
    total_losses = 0
    total_folds = 0
    total_hands = 0

    print(f"\n{'='*50}")
    print(f"POKER EXPERIMENT: {n_sessions} sessions x {n_hands} hands")
    print(f"Opponent: {opponent_type} | Starting chips: {starting_chips}")
    print(f"{'='*50}\n")

    for session in range(n_sessions):
        print(f"\n--- Session {session + 1}/{n_sessions} ---")
        actr.reset()

        game.player_chips = starting_chips
        game.opponent_chips = starting_chips

        results = run_session(n_hands=n_hands, opponent_type=opponent_type,
                              visible=False, starting_chips=starting_chips)
        all_results.extend(results)

        for r in results:
            if r['result'] == 'win':
                total_wins += 1
            elif r['result'] == 'lose':
                total_losses += 1
            elif r['result'] == 'fold':
                total_folds += 1

        total_profit += (game.player_chips - starting_chips)
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


def compare_opponents(n_hands=20, starting_chips=5000):
    """Compare model performance against different opponent types"""
    opponent_types = ['always-call', 'always-fold', 'tight', 'aggressive', 'random']

    results_summary = {}

    print("\n" + "="*60)
    print("OPPONENT COMPARISON EXPERIMENT")
    print(f"Starting chips: {starting_chips}")
    print("="*60)

    for opp_type in opponent_types:
        print(f"\n>>> Testing against: {opp_type}")
        actr.reset()
        game.player_chips = starting_chips
        game.opponent_chips = starting_chips

        results = run_session(n_hands=n_hands, opponent_type=opp_type, visible=False,
                              starting_chips=starting_chips)

        wins = sum(1 for r in results if r['result'] == 'win')
        folds = sum(1 for r in results if r['result'] == 'fold')

        results_summary[opp_type] = {
            'hands': len(results),
            'wins': wins,
            'folds': folds,
            'final_chips': game.player_chips,
            'profit': game.player_chips - starting_chips
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


def validate_learning(n_hands=500, block_size=50, starting_chips=25000, opponent_type='always-call'):
    """Validate that the model is learning over time.

    Runs n_hands and tracks win rates in blocks to show learning progression.
    Uses higher starting chips to allow for longer sessions.

    Args:
        n_hands: Total number of hands to play
        block_size: Number of hands per block for tracking
        starting_chips: Starting chip amount (higher = longer sessions)
        opponent_type: Type of opponent to play against

    Returns:
        Dictionary with learning statistics
    """
    global game

    set_opponent(opponent_type)

    # Use higher chip amounts
    game.player_chips = starting_chips
    game.opponent_chips = starting_chips
    game.hand_number = 0

    # Track results by block
    blocks = []
    current_block = {'wins': 0, 'losses': 0, 'folds': 0, 'hands': 0}
    all_results = []

    print(f"\n{'='*60}")
    print(f"LEARNING VALIDATION: {n_hands} hands vs {opponent_type}")
    print(f"Starting chips: {starting_chips} | Block size: {block_size}")
    print(f"{'='*60}\n")

    # Reset ONCE at start to clear previous state
    actr.reset()
    if not actr.current_model():
        reload_model()

    for hand_num in range(n_hands):
        # Check if we can continue
        if game.player_chips < game.big_blind:
            print(f"\nPlayer cannot afford blind at hand {hand_num + 1}!")
            break

        result = run_hand(visible=False)
        all_results.append(result)

        # Track in current block
        current_block['hands'] += 1
        if result['result'] == 'win':
            current_block['wins'] += 1
        elif result['result'] == 'lose':
            current_block['losses'] += 1
        elif result['result'] == 'fold':
            current_block['folds'] += 1

        # End of block?
        if (hand_num + 1) % block_size == 0:
            blocks.append(current_block.copy())
            block_num = len(blocks)
            win_rate = current_block['wins'] / current_block['hands'] * 100 if current_block['hands'] > 0 else 0
            print(f"Block {block_num} (hands {hand_num + 2 - block_size}-{hand_num + 1}): "
                  f"Win rate: {win_rate:.1f}% | "
                  f"W:{current_block['wins']} L:{current_block['losses']} F:{current_block['folds']} | "
                  f"Chips: {game.player_chips}")
            current_block = {'wins': 0, 'losses': 0, 'folds': 0, 'hands': 0}

    # Add final partial block if any
    if current_block['hands'] > 0:
        blocks.append(current_block.copy())

    # Calculate learning progression
    print(f"\n{'='*60}")
    print("LEARNING PROGRESSION ANALYSIS")
    print(f"{'='*60}")

    if len(blocks) >= 2:
        first_half = blocks[:len(blocks)//2]
        second_half = blocks[len(blocks)//2:]

        first_wins = sum(b['wins'] for b in first_half)
        first_hands = sum(b['hands'] for b in first_half)
        first_rate = first_wins / first_hands * 100 if first_hands > 0 else 0

        second_wins = sum(b['wins'] for b in second_half)
        second_hands = sum(b['hands'] for b in second_half)
        second_rate = second_wins / second_hands * 100 if second_hands > 0 else 0

        improvement = second_rate - first_rate

        print(f"First half win rate:  {first_rate:.1f}% ({first_wins}/{first_hands})")
        print(f"Second half win rate: {second_rate:.1f}% ({second_wins}/{second_hands})")
        print(f"Improvement: {improvement:+.1f}%")

        if improvement > 5:
            print("[OK] Model is LEARNING - win rate improved significantly!")
        elif improvement > 0:
            print("[~] Model showing slight improvement")
        else:
            print("[X] No clear learning detected - may need more hands or different opponent")

    # Overall summary
    total_wins = sum(1 for r in all_results if r['result'] == 'win')
    total_losses = sum(1 for r in all_results if r['result'] == 'lose')
    total_folds = sum(1 for r in all_results if r['result'] == 'fold')

    print(f"\nOVERALL RESULTS:")
    print(f"Total hands: {len(all_results)}")
    print(f"Wins: {total_wins} ({total_wins/len(all_results)*100:.1f}%)")
    print(f"Losses: {total_losses} ({total_losses/len(all_results)*100:.1f}%)")
    print(f"Folds: {total_folds} ({total_folds/len(all_results)*100:.1f}%)")
    print(f"Final chips: {game.player_chips}")
    print(f"Net profit: {game.player_chips - starting_chips}")

    return {
        'blocks': blocks,
        'total_wins': total_wins,
        'total_losses': total_losses,
        'total_folds': total_folds,
        'final_chips': game.player_chips,
        'results': all_results
    }


def debug_learning(n_hands=20, opponent_type='always-call', starting_chips=5000):
    """Debug the model's learning by tracing memory retrievals.

    This function shows EXACTLY what the model is learning and retrieving:
    - What opponent patterns it has stored
    - What hand memories it retrieves when deciding
    - How decisions are being made

    Args:
        n_hands: Number of hands to trace (default 20)
        opponent_type: Opponent type
        starting_chips: Starting chips
    """
    global game

    set_opponent(opponent_type)
    game.player_chips = starting_chips
    game.opponent_chips = starting_chips
    game.hand_number = 0

    print(f"\n{'='*70}")
    print(f"LEARNING DEBUG: {n_hands} hands vs {opponent_type}")
    print(f"{'='*70}\n")

    # Reset and enable detailed tracing
    actr.reset()
    if not actr.current_model():
        reload_model()

    # Enable trace output
    actr.set_parameter_value(':v', True)
    actr.set_parameter_value(':trace-detail', 'high')

    results = []

    for hand_num in range(n_hands):
        if game.player_chips < game.big_blind:
            print(f"Cannot afford blind!")
            break

        print(f"\n{'='*70}")
        print(f"HAND {hand_num + 1}")
        print(f"{'='*70}")

        # Before hand: Show current memory state
        print("\n--- MEMORY STATE BEFORE HAND ---")
        dm_chunks = actr.dm()
        if dm_chunks:
            opp_patterns = [c for c in dm_chunks if 'OPPONENT-PATTERN' in str(c).upper()]
            hand_memories = [c for c in dm_chunks if 'HAND-MEMORY' in str(c).upper()]
            print(f"Opponent patterns stored: {len(opp_patterns)}")
            print(f"Hand memories stored: {len(hand_memories)}")

            # Show some details
            if opp_patterns:
                print("  Recent opponent patterns:")
                for chunk in opp_patterns[-5:]:
                    chunk_str = actr.chunk_slot_value(chunk, 'opp-response')
                    print(f"    - {chunk}: opp-response={chunk_str}")

        print("\n--- RUNNING HAND (watch for MEMORY/EXPLOIT outputs) ---")
        result = run_hand(visible=False)
        results.append(result)

        # Summary
        action_map = {'f': 'FOLD', 'c': 'CALL', 'r': 'RAISE', '': 'NO ACTION'}
        print(f"\n--- HAND {hand_num + 1} SUMMARY ---")
        print(f"Cards: {result['player_cards']} | Strength: {result['hand_strength']}")
        print(f"Action: {action_map.get(result['player_action'], result['player_action'])}")
        print(f"Result: {result['result'].upper()}")
        print(f"Chips: {result['player_chips']}")

    # Final memory state
    print(f"\n{'='*70}")
    print("FINAL MEMORY STATE")
    print(f"{'='*70}")

    dm_chunks = actr.dm()
    if dm_chunks:
        # Count by type
        opp_fold = 0
        opp_call = 0
        opp_raise = 0
        win_raise = 0
        win_call = 0
        lose_raise = 0
        lose_call = 0

        for chunk in dm_chunks:
            chunk_type = actr.chunk_slot_value(chunk, 'isa')
            if chunk_type == 'opponent-pattern':
                resp = actr.chunk_slot_value(chunk, 'opp-response')
                if resp == 'fold-action':
                    opp_fold += 1
                elif resp == 'call-action':
                    opp_call += 1
                elif resp == 'raise-action':
                    opp_raise += 1
            elif chunk_type == 'hand-memory':
                action = actr.chunk_slot_value(chunk, 'action')
                outcome = actr.chunk_slot_value(chunk, 'outcome')
                if outcome == 'win':
                    if action == 'raise-action':
                        win_raise += 1
                    elif action == 'call-action':
                        win_call += 1
                elif outcome == 'lose':
                    if action == 'raise-action':
                        lose_raise += 1
                    elif action == 'call-action':
                        lose_call += 1

        print(f"\nOPPONENT PATTERN MEMORIES:")
        print(f"  Fold responses: {opp_fold}")
        print(f"  Call responses: {opp_call}")
        print(f"  Raise responses: {opp_raise}")

        print(f"\nHAND OUTCOME MEMORIES:")
        print(f"  Win by raising: {win_raise}")
        print(f"  Win by calling: {win_call}")
        print(f"  Lose by raising: {lose_raise}")
        print(f"  Lose by calling: {lose_call}")

        # Predict what model should do based on memories
        if opp_fold > opp_call + opp_raise:
            print(f"\n[PREDICTION] Model should EXPLOIT by raising (opponent folds a lot)")
        elif opp_raise > opp_call:
            print(f"\n[PREDICTION] Model should be CAUTIOUS (opponent is aggressive)")
        else:
            print(f"\n[PREDICTION] Model should use hand-strength based decisions")

    # Turn off detailed tracing
    actr.set_parameter_value(':trace-detail', 'low')

    return results


def get_ideal_action_visible_info(strength, opponent_type):
    """Get ideal action based ONLY on visible information.

    This is what a rational player should do given:
    - Their hand strength (which they can see)
    - The opponent type (which they can observe over time)

    NOT based on:
    - Opponent's actual cards (hidden information)
    - Whether they would actually win (unknown until showdown)
    """
    # Against always-call: never bluff, value bet strong hands
    if opponent_type == 'always-call':
        if strength == 'strength-high':
            return 'raise'  # Value bet strong hands
        elif strength == 'strength-medium':
            return 'call'   # Don't raise without strong hand, but don't fold
        else:
            return 'fold'   # Don't bluff a calling station

    # Against always-fold: bluff everything!
    elif opponent_type == 'always-fold':
        return 'raise'  # Raise everything, they'll fold

    # Against tight: can bluff more, value bet strong
    elif opponent_type == 'tight':
        if strength == 'strength-high':
            return 'raise'
        elif strength == 'strength-medium':
            return 'call'  # Be careful, if they play back they have it
        else:
            return 'raise'  # Can bluff weak hands

    # Against aggressive: trap with strong, fold weak
    elif opponent_type == 'aggressive':
        if strength == 'strength-high':
            return 'call'   # Trap, let them bluff
        elif strength == 'strength-medium':
            return 'fold'   # Don't get blown off medium hands? Actually call
        else:
            return 'fold'

    # Against random: play straightforward
    else:
        if strength == 'strength-high':
            return 'raise'
        elif strength == 'strength-medium':
            return 'call'
        else:
            return 'fold'


def analyze_model_vs_ideal(results, opponent_type):
    """Analyze how well model plays vs ideal strategy based on VISIBLE info.

    This compares model actions to what a rational player should do given
    only the information they can see (hand strength + opponent tendencies).
    """
    matches = 0
    total = 0

    by_strength = {
        'strength-high': {'match': 0, 'total': 0, 'actions': []},
        'strength-medium': {'match': 0, 'total': 0, 'actions': []},
        'strength-low': {'match': 0, 'total': 0, 'actions': []},
    }

    action_map = {'r': 'raise', 'c': 'call', 'f': 'fold'}

    for r in results:
        strength = r['hand_strength']
        model_action = action_map.get(r['player_action'], r['player_action'])
        ideal_action = get_ideal_action_visible_info(strength, opponent_type)

        if strength in by_strength:
            by_strength[strength]['total'] += 1
            by_strength[strength]['actions'].append(model_action)

            if model_action == ideal_action:
                matches += 1
                by_strength[strength]['match'] += 1

            total += 1

    print(f"\n{'='*60}")
    print(f"MODEL vs IDEAL (based on VISIBLE information)")
    print(f"Opponent: {opponent_type}")
    print(f"{'='*60}")

    if total > 0:
        print(f"\nOverall match rate: {matches}/{total} ({matches/total*100:.1f}%)")

    print(f"\nBy hand strength:")
    for strength, data in by_strength.items():
        if data['total'] > 0:
            rate = data['match'] / data['total'] * 100
            ideal = get_ideal_action_visible_info(strength, opponent_type)

            # Count actual actions
            action_counts = {}
            for a in data['actions']:
                action_counts[a] = action_counts.get(a, 0) + 1

            print(f"\n  {strength}:")
            print(f"    Ideal action: {ideal.upper()}")
            print(f"    Model actions: {action_counts}")
            print(f"    Match rate: {data['match']}/{data['total']} ({rate:.1f}%)")

    return {
        'match_rate': matches / total if total > 0 else 0,
        'by_strength': by_strength
    }


def boost_learning():
    """Adjust ACT-R parameters to accelerate learning for debugging.

    This makes the model:
    - Learn faster (lower BLL decay)
    - Remember recent experiences better
    - Be more consistent in retrievals (lower noise)

    Call this at start of session to see learning effects more quickly.
    """
    # More aggressive base-level learning (recent memories decay slower)
    actr.set_parameter_value(':bll', 0.3)  # Was 0.5, lower = slower decay

    # Lower activation noise = more consistent retrievals
    actr.set_parameter_value(':ans', 0.2)  # Was 0.3

    # Lower retrieval threshold = retrieve more easily
    actr.set_parameter_value(':rt', -3.0)  # Was -2.0

    # Higher partial matching penalty = stricter matching
    actr.set_parameter_value(':mp', 3.0)  # Was 2.0

    print("[BOOST] Learning parameters adjusted for faster learning:")
    print("  :bll 0.3 (slower decay)")
    print("  :ans 0.2 (more consistent)")
    print("  :rt -3.0 (easier retrieval)")
    print("  :mp 3.0 (stricter matching)")


def show_chunk_activations():
    """Show activation levels of all chunks in declarative memory.

    This is KEY for debugging - shows which memories are most likely to be retrieved.
    Higher activation = more likely to be retrieved.
    """
    dm_chunks = actr.dm()
    if not dm_chunks:
        print("No chunks in declarative memory")
        return

    print(f"\n{'='*70}")
    print("CHUNK ACTIVATIONS (higher = more likely to retrieve)")
    print(f"{'='*70}\n")

    # Get activations for each chunk
    activations = []
    for chunk in dm_chunks:
        # Get chunk details
        chunk_type = actr.chunk_slot_value(chunk, 'isa')
        activation = actr.sdp(chunk, ':last-retrieval-activation')

        # Build description based on type
        if chunk_type == 'opponent-pattern':
            resp = actr.chunk_slot_value(chunk, 'opp-response')
            desc = f"OPP: {resp}"
        elif chunk_type == 'hand-memory':
            strength = actr.chunk_slot_value(chunk, 'strength')
            action = actr.chunk_slot_value(chunk, 'action')
            outcome = actr.chunk_slot_value(chunk, 'outcome')
            desc = f"HAND: {strength} + {action} -> {outcome}"
        else:
            desc = f"{chunk_type}"

        # Get reference count (number of times created/merged)
        refs = actr.sdp(chunk, ':references')

        activations.append({
            'chunk': chunk,
            'type': chunk_type,
            'desc': desc,
            'activation': activation[0] if activation else 'N/A',
            'references': refs[0] if refs else 0
        })

    # Sort by type then activation
    opp_chunks = [a for a in activations if a['type'] == 'opponent-pattern']
    hand_chunks = [a for a in activations if a['type'] == 'hand-memory']

    print("OPPONENT PATTERNS:")
    for a in sorted(opp_chunks, key=lambda x: x['references'] if isinstance(x['references'], (int, float)) else 0, reverse=True)[:10]:
        print(f"  {a['desc']:<30} refs={a['references']}")

    print("\nHAND MEMORIES (top 15):")
    for a in sorted(hand_chunks, key=lambda x: x['references'] if isinstance(x['references'], (int, float)) else 0, reverse=True)[:15]:
        print(f"  {a['desc']:<45} refs={a['references']}")

    # Summary
    print(f"\nTOTAL: {len(opp_chunks)} opponent patterns, {len(hand_chunks)} hand memories")


def quick_debug(n_hands=10):
    """Quick debug run - 10 hands with memory trace.

    Use this to quickly see if learning is working:
    - Shows memory state before/after
    - Shows what decisions are being made
    - Analyzes vs ideal strategy
    """
    print("\n" + "="*70)
    print("QUICK DEBUG - 10 hands with full trace")
    print("="*70)

    # Reset fresh
    actr.reset()
    if not actr.current_model():
        reload_model()

    # Boost learning for faster feedback
    boost_learning()

    # Show initial state
    print("\n--- INITIAL MEMORY STATE ---")
    show_chunk_activations()

    # Run hands
    results = []
    for i in range(n_hands):
        print(f"\n--- Hand {i+1} ---")
        result = run_hand(visible=False)
        results.append(result)

        action_map = {'f': 'FOLD', 'c': 'CALL', 'r': 'RAISE'}
        print(f"  {result['player_cards']} ({result['hand_strength']}) -> {action_map.get(result['player_action'], '?')} -> {result['result'].upper()}")

    # Show final state
    print("\n--- FINAL MEMORY STATE ---")
    show_chunk_activations()

    # Analyze
    analyze_model_vs_ideal(results, 'always-call')

    return results


def validate_learning_all_opponents(n_hands=500, block_size=50, starting_chips=25000):
    """Validate learning across ALL opponent types.

    This is the comprehensive test to ensure the model learns against each opponent style.

    Args:
        n_hands: Number of hands per opponent
        block_size: Block size for tracking
        starting_chips: Starting chips (higher = longer sessions)

    Returns:
        Dictionary with results for each opponent type
    """
    opponent_types = ['always-call', 'always-fold', 'tight', 'aggressive', 'random']

    all_results = {}

    print("\n" + "="*70)
    print("COMPREHENSIVE LEARNING VALIDATION ACROSS ALL OPPONENT TYPES")
    print("="*70)

    for opp_type in opponent_types:
        print(f"\n{'*'*70}")
        print(f"TESTING vs {opp_type.upper()}")
        print(f"{'*'*70}")

        results = validate_learning(
            n_hands=n_hands,
            block_size=block_size,
            starting_chips=starting_chips,
            opponent_type=opp_type
        )
        all_results[opp_type] = results

    # Summary comparison
    print("\n" + "="*70)
    print("LEARNING SUMMARY ACROSS ALL OPPONENTS")
    print("="*70)
    print(f"{'Opponent':<15} {'Hands':<8} {'Win%':<10} {'First Half':<12} {'Second Half':<12} {'Δ':<8}")
    print("-"*70)

    for opp_type, data in all_results.items():
        blocks = data['blocks']
        total_hands = sum(b['hands'] for b in blocks)
        total_wins = data['total_wins']
        win_rate = total_wins / total_hands * 100 if total_hands > 0 else 0

        if len(blocks) >= 2:
            first_half = blocks[:len(blocks)//2]
            second_half = blocks[len(blocks)//2:]

            first_wins = sum(b['wins'] for b in first_half)
            first_hands = sum(b['hands'] for b in first_half)
            first_rate = first_wins / first_hands * 100 if first_hands > 0 else 0

            second_wins = sum(b['wins'] for b in second_half)
            second_hands = sum(b['hands'] for b in second_half)
            second_rate = second_wins / second_hands * 100 if second_hands > 0 else 0

            improvement = second_rate - first_rate

            print(f"{opp_type:<15} {total_hands:<8} {win_rate:<10.1f} {first_rate:<12.1f} {second_rate:<12.1f} {improvement:+.1f}")
        else:
            print(f"{opp_type:<15} {total_hands:<8} {win_rate:<10.1f} {'N/A':<12} {'N/A':<12} {'N/A':<8}")

    return all_results


if __name__ == "__main__":
    # Usage:
    #   python poker.py                         - Run 20 hands vs always-call
    #   python poker.py 50                      - Run 50 hands vs always-call
    #   python poker.py 50 tight                - Run 50 hands vs tight
    #   python poker.py validate                - Run learning validation vs always-call
    #   python poker.py validate tight          - Run learning validation vs tight
    #   python poker.py validate-all            - Run learning validation vs ALL opponents
    #   python poker.py person                  - Interactive visual play (1 hand)
    #   python poker.py person tight            - Interactive visual play vs tight
    #   python poker.py person-session 10       - Interactive session (10 hands)
    #   python poker.py person-session 10 tight - Interactive session vs tight

    n_hands = 20
    opponent = 'always-call'

    if len(sys.argv) > 1:
        if sys.argv[1] == 'validate':
            # Run learning validation for single opponent
            opp = sys.argv[2] if len(sys.argv) > 2 else 'always-call'
            print(f"Running learning validation vs {opp}...")
            validate_learning(n_hands=500, block_size=50, starting_chips=25000, opponent_type=opp)
        elif sys.argv[1] == 'validate-all':
            # Run learning validation for ALL opponents
            print("Running comprehensive learning validation...")
            validate_learning_all_opponents(n_hands=500, block_size=50, starting_chips=25000)
        elif sys.argv[1] == 'compare':
            # Run opponent comparison
            n = int(sys.argv[2]) if len(sys.argv) > 2 else 50
            print(f"Running opponent comparison with {n} hands each...")
            compare_opponents(n_hands=n)
        elif sys.argv[1] == 'person':
            # Interactive visual play (single hand)
            opp = sys.argv[2] if len(sys.argv) > 2 else 'always-call'
            print(f"Starting interactive play vs {opp}...")
            person(opponent_type=opp)
        elif sys.argv[1] == 'person-session':
            # Interactive visual play (multiple hands)
            n = int(sys.argv[2]) if len(sys.argv) > 2 else 5
            opp = sys.argv[3] if len(sys.argv) > 3 else 'always-call'
            print(f"Starting {n}-hand session vs {opp}...")
            person_session(n_hands=n, opponent_type=opp)
        else:
            # Standard session
            n_hands = int(sys.argv[1])
            if len(sys.argv) > 2:
                opponent = sys.argv[2]
            print(f"Running poker experiment: {n_hands} hands vs {opponent}")
            run_session(n_hands=n_hands, opponent_type=opponent)
    else:
        print(f"Running poker experiment: {n_hands} hands vs {opponent}")
        run_session(n_hands=n_hands, opponent_type=opponent)