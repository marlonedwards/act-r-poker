# poker_viz.py - Visualization for ACT-R Poker Model Learning
"""
Visualizes model learning over time showing:
- Profit/loss trends
- Win/fold rates
- Expected value (EV) per hand
- Stage-by-stage decision patterns

Run after experiments to see learning progression.
"""

import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict


class PokerAnalyzer:
    """Analyzes poker results and tracks learning metrics."""

    def __init__(self):
        self.hands = []
        self.opponent_type = 'unknown'

    def reset(self):
        self.hands = []

    def record_hand(self, hand_data):
        """Record a single hand's data for analysis."""
        self.hands.append(hand_data)

    def calculate_metrics(self, window_size=20):
        """Calculate rolling metrics for visualization."""
        n = len(self.hands)
        if n == 0:
            return None

        metrics = {
            'hand_num': list(range(1, n + 1)),
            'cumulative_profit': [],
            'rolling_win_rate': [],
            'rolling_fold_rate': [],
            'rolling_ev': [],  # Expected value per hand
            'chips': [],
        }

        starting_chips = self.hands[0].get('player_chips', 5000) if self.hands else 5000
        prev_chips = starting_chips

        # Track rolling windows
        recent_results = []
        recent_folds = []
        recent_profits = []

        for i, hand in enumerate(self.hands):
            # Current chips and profit
            chips = hand.get('player_chips', prev_chips)
            hand_profit = chips - prev_chips  # Profit this hand
            cumulative_profit = chips - starting_chips

            metrics['cumulative_profit'].append(cumulative_profit)
            metrics['chips'].append(chips)

            # Track results
            result = hand.get('result', 'lose')
            is_win = 1 if result == 'win' else 0
            is_fold = 1 if result == 'fold' else 0

            recent_results.append(is_win)
            recent_folds.append(is_fold)
            recent_profits.append(hand_profit)

            # Calculate rolling averages
            window = min(window_size, i + 1)

            win_rate = sum(recent_results[-window:]) / window * 100
            fold_rate = sum(recent_folds[-window:]) / window * 100
            avg_ev = sum(recent_profits[-window:]) / window  # Chips per hand

            metrics['rolling_win_rate'].append(win_rate)
            metrics['rolling_fold_rate'].append(fold_rate)
            metrics['rolling_ev'].append(avg_ev)

            prev_chips = chips

        return metrics

    def get_stage_actions(self):
        """Get breakdown of actions by stage."""
        stage_actions = {
            'preflop': {'r': 0, 'c': 0, 'f': 0, 'k': 0},
            'flop': {'r': 0, 'c': 0, 'f': 0, 'k': 0},
            'turn': {'r': 0, 'c': 0, 'f': 0, 'k': 0},
            'river': {'r': 0, 'c': 0, 'f': 0, 'k': 0},
        }

        # Map full action names to single letters
        action_map = {
            'raise': 'r', 'r': 'r',
            'call': 'c', 'c': 'c',
            'fold': 'f', 'f': 'f',
            'check': 'k', 'k': 'k',
        }

        for hand in self.hands:
            action_history = hand.get('action_history', [])
            for action in action_history:
                stage = action.get('stage') if isinstance(action, dict) else action[0]
                player_action = action.get('player') if isinstance(action, dict) else action[1]
                # Normalize action to single letter
                player_action = action_map.get(player_action, player_action)
                if stage in stage_actions and player_action in stage_actions[stage]:
                    stage_actions[stage][player_action] += 1

        return stage_actions

    def get_action_by_strength_and_stage(self):
        """Get detailed breakdown: stage x strength x action."""
        breakdown = {}
        stages = ['preflop', 'flop', 'turn', 'river']
        strengths = ['strength-high', 'strength-medium', 'strength-low']

        for stage in stages:
            breakdown[stage] = {s: {'r': 0, 'c': 0, 'f': 0, 'k': 0} for s in strengths}

        # Map full action names to single letters
        action_map = {
            'raise': 'r', 'r': 'r',
            'call': 'c', 'c': 'c',
            'fold': 'f', 'f': 'f',
            'check': 'k', 'k': 'k',
        }

        for hand in self.hands:
            action_history = hand.get('action_history', [])
            # For simplicity, use final hand strength
            # A more sophisticated version would track strength at each stage
            strength = hand.get('hand_strength', 'strength-low')

            for action in action_history:
                stage = action.get('stage') if isinstance(action, dict) else action[0]
                player_action = action.get('player') if isinstance(action, dict) else action[1]
                # Normalize action to single letter
                player_action = action_map.get(player_action, player_action)
                if stage in breakdown and player_action:
                    if player_action in breakdown[stage][strength]:
                        breakdown[stage][strength][player_action] += 1

        return breakdown

    def get_learning_by_half(self):
        """Compare first half vs second half performance."""
        n = len(self.hands)
        if n < 10:
            return None

        half = n // 2
        first_half = self.hands[:half]
        second_half = self.hands[half:]

        def calc_stats(hands):
            wins = sum(1 for h in hands if h.get('result') == 'win')
            folds = sum(1 for h in hands if h.get('result') == 'fold')

            if len(hands) == 0:
                return {'win_rate': 0, 'fold_rate': 0}

            return {
                'win_rate': wins / len(hands) * 100,
                'fold_rate': folds / len(hands) * 100,
                'n_hands': len(hands)
            }

        return {
            'first_half': calc_stats(first_half),
            'second_half': calc_stats(second_half)
        }


def plot_learning_curves(analyzer, opponent_type='Unknown', save_path=None):
    """Create comprehensive learning visualization.

    Shows:
    1. Cumulative profit over hands
    2. Rolling win rate vs fold rate
    3. Expected Value (EV) per hand over time
    4. Stage-by-stage action breakdown
    """
    metrics = analyzer.calculate_metrics(window_size=20)
    if not metrics:
        print("No data to plot!")
        return

    n_hands = len(analyzer.hands)

    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle(f'{opponent_type} - {n_hands} Hands', fontsize=16, fontweight='bold')

    # Plot 1: Cumulative Profit
    ax1 = axes[0, 0]
    ax1.plot(metrics['hand_num'], metrics['cumulative_profit'],
             color='green', linewidth=2, label='Profit')
    ax1.axhline(y=0, color='gray', linestyle='--', alpha=0.5)
    ax1.fill_between(metrics['hand_num'], 0, metrics['cumulative_profit'],
                     where=[p >= 0 for p in metrics['cumulative_profit']],
                     color='green', alpha=0.3)
    ax1.fill_between(metrics['hand_num'], 0, metrics['cumulative_profit'],
                     where=[p < 0 for p in metrics['cumulative_profit']],
                     color='red', alpha=0.3)
    ax1.set_xlabel('Hand #')
    ax1.set_ylabel('Chips')
    ax1.set_title('Cumulative Profit/Loss')
    ax1.legend()
    ax1.grid(True, alpha=0.3)

    # Plot 2: Rolling Win Rate & Fold Rate
    ax2 = axes[0, 1]
    ax2.plot(metrics['hand_num'], metrics['rolling_win_rate'],
             color='blue', linewidth=2, label='Win Rate')
    ax2.plot(metrics['hand_num'], metrics['rolling_fold_rate'],
             color='orange', linewidth=2, label='Fold Rate')
    ax2.axhline(y=50, color='gray', linestyle='--', alpha=0.5, label='50%')
    ax2.set_xlabel('Hand #')
    ax2.set_ylabel('Rate (%)')
    ax2.set_title('Rolling Win Rate & Fold Rate (20-hand window)')
    ax2.legend()
    ax2.set_ylim(0, 100)
    ax2.grid(True, alpha=0.3)

    # Plot 3: Expected Value (EV) per Hand
    ax3 = axes[1, 0]
    ev_data = metrics['rolling_ev']
    ax3.plot(metrics['hand_num'], ev_data, color='purple', linewidth=2, label='EV/hand')
    ax3.axhline(y=0, color='gray', linestyle='--', alpha=0.5, label='Break-even')
    ax3.fill_between(metrics['hand_num'], 0, ev_data,
                     where=[e >= 0 for e in ev_data],
                     color='green', alpha=0.3)
    ax3.fill_between(metrics['hand_num'], 0, ev_data,
                     where=[e < 0 for e in ev_data],
                     color='red', alpha=0.3)

    # Add trend line
    if n_hands > 10:
        z = np.polyfit(metrics['hand_num'], ev_data, 1)
        p = np.poly1d(z)
        ax3.plot(metrics['hand_num'], p(metrics['hand_num']),
                 'b--', alpha=0.8, label=f'Trend ({z[0]:+.2f}/hand)')

    ax3.set_xlabel('Hand #')
    ax3.set_ylabel('Chips per Hand')
    ax3.set_title('Expected Value (Rolling 20-hand Average)')
    ax3.legend()
    ax3.grid(True, alpha=0.3)

    # Plot 4: Stage-by-Stage Action Breakdown
    ax4 = axes[1, 1]
    stage_actions = analyzer.get_stage_actions()

    stages = ['preflop', 'flop', 'turn', 'river']
    stage_labels = ['Preflop', 'Flop', 'Turn', 'River']
    actions = ['r', 'c', 'k', 'f']
    action_labels = ['Raise', 'Call', 'Check', 'Fold']
    colors = ['#2ecc71', '#3498db', '#9b59b6', '#e74c3c']  # green, blue, purple, red

    x = np.arange(len(stages))
    width = 0.2

    for i, (action, label, color) in enumerate(zip(actions, action_labels, colors)):
        counts = [stage_actions[s].get(action, 0) for s in stages]
        ax4.bar(x + i * width, counts, width, label=label, color=color, alpha=0.8)

    ax4.set_xlabel('Stage')
    ax4.set_ylabel('Count')
    ax4.set_title('Actions by Stage (All Hands)')
    ax4.set_xticks(x + 1.5 * width)
    ax4.set_xticklabels(stage_labels)
    ax4.legend()
    ax4.grid(True, alpha=0.3, axis='y')

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Saved visualization to {save_path}")

    plt.show()


def print_summary(analyzer, opponent_type='Unknown'):
    """Print text summary of results."""
    n = len(analyzer.hands)
    if n == 0:
        print("No hands to analyze!")
        return

    wins = sum(1 for h in analyzer.hands if h.get('result') == 'win')
    losses = sum(1 for h in analyzer.hands if h.get('result') == 'lose')
    folds = sum(1 for h in analyzer.hands if h.get('result') == 'fold')

    starting_chips = analyzer.hands[0].get('player_chips', 5000)
    final_chips = analyzer.hands[-1].get('player_chips', 5000)
    profit = final_chips - starting_chips

    print("=" * 60)
    print(f"SUMMARY: {opponent_type} - {n} Hands")
    print("=" * 60)
    print()
    print("Results:")
    print(f"  Wins:    {wins} ({wins/n*100:.1f}%)")
    print(f"  Losses:  {losses} ({losses/n*100:.1f}%)")
    print(f"  Folds:   {folds} ({folds/n*100:.1f}%)")
    print()
    print(f"Profit: {profit:+d} chips")
    print()

    # Stage breakdown
    print("Action Breakdown by Stage:")
    stage_actions = analyzer.get_stage_actions()
    print(f"  {'Stage':<10} {'Raise':>8} {'Call':>8} {'Check':>8} {'Fold':>8}")
    print(f"  {'-'*10} {'-'*8} {'-'*8} {'-'*8} {'-'*8}")
    for stage in ['preflop', 'flop', 'turn', 'river']:
        actions = stage_actions[stage]
        total = sum(actions.values())
        if total > 0:
            print(f"  {stage.capitalize():<10} {actions['r']:>8} {actions['c']:>8} {actions['k']:>8} {actions['f']:>8}")

    # Learning analysis
    learning = analyzer.get_learning_by_half()
    if learning:
        print()
        print("Learning Analysis:")
        first = learning['first_half']
        second = learning['second_half']
        print(f"  First half win rate:  {first['win_rate']:.1f}%")
        print(f"  Second half win rate: {second['win_rate']:.1f}%")
        win_improvement = second['win_rate'] - first['win_rate']
        print(f"  Improvement: {win_improvement:+.1f}%")

        if win_improvement > 3:
            print("  [+] Clear improvement - model is learning!")
        elif win_improvement > 0:
            print("  [~] Slight improvement")
        else:
            print("  [-] No improvement or decline")


def run_analysis(results, opponent_type='Unknown', show_plot=True, save_path=None):
    """Main entry point for analysis.

    Args:
        results: List of hand results from run_session()
        opponent_type: Name of opponent for display
        show_plot: Whether to show matplotlib plot
        save_path: Path to save figure (optional)
    """
    analyzer = PokerAnalyzer()
    analyzer.opponent_type = opponent_type

    for result in results:
        analyzer.record_hand(result)

    print_summary(analyzer, opponent_type)

    if show_plot:
        plot_learning_curves(analyzer, opponent_type, save_path)

    return analyzer


# CLI usage:
if __name__ == '__main__':
    import sys

    if len(sys.argv) >= 3:
        n_hands = int(sys.argv[1])
        opponent_type = sys.argv[2]
        save_path = sys.argv[3] if len(sys.argv) > 3 else None

        # Import poker and run session
        import poker
        print(f"Running {n_hands} hands against {opponent_type}...")
        results = poker.run_session(n_hands=n_hands, opponent_type=opponent_type)
        run_analysis(results, opponent_type, show_plot=True, save_path=save_path)
    else:
        print("Usage: python poker_viz.py <n_hands> <opponent_type> [save_path]")
        print("Example: python poker_viz.py 100 always-fold")
        print("         python poker_viz.py 500 always-call results.png")
