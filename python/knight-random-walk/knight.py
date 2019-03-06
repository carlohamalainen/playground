# Author: Carlo Hamalainen

import numpy            as np
import numpy.linalg     as LA
import scipy.linalg     as linalg
import random

from numpy.linalg           import matrix_power
from graphviz               import Digraph
from scipy.sparse.linalg    import eigs

# Valid moves for a normal chess knight piece.
moves = [(-2, 1), (-1, 2), (1, 2), (2, 1), (2,-1), (1,-2), (-1,-2), (-2,-1)]
assert len(moves) == 8

def is_move(r, c, u, v):
    """
    Is (r, c) -> (u, v) a valid move for a knight? This is True even
    if (u, v) goes off-board.
    """

    for m in moves:
        if (u, v) == (r + m[0], c + m[1]):
            return True
    return False

def on_board(x):
    """
    We are "on board" if we are in the usual 8x8 chess board.
    """

    return 0 <= x[0] < 8 and 0 <= x[1] < 8

def make_matrix(states):
    """
    Create the transition matrix for a knight on a chess board
    with all moves chosen uniformly at random. When the knight
    moves off-board, no more moves are made.
    """

    # Handy mapping from (row, col) -> index into 'states'
    to_idx = dict([(s, i) for (i, s) in enumerate(states)])

    P = np.array([[0.0 for _ in range(len(states))] for _ in range(len(states))], dtype='float64')
    assert P.shape == (len(states), len(states))

    for (i, (r, c)) in enumerate(states):
        for (j, (u, v)) in enumerate(states):
            # On board, equal probability to each destination, even if goes off board.
            if on_board((r, c)):
                if is_move(r, c, u, v):
                    P[i][j] = 1.0/len(moves)

            # Off board, no more moves.
            else:
                if (r, c) == (u, v): # terminal state
                    P[i][j] = 1.0
                else:
                    P[i][j] = 0.0

    return to_idx, P

def draw_graph(states, M, filename):
    """
    Render the transition graph with nodes placed in
    corresponding locations to the chess board.
    """

    NODE_SCALE = 2.0

    # This doesn't seem to work unless engine='neato'. Found the
    # weird syntax for 'pos' of a node on stackoverflow (note the
    # trailing "!").
    g = Digraph('knights', engine='neato', filename=filename, format='png')

    for s in states:
        # set the node location to (s[0], s[1])
        g.node(str(s), pos=str(NODE_SCALE*s[0]) + ',' + str(NODE_SCALE*s[1]) + '!')

    for (i, (r, c)) in enumerate(states):
        for (j, (u, v)) in enumerate(states):
            if M[i][j] > 0:
                g.edge(str((r,c)), str((u,v)), label=str(M[i][j]))

    g.render()

def do_n_steps(start, n):
    """
    For MC simulation: do n steps from start.
    """

    current = start

    for _ in range(n):
        move = random.choice(moves)

        new = (current[0] + move[0], current[1] + move[1])

        if not on_board(new): break

        current = new

    return on_board(new)

def walk_forever(start):
    """
    For MC simulation: walk until we step off the board.
    """

    current = tuple(start)

    n = 0

    while on_board(current):
        n += 1
        move = random.choice(moves)
        current = (current[0] + move[0], current[1] + move[1])

    return n

if __name__ == '__main__':
    # Valid states are all normal 8x8 chess board cells plus a 2x2 border.
    states = [(r, c) for r in range(-2, 8+2) for c in range(-2, 8+2)]

    (_, P) = make_matrix(states)
    draw_graph(states, P, 'with_corners')

    corners = [(-2,9), (9,9), (-2,-2), (9,-2)]
    states = [(r, c) for r in range(-2, 8+2) for c in range(-2, 8+2) if (r,c) not in corners]
    (to_idx, P) = make_matrix(states)

    draw_graph(states, P, 'no_corners')

    for (start, n) in [((0, 0), 0), ((3, 3), 1), ((0, 0), 1), ((3,3),4), ((3,3),5), ((3,3),100)]:
        idx = to_idx[start]

        Pn = matrix_power(P, n)

        # You are a stochastic matrix!
        for r in range(Pn.shape[0]):
            assert abs(1.0 - Pn[r][:].sum()) < 1e-15

        # What is the probability that we stay on-board after n steps, starting from idx?
        pr = sum([Pn[idx][r] for (r, s) in enumerate(states) if on_board(s)])

        print('start: {start}\tn: {n}\tPr(on board): {pr}'.format(start=start, n=n, pr=pr))


    N_sims = 10000000
    n = 5

    nr_on_board = 0

    for _ in range(N_sims):
        if do_n_steps((3,3), n): nr_on_board += 1

    print()

    print('pr on board from (3,3) after 5 steps:', nr_on_board/N_sims)

    print()

    # re-order the states
    states = [s for s in states if on_board(s)] + [s for s in states if not on_board(s)]

    (to_idx, P) = make_matrix(states)

    # k states
    k = len(states)

    # t transient states
    t = len([s for s in states if on_board(s)])


    """
    P = [ Q | R ]
        [ 0 | S ]

    Q = t x t

    S == I

    """

    S = P[t:, t:]
    assert S.shape == (k-t, k-t)
    assert (S == np.eye(k-t)).all()

    Q = P[:t, :t]
    assert Q.shape == (t, t)
    assert Q.shape == (64, 64)

    R = P[:t, t:]
    assert R.shape == (t, k-t)

    # Fundamental matrix.
    F = linalg.inv(np.eye(*Q.shape) - Q)

    # Don't actually need this...
    B = np.matmul(F, R)

    for start in [(0,0), (3,3)]:
        nr_sims = 10000000

        pr_mc = 1.0*sum([walk_forever(start) for _ in range(nr_sims)])/nr_sims

        pr_F = F[to_idx[start], :].sum()

        print('start: {start}\tAvg nr steps to absorb (MC): {pr_mc}'.format(start=start, pr_mc=pr_mc))
        print('start: {start}\tAvg nr steps (F matrix):     {pr_F}'.format(start=start,  pr_F=pr_F))
        print('')
