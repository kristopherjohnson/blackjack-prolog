#!/usr/bin/env swipl -g run_tests -t halt

% Unit tests for blackjack.pl

:- use_module('blackjack.pl', [
    deck/1, cards/1, shuffled_deck/1, draw_card/3,
    new_hand/1, cards_score/3
]).

:- use_module(library(plunit)).


:- begin_tests(test_deck_and_shuffled_deck).

test(deck) :-
    deck(Deck),
    length(Deck, L), L =:= 52,
    cards(Cards), forall(member(C, Deck), member(C, Cards)),
    [C1, C2, C3, C4|_] = Deck,
    C1 = 'A', C2 = '2', C3 = '3', C4 = '4'.

test(shuffled_deck) :-
    shuffled_deck(Deck),
    length(Deck, L), L =:= 52,
    cards(Cards), forall(member(C, Deck), member(C, Cards)).

:- end_tests(test_deck_and_shuffled_deck).


:- begin_tests(draw_card).

test(draw_card) :-
    draw_card(['A', '2', '3', '4'], Top, Remainder),
    Top = 'A',
    Remainder = ['2', '3', '4'],
    draw_card(Remainder, Top2, Remainder2),
    Top2 = '2',
    Remainder2 = ['3', '4'].

:- end_tests(draw_card).


:- begin_tests(new_hand).

test(new_hand) :-
    new_hand(hand([P1, P2], [D1, D2], Deck)),
    cards(Cards),
    member(P1, Cards), member(P2, Cards),
    member(D1, Cards), member(D2, Cards),
    !,
    length(Deck, L), L =:= 48.

:- end_tests(new_hand).


:- begin_tests(cards_score).

test(hand_empty)  :- cards_score([],                0, false).
test(hand_2_3)    :- cards_score(['2', '3'],        5, false).
test(hand_j_q)    :- cards_score(['J', 'Q'],       20, false).
test(hand_j_q_k)  :- cards_score(['J', 'Q', 'K'],  30, false).
test(hand_10_q_a) :- cards_score(['10', 'Q', 'A'], 21, false).
test(hand_10_a_a) :- cards_score(['10', 'A', 'A'], 12, false).
test(hand_a_k)    :- cards_score(['A', 'K'],       21, true).
test(hand_a_a)    :- cards_score(['A', 'A'],       12, true).
test(hand_a_a_a)  :- cards_score(['A', 'A', 'A'],  13, true).

:- end_tests(cards_score).


% vim: set ts=8 sw=4 tw=0 et :
% vim: set ft=swiprolog :
