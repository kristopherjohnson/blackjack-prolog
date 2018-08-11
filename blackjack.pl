#!/usr/bin/env swipl

/** <module> Blackjack: Interactive console game

 @author Kristopher Johnson
 @license MIT
*/

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(edit)).
:- use_module(library(lists)).
:- use_module(library(plunit)).
:- use_module(library(random)).

%! cards(--Cards:list) is det.
%
% Returns a sorted list of 13 card ranks, Ace through King.
%
% Each rank is represented by an atom.
cards(Cards) :-
    Cards = ['A','2','3','4','5','6','7','8','9','10','J','Q','K'].

%! deck(--Deck:list) is det.
%
% Returns a sorted list of 52 cards.
deck(Deck) :-
    cards(Clubs), cards(Diamonds), cards(Hearts), cards(Spades),
    append([Clubs, Diamonds, Hearts, Spades], Deck).

%! shuffled_deck(--ShuffledDeck:list) is det.
%
% Returns a randomly-permuted list of 52 cards.
shuffled_deck(ShuffledDeck) :-
    deck(Deck),
    random_permutation(Deck, ShuffledDeck).

:- begin_tests(deck_and_shuffled_deck).

test(deck) :-
    deck(Deck),
    length(Deck, L), L #= 52,
    cards(Cards), forall(member(C, Deck), member(C, Cards)),
    [C1, C2, C3, C4|_] = Deck,
    C1 = 'A', C2 = '2', C3 = '3', C4 = '4'.

test(shuffled_deck) :-
    shuffled_deck(Deck),
    length(Deck, L), L =:= 52,
    cards(Cards), forall(member(C, Deck), member(C, Cards)).

:- end_tests(deck_and_shuffled_deck).

%! draw_card(+Deck:list, -Top, -Remainder:list) is semidet.
%
% Given a list of cards, obtain the top card and list of remaining cards.
%
% Undefined if _Deck_ is empty.
draw_card([Top|Remainder], Top, Remainder).

:- begin_tests(draw_card).

test(draw_card) :-
    draw_card(['A', '2', '3', '4'], Top, Remainder),
    Top = 'A',
    Remainder = ['2', '3', '4'].

:- end_tests(draw_card).

%! new_hand(--PlayerCards:list, --DealerCards:list, --RemainingDeck:list) is det.
%
% Initial state of a newly dealt hand, including lists of dealt cards and remaining cards in deck.
%
% @param PlayerCards list of two cards dealt to player
% @param DealerCards list of two cards dealt to dealer
% @param RemainingDeck list of remaining 48 cards in deck
new_hand([P1, P2], [D1, D2], RemainingDeck) :-
    shuffled_deck(Deck1),
    draw_card(Deck1, P1, Deck2),
    draw_card(Deck2, D1, Deck3),
    draw_card(Deck3, P2, Deck4),
    draw_card(Deck4, D2, RemainingDeck).

:- begin_tests(new_hand).

test(new_hand) :-
    new_hand([P1, P2], [D1, D2], RemainingDeck),
    cards(Cards),
    member(P1, Cards), member(P2, Cards),
    member(D1, Cards), member(D2, Cards),
    !,
    length(RemainingDeck, L), L =:= 48.

:- end_tests(new_hand).

%! card_value(+Card, -Value) is nondet.
%
% Score associated with each type of card.
%
% Ace counts as 1.
card_value('A',   1).
card_value('2',   2).
card_value('3',   3).
card_value('4',   4).
card_value('5',   5).
card_value('6',   6).
card_value('7',   7).
card_value('8',   8).
card_value('9',   9).
card_value('10', 10).
card_value('J',  10).
card_value('Q',  10).
card_value('K',  10).

%! bust(+Score:int) is det.
%
% True if the _Score_ is greater than 21.
bust(Score) :- Score #> 21.

%! has_ace(Cards)
%
% True if Cards contains an 'A'.
has_ace(Cards) :- member('A', Cards).

%! hand_score(+Cards:list, -Score, -IsSoft) is semidet.
%
% Determine _Score_ for the specified _Hand_.
% IsSoft will be true if an Ace is counted as 11.
%
% Returns the highest possible score that is less than
% or equal to 21, or if that is not possible, then
% return the minimum score over 21.
hand_score(Cards, Score, IsSoft) :-
    has_ace(Cards), !,
    hard_hand_score(Cards, HardScore),
    score_with_ace(HardScore, Score, IsSoft).
hand_score(Cards, Score, false) :-
    hard_hand_score(Cards, Score).

%! hard_hand_score(Cards, Score)
%
% Determine _Score_ counting all Aces as 1 point.
hard_hand_score(Cards, Score) :-
    maplist(card_value, Cards, Values),
    sum_list(Values, Score).

%! score_with_ace(-HardScore, +Score, +IsSoft).
%
% Score for hand that includes at least one Ace.
score_with_ace(HardScore, Score, true) :-
    HardScore < 12, !,
    Score is HardScore + 10.
score_with_ace(HardScore, HardScore, false).

%! hand_score(+Cards:list, -Score) is semidet.
%
% Determine _Score_ for the specified _Hand_.
%
% Returns the highest possible score that is less than
% or equal to 21, or if that is not possible, then
% return the minimum score over 21.
hand_score(Cards, Score) :- hand_score(Cards, Score, _).


:- begin_tests(hand_score).

test(hand_2_3)    :- hand_score(['2', '3'],        5, false).
test(hand_j_q)    :- hand_score(['J', 'Q'],       20, false).
test(hand_j_q_k)  :- hand_score(['J', 'Q', 'K'],  30, false).
test(hand_10_q_a) :- hand_score(['10', 'Q', 'A'], 21, false).
test(hand_10_a_a) :- hand_score(['10', 'A', 'A'], 12, false).
test(hand_a_k)    :- hand_score(['A', 'K'],       21, true).
test(hand_a_a)    :- hand_score(['A', 'A'],       12, true).
test(hand_a_a_a)  :- hand_score(['A', 'A', 'A'],  13, true).

:- end_tests(hand_score).

%! print_cards(+Cards:list) is det.
%
% Prints list of card symbols separated by spaces.
%
% The list is terminated with a space.
print_cards([]) :- !.
print_cards([Head|Tail]) :-
    write(Head), write(' '), print_cards(Tail).

%! print_banner is det.
%
%! Show program title and copyright information.
print_banner :-
    write('Blackjack. Copyright 2018 Kristopher Johnson'), nl, nl.

/*
play_hand :-
    new_hand(PlayerCards, DealerCards, RemainingHand)
    dealt(PlayerCards, DealerCards, RemainingHand).

dealt(PlayerCards, DealerCards, RemainingHand) :-
    hand_score(PlayerCards, PlayerScore),
    hand_score(DealerCards, DealerScore),
    dealt(PlayerScore, DealerScore, PlayerCards, DealerCards, RemainingHand).

dealt(21, 21, PlayerCards, DealerCards, RemainingHand) :-
    push().

*/

%! go is det.
%
% Program entry point
go :-
    print_banner.


%! ed.
%
% Open the `blackjack.pl` file in the editor.
ed :- edit(blackjack).


:- run_tests.


% vim: set ts=8 sw=4 tw=0 et :
% vim: set ft=swiprolog :

