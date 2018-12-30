#!/usr/bin/env swipl -g main -t halt

/** <module> Blackjack: Interactive console game

 @author Kristopher Johnson
 @license MIT
*/

% Export the predicates tested by blackjack_tests.pl.
:- module('blackjack', [
    main/0,
    deck/1, cards/1, shuffled_deck/1,
    draw_card/3, new_hand/1, cards_score/3
]).

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(edit)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(readutil)).

%! cards(--Cards:list)
%
% Returns a sorted list of 13 card ranks, Ace through King.
%
% Each rank is represented by an atom.
cards(Cards) :-
    Cards = ['A','2','3','4','5','6','7','8','9','10','J','Q','K'].

%! deck(--Deck:list)
%
% Returns a sorted list of 52 cards.
deck(Deck) :-
    cards(Clubs), cards(Diamonds), cards(Hearts), cards(Spades),
    append([Clubs, Diamonds, Hearts, Spades], Deck).

%! shuffled_deck(--ShuffledDeck:list)
%
% Returns a randomly-permuted list of 52 cards.
shuffled_deck(ShuffledDeck) :-
    deck(Deck),
    random_permutation(Deck, ShuffledDeck).

%! draw_card(+Deck:list, -Top, -Remainder:list)
%
% Given a list of cards, obtain the top card and list of remaining cards.
%
% Undefined if _Deck_ is empty.
draw_card([Top|Remainder], Top, Remainder).

%! new_hand(Hand)
%
% Initial state of a newly dealt hand, including lists of dealt cards and remaining cards in deck.
%
% @param Hand A hand term with player cards, dealer cards, and remaining deck.
new_hand(hand([P1, P2], [D1, D2], Deck)) :-
    shuffled_deck(Deck1),
    draw_card(Deck1, P1, Deck2),
    draw_card(Deck2, D1, Deck3),
    draw_card(Deck3, P2, Deck4),
    draw_card(Deck4, D2, Deck).

%! player_cards(+Hand, -Cards).
%
% Extract player cards list from Hand.
player_cards(hand(Pcards, _, _), Pcards).

%! dealer_cards(+Hand, -Cards).
%
% Extract dealer cards list from Hand.
dealer_cards(hand(_, Dcards, _), Dcards).

%! deck(+Hand, -Cards).
%
% Extract remaining-deck cards list from Hand.
deck(hand(_, _, Deck), Deck).

%! card_value(+Card, -Value)
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

%! has_ace(Cards)
%
% True if Cards contains an 'A'.
has_ace(Cards) :- member('A', Cards).

%! cards_score(+Cards:list, -Score, -IsSoft)
%
% Determine _Score_ for the specified _Hand_.
% IsSoft will be true if an Ace is counted as 11.
%
% Returns the highest possible score that is less than
% or equal to 21, or if that is not possible, then
% return the minimum score over 21.
cards_score(Cards, Score, IsSoft) :-
    has_ace(Cards), !,
    hard_cards_score(Cards, HardScore),
    ( HardScore #< 12 ->
        Score #= HardScore + 10, IsSoft = true
    ;
        Score = HardScore, IsSoft = false
    ).
cards_score(Cards, Score, false) :-
    hard_cards_score(Cards, Score).

%! hard_cards_score(Cards, Score)
%
% Determine _Score_ counting Aces as 1 point each.
hard_cards_score(Cards, Score) :- hard_cards_score(Cards, 0, Score).

hard_cards_score([], Score, Score) :- !.
hard_cards_score([Card|Cards], Accum, Score) :-
    card_value(Card, Value),
    Accum2 #= Accum + Value,
    hard_cards_score(Cards, Accum2, Score).

%! cards_score(+Cards:list, -Score)
%
% Determine _Score_ for the specified _Hand_.
%
% Returns the highest possible score that is less than
% or equal to 21, or if that is not possible, then
% return the minimum score over 21.
cards_score(Cards, Score) :- cards_score(Cards, Score, _).

player_score(Hand, Score) :-
    player_cards(Hand, Cards), cards_score(Cards, Score).

dealer_score(Hand, Score) :-
    dealer_cards(Hand, Cards), cards_score(Cards, Score).

player_has_21(Hand) :-
    player_score(Hand, Score), Score #= 21.

dealer_has_21(Hand) :-
    dealer_score(Hand, Score), Score #= 21.

player_over_21(Hand) :-
    player_score(Hand, Score), Score #> 21.

dealer_over_21(Hand) :-
    dealer_score(Hand, Score), Score #> 21.


%! show_cards(+Cards:list)
%
% Prints list of card symbols separated by spaces.
%
% The list is terminated with a space.
show_cards([]) :- !.
show_cards([Head|Tail]) :-
    write(Head), write(' '), show_cards(Tail).

show_dealer_cards([_, UpCard]) :-
    write('? '), write(UpCard).

show_hand(Hand) :-
    dealer_cards(Hand, Dcards),
    player_cards(Hand, Pcards),
    player_score(Hand, Score),
    write('Dealer cards: '), show_dealer_cards(Dcards), nl,
    write('Your cards:   '), show_cards(Pcards), nl,
    write('Your total:   '), write(Score), nl.

%! read_single_char_atom(--Atom)
%
% Read a single character from input, returning it as an atom.
read_single_char_atom(Atom) :-
    flush_output, get_single_char(Code),
    % TODO: Handle Code #= -1, which is EOF
    atom_codes(Atom, [Code]).

%! play_hand
%
% Deal a new hand and play it through to the end.
play_hand :-
    write('New Deal'), nl, nl,
    new_hand(Hand),
    show_hand(Hand),
    after_deal(Hand).

%! dealt(++Hand)
%
% On a newly dealt hand, determine whether player or dealer has 21.
%
% If neither has 21, let player choose action.
after_deal(Hand) :-
    player_has_21(Hand), dealer_has_21(Hand), !,
    push.
after_deal(Hand) :-
    player_has_21(Hand), !,
    player_wins.
after_deal(Hand) :-
    dealer_has_21(Hand), !,
    dealer_wins.
after_deal(Hand) :-
    choose_initial_action(Hand).

%! choose_initial_action(++Hand)
%
% Read user's action right after a deal and execute it.
choose_initial_action(Hand) :-
    nl,
    % TODO: Only show Split if player has a pair
    write('Possible actions: h - Hit, s - Stand, d - Double down, p - Split'), nl,
    write('Your action > '), read_single_char_atom(Action),
    choose_initial_action(Hand, Action).

%! choose_action(++Hand)
%
% Read player's action and execute it.
choose_action(Hand) :-
    nl,
    write('Possible actions: h - Hit, s - Stand'), nl,
    write('Your action > '), read_single_char_atom(Action),
    choose_action(Hand, Action).

%! choose_initial_action(++Hand, ++Action).
%
% Handle player action after deal.
%
% Possible actions are
%
% - 'h' Hit
% - 's' Stand
% - 'd' Double down
% - 'p' Split
%
% For any other input, prompt again.
choose_initial_action(Hand, d) :-
    !, write('Double down'), nl,
    % TODO: Implement double down
    write('Double down is not implemented'), nl,
    choose_initial_action(Hand).
choose_initial_action(Hand, p) :-
    !, write('Split'), nl,
    % TODO: Implement split
    write('Split is not implemented'), nl,
    choose_initial_action(Hand).
choose_initial_action(Hand, h) :-
    !, choose_action(Hand, h).
choose_initial_action(Hand, s) :-
    !, choose_action(Hand, s).
choose_initial_action(Hand, Atom) :-
    !, write(Atom), write(' - Invalid input'), nl,
    choose_initial_action(Hand).

%! choose_action(++Hand, ++Action).
%
% Handle player action.
%
% Possible actions are
%
% - 'h' Hit
% - 's' Stand
%
% For any other input, prompt again.
choose_action(Hand, h) :-
    !, write('Hit'), nl,
    deck(Hand, Deck),
    player_cards(Hand, Cards),
    dealer_cards(Hand, DealerCards),
    draw_card(Deck, DealtCard, RemainingDeck),
    append(Cards, [DealtCard], NewCards),
    NewHand = hand(NewCards, DealerCards, RemainingDeck),
    show_hand(NewHand),
    after_hit(NewHand).
choose_action(Hand, s) :-
    !, write('Stand'), nl,
    dealer_actions(Hand).
choose_action(Hand, Atom) :-
    !, write(Atom), write(' - Invalid input'), nl,
    choose_action(Hand).

%! after_hit(++Hand)
%
% Determine what to do after player hits.
after_hit(Hand) :-
    player_has_21(Hand),
    !, dealer_actions(Hand).
after_hit(Hand) :-
    player_over_21(Hand),
    !, after_bust(Hand).
after_hit(Hand) :-
    !, choose_action(Hand).

%! after_bust(++Hand)
%
% Player has total over 21. Dealer wins.
after_bust(_Hand) :-
    % TODO: Show dealer cards.
    dealer_wins.

%! dealer_actions(++Hand).
%
% Handle dealer's turn after player stands.
dealer_actions(_Hand) :-
    !, write('Dealer''s turn:'), nl.

push :-
    write('Push'), nl.

player_wins :-
    write('Player wins'), nl.

dealer_wins :-
    write('Dealer wins'), nl.

next_hand :-
    play_hand.

%! show_banner
%
%! Show program title and copyright information.
show_banner :-
    write('Blackjack. Copyright 2018 Kristopher Johnson'), nl, nl.

%! go
%
% Program entry point
go :-
    show_banner, play_hand.


%! main
%
% Command-line entry point
main :-
    % TODO: Handle errors, allow exit via
    % Ctrl-C or Ctrl-D
    go.

%! ed.
%
% Open the `blackjack.pl` file in the editor.
ed :- edit(blackjack).


% vim: set ts=8 sw=4 tw=0 et :
% vim: set ft=swiprolog :

