% Blackjack by Kristopher Johnson

cards(Cards) :-
    Cards = ['A','2','3','4','5','6','7','8','9','10','J','Q','K'].

deck(Deck) :-
    cards(Clubs), cards(Diamonds), cards(Hearts), cards(Spades),
    append([Clubs, Diamonds, Hearts, Spades], Deck).

shuffled_deck(ShuffledDeck) :-
    deck(Deck),
    random_permutation(Deck, ShuffledDeck).

:- begin_tests(deck_and_shuffled_deck).

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

:- end_tests(deck_and_shuffled_deck).


draw_card([Top|Remainder], Top, Remainder).

:- begin_tests(draw_card).

test(draw_card) :-
    draw_card(['A', '2', '3', '4'], Top, Remainder),
    Top = 'A',
    Remainder = ['2', '3', '4'].

:- end_tests(draw_card).


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
card_value('A',  11).
card_value('A',   1).

has_21(Score) :- Score =:= 21.

bust(Score) :- Score > 21.

possible_hand_score(Cards, Score) :-
    maplist(card_value, Cards, Values),
    sumlist(Values, Score).

possible_hand_scores(Cards, Scores) :-
    findall(S, possible_hand_score(Cards, S), Scores).

hand_score(Cards, Score) :-
    possible_hand_scores(Cards, S),
    exclude(bust, S, Under),
    max_list(Under, Score), !.
hand_score(Cards, Score) :-
    possible_hand_scores(Cards, S),
    min_list(S, Score).

:- begin_tests(hand_score).

test(hand_2_3)    :- hand_score(['2', '3'],        5).
test(hand_j_q)    :- hand_score(['J', 'Q'],       20).
test(hand_j_q_k)  :- hand_score(['J', 'Q', 'K'],  30).
test(hand_10_q_a) :- hand_score(['10', 'Q', 'A'], 21).
test(hand_10_a_a) :- hand_score(['10', 'A', 'A'], 12).
test(hand_a_k)    :- hand_score(['A', 'K'],       21).
test(hand_a_a)    :- hand_score(['A', 'A'],       12).
test(hand_a_a_a)  :- hand_score(['A', 'A', 'A'],  13).

:- end_tests(hand_score).


print_cards([]) :- !.
print_cards([Head|Tail]) :-
    write(Head), write(' '), print_cards(Tail).

e :- edit(blackjack).

:- run_tests.

