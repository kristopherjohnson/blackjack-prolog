DOT?=dot

blackjack_sm.png: blackjack_sm.gv
	$(DOT) -Tpng "$<" > "$@"
