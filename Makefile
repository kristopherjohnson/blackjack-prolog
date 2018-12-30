SWIPL?=swipl
DOT?=dot

# Run application
run:
	$(SWIPL) -g main -t halt blackjack.pl
PHONY: run

# Run unit tests
test:
	$(SWIPL) -g run_tests -t halt blackjack_tests.pl
PHONY: test

# Generate state-machine diagram
blackjack_sm.png: blackjack_sm.gv
	$(DOT) -Tpng "$<" > "$@"
