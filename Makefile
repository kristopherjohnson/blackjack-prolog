SWIPL?=swipl
DOT?=dot
WATCHMAN_MAKE=watchman-make

all: blackjack_sm.png
.PHONY: all

# Run program
run:
	$(SWIPL) -g main -t halt blackjack.pl
.PHONY: run

# Run unit tests
test:
	$(SWIPL) -g run_tests -t halt blackjack_tests.pl
.PHONY: test

# Run unit tests whenever source files change
watch:
	$(WATCHMAN_MAKE) -p '*.pl' -t test
.PHONY: watch

# Generate state-machine diagram
blackjack_sm.png: blackjack_sm.gv
	$(DOT) -Tpng "$<" > "$@"
