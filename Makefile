# Makefile targets:
#
# blackjack        - build blackjack executable (requires GNU Prolog)
# run-gprolog      - run blackjack.pl in GNU Prolog
# run-swipl        - run blackjack.pl in SWI-Prolog
# test             - run unit tests (requires SWI-Prolog)
# watch            - auto-run test whenever source files change (requires watchman-make)
# blackjack_sm.png - create state-machine diagram (requires graphviz dot)
# clean            - delete all generated files
# all              - builds blackjack and blackjack_sm.png

# SWI-Prolog
SWIPL?=swipl

# GNU Prolog
GPROLOG?=gprolog
GPLC?=gplc
GPLCFLAGS?=--min-size -C -Os

DOT?=dot
WATCHMAN_MAKE=watchman-make

all: blackjack blackjack_sm.png
.PHONY: all

# Compile to executable using GNU Prolog
blackjack: blackjack.pl
	$(GPLC) $(GPLCFLAGS) -o $@ $<

# Run program in GNU Prolog
run-gprolog:
	$(GPROLOG) --consult-file blackjack.pl --query-goal main
.PHONY: run-gprolog

# Run program in SWI Prolog
run-swipl:
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
	$(DOT) -Tpng $< > $@

clean:
	- $(RM) blackjack
	- $(RM) blackjack_sm.png
.PHONY: clean