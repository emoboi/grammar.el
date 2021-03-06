#
# File: grammar/Makefile
#
# $Id$
#

INCDIRS = -I. -I/usr/local/include/link-grammar -I/usr/include/link-grammar
objects := grammar.o
testlogs =  test/grammar-test-el.log
EMACS_LOAD_PATH = -L . -L ../emacs-request -L ../ht.el -L ~/.emacs.d/elpa/s-20131223.944/

#all: $(objects) test
all: $(objects) 
	g++ -g -llink-grammar -o grammar $(objects)

grammar.o: grammar.cc
	g++ -g $(INCDIRS) -c grammar.cc

test: $(testlogs)
%-test-el.log: %-test.el
	emacs -batch $(EMACS_LOAD_PATH)   -l ert -l  $< -f ert-run-tests-batch-and-exit 2>&1 | tee $@  

.PHONY: clean
 clean:
	rm -f test/*.log
