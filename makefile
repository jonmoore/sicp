# A makefile to run only those tests that need to be re-run.
#
# raco make is used to keep track of the dependencies between racket
# files.  If and only if we see that a compiled (_rkt.zo) file has
# been rebuilt by a raco make pass then we want to re-run the tests
# from the corresponding .rkt file.
#
.PHONY: default raco_make test
default: test

# Find all the .rkt files using rackunit (the test is not exact) in
# RKT_DIRS
RKT_DIRS := test-approaches chap1
FIND_TEST_FILES=$(shell cmd /c "findstr /m /s require.*rackunit  $(rkt_dir)\*.rkt")
RKT_TEST_FILES := $(foreach rkt_dir, $(RKT_DIRS), $(FIND_TEST_FILES))

ZO_FILES := $(join $(patsubst %, %compiled\, $(dir $(RKT_TEST_FILES))), \
		  $(patsubst %.rkt, %_rkt.zo, $(notdir $(RKT_TEST_FILES))))

raco_make:
	@raco make $(RKT_TEST_FILES)

raco_test.dummy: $(ZO_FILES)
	raco test -t -x $(subst \compiled,,$(?:_rkt.zo=.rkt))
	@echo testing_done > raco_test.dummy

test: raco_make raco_test.dummy

# Notes on this makefile
#
# The string manipulation is a bit hacky and it might be possible to
# improve this by:
# - making better use of make's string functions or
# - running make multiple times from a wrapper script
#
# Two more generic solutions have been rejected for now:
# - Paul Smith's approach to multi-architecture builds; heavier than this needs
# - guile chdir; too magical/requires new-ish make
