# A makefile to run only those tests that need to be re-run.
#
# raco make is used to keep track of the dependencies between racket
# files.  If and only if we see that a compiled (_rkt.zo) file has
# been rebuilt by a raco make pass then we want to re-run the tests
# from the corresponding .rkt file.
#
.PHONY: default raco_make test clean
default: build

# Find all the .rkt files using rackunit (the test is not exact) in
# RKT_DIRS
RKT_DIRS := test-approaches chap1 chap2 utils
FIND_TEST_FILES=$(shell cmd /c "findstr /m /s rackunit  $(rkt_dir)\*.rkt")
RKT_TEST_FILES := $(foreach rkt_dir, $(RKT_DIRS), $(FIND_TEST_FILES))

ZO_FILES := $(join $(patsubst %, %compiled\, $(dir $(RKT_TEST_FILES))), \
		  $(patsubst %.rkt, %_rkt.zo, $(notdir $(RKT_TEST_FILES))))

build:
	@raco make -j 4 $(RKT_TEST_FILES)

raco_test.dummy: $(ZO_FILES)
	raco test -j 4 -t -q -x $(subst \compiled,,$(?:_rkt.zo=.rkt))
	@echo testing_done > raco_test.dummy

test: build raco_test.dummy

clean:
	del /s *.zo
	del raco_test.dummy
