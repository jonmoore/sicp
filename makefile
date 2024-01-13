# A makefile to run only those tests that need to be re-run.
#
# raco make is used to keep track of the dependencies between racket
# files.  If and only if we see that a compiled (_rkt.zo) file has
# been rebuilt by a raco make pass then we want to re-run the tests
# from the corresponding .rkt file.
#
# Pre-requisites
# - racket installed and PATH includes e.g. C:\program files\racket
# - raco pkg install sicp
#
#
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html
.PHONY: default clean utils chap1 chap2 chap3

default: test

SHELL:=bash
RG:=C:/Users/jonat/bin/rg.exe
RACO:="C:/Program Files/Racket/raco.exe"

export RACO_MAKE_FLAGS = -j 4

# Find all the .rkt files using rackunit (the test is not exact) in
# RKT_DIRS
RKT_DIRS := utils chap1 chap2 chap3
FIND_TEST_FILES=$(shell $(RG) --files-with-matches rackunit $(rkt_dir))
RKT_TEST_FILES := $(foreach rkt_dir, $(RKT_DIRS), $(FIND_TEST_FILES))
RKT_TEST_FILES := $(shell $(RG) --files-with-matches rackunit $(RKT_DIRS))
ZO_TEST_FILES := $(join $(patsubst %, %compiled/, $(dir $(RKT_TEST_FILES))), \
			$(patsubst %.rkt, %_rkt.zo, $(notdir $(RKT_TEST_FILES))))

build:
	raco make -j 4 $(RKT_TEST_FILES)
.PHONY: build

# --table            : print table of results at the end
# --no-run-if-absent : skip if no tests
# $? prerequisites that are newer than the target
racotest.out: $(ZO_TEST_FILES)
	$(RACO) test --make --jobs 4 --table --no-run-if-absent \
		$(subst \compiled,,$(?:_rkt.zo=.rkt)) \
		1> >(tee racotest.out) \
		2> >(tee racotest.err)

test: build racotest.out

clean:
	del /s *.zo
	del raco_test.dummy
