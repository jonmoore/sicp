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
.PHONY: default clean build

default: test

SHELL=C:\Program Files\Git\usr\bin\bash.exe

# Find all the .rkt files using rackunit (the test is not exact) in
# RKT_DIRS
RKT_DIRS := utils chap1 chap2 chap3
RKT_TEST_FILES := $(shell rg --files-with-matches rackunit $(RKT_DIRS))
ZO_TEST_FILES := $(join $(patsubst %, %compiled/, $(dir $(RKT_TEST_FILES))), \
			$(patsubst %.rkt, %_rkt.zo, $(notdir $(RKT_TEST_FILES))))

build:
	raco make -v -j 4 $(RKT_TEST_FILES)

# --table            : print table of results at the end
# --no-run-if-absent : skip if no tests
# $? prerequisites that are newer than the target
racotest.out: $(ZO_TEST_FILES)
	raco test --make --quiet --quiet-program --jobs 4 --table --no-run-if-absent \
		$(subst \compiled,,$(?:_rkt.zo=.rkt)) \
		1> >(tee racotest.out) \
		2> >(tee racotest.err)

# as racotest.out but we use $^ rather than $? so that all the zo files are passed to raco
# test
racotest_all.out: $(ZO_TEST_FILES)
	raco test --make --quiet --quiet-program --jobs 4  --table --no-run-if-absent \
		$(subst \compiled,,$(^:_rkt.zo=.rkt)) \
		1> >(tee racotest_all.out) \
		2> >(tee racotest_all.err)

test: build racotest.out

testall: build racotest_all.out

clean:
	(shopt -s globstar; rm -f **/*.zo **/*.dep racotest.out racotest.err racotest_all.out racotest_all.err)
