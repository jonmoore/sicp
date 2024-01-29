# A makefile to run only those tests that need to be re-run.
#
# raco make is used to keep track of the dependencies between racket
# files.  If and only if we see that a compiled (_rkt.zo) file has
# been rebuilt by a raco make pass then we want to re-run the tests
# from the corresponding .rkt file.
#
# Pre-requisites
# - racket installed and PATH includes e.g. C:\program files\racket
# - raco pkg install sicp debug
# - ripgrep
#
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html


################################################################################
#
# Block for setup

.PHONY: default clean build test testall

default: test

SHELL=C:\Program Files\Git\usr\bin\bash.exe

# Find all the .rkt files using rackunit (the test is not exact) in
# RKT_DIRS
RKT_DIRS := utils chap1 chap2 chap3
RKT_TEST_FILES := $(shell rg --files-with-matches rackunit $(RKT_DIRS))
ZO_TEST_FILES := $(join $(patsubst %, %compiled/, $(dir $(RKT_TEST_FILES))), \
			$(patsubst %.rkt, %_rkt.zo, $(notdir $(RKT_TEST_FILES))))


# The foreach loops over $(RKT_DIRS) below are a workaround for
# https://github.com/racket/racket/issues/4915.

################################################################################
#
# Block for target "build"
#
# Build compiled racket files
#
# The definition of newline is a fairly common trick to generate multiple commands from
# one make function (see e.g. https://stackoverflow.com/a/7040400)
define newline


endef

build:
	$(foreach rkt_dir, $(RKT_DIRS), \
		raco make -j 4 $(filter $(rkt_dir)%, $(RKT_TEST_FILES)) $(newline))


################################################################################
# In this block we try to run only those tests that need to be rerun
#
# Block for target "test"
#
# --table            : print table of results at the end
# --no-run-if-absent : skip if no tests
#
# $? is the list of prerequisites that are newer than the target.

# defining maybe_test as a callable helps avoid duplication of the list of
# files that tests need to be run for, i.e. $(1) in this context
maybe_test = $(if $(strip $(1)), \
		raco test  --make --quiet --quiet-program --jobs 4  --table --no-run-if-absent $(strip $(1)), \
		@echo skipping $(strip $(2)) - no tests need to be run) $(newline)

racotest.out: $(ZO_TEST_FILES)
	$(foreach rkt_dir, $(RKT_DIRS), \
	$(call maybe_test, $(filter $(rkt_dir)%, $(subst \compiled,,$(?:_rkt.zo=.rkt))), $(rkt_dir)))
	touch $@

test: build racotest.out

################################################################################
# as racotest.out but we use $^ rather than $? so that all the zo files are passed to raco
# test.  This may need to be fixed to work around the same raco bug as in build
#
# Block for target "testall"
#
racotest_all.out: $(ZO_TEST_FILES)
	$(foreach rkt_dir, $(RKT_DIRS), \
	$(call maybe_test, $(filter $(rkt_dir)%, $(subst \compiled,,$(^:_rkt.zo=.rkt))), $(rkt_dir)))
	touch $@

testall: build racotest_all.out

################################################################################
#
# Block for target "clean"
#
clean:
	(shopt -s globstar; rm -f **/*.zo **/*.dep racotest.out racotest.err racotest_all.out racotest_all.err)
