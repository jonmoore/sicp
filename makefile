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
# - sed
#
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html

################################################################################
# Block for setup

.PHONY: default clean build test
default: test

# disable built-in rules - this greatly reduces irrelevant output when debugging
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-builtin-variables
.SUFFIXES:

# https://www.gnu.org/software/make/manual/html_node/Secondary-Expansion.html
.SECONDEXPANSION:

SHELL=C:\Program Files\Git\usr\bin\bash.exe

# Directories containing Racket source code
RKT_DIRS := utils chap1 chap2 chap3

################################################################################
# Block for target "build"
#
# Build compiled racket files
#
# The definition of newline is used to generate multiple commands from one make function
# (see e.g. https://stackoverflow.com/a/7040400)
define newline


endef

# The loop over $(RKT_DIRS) below is a workaround for
# https://github.com/racket/racket/issues/4915.
RKT_TEST_FILES := $(shell rg --files-with-matches rackunit $(RKT_DIRS))
build:
	$(foreach rkt_dir, $(RKT_DIRS), \
		raco make -j 4 $(filter $(rkt_dir)%, $(RKT_TEST_FILES)) $(newline))

################################################################################
# Block for target "test"
#
# In this block we try to run only those tests that need to be rerun
#
# --table            : print table of results at the end
# --no-run-if-absent : skip if no tests
#

# defining maybe_test as a callable helps avoid duplication of the list of
# files that tests need to be run for, i.e. $(1) in this context
maybe_test = $(if $(strip $(1)), \
		raco test --make --jobs 4 --table --no-run-if-absent $(strip $(1)), \
		@echo skipping $(strip $(2)) - no tests need to be run) $(newline)

TEST_OUTPUTS = $(addsuffix /test.out, $(RKT_DIRS))

# Uses https://www.gnu.org/software/make/manual/html_node/Secondary-Expansion.html
%/test.out : $$(shell rg --files-with-matches rackunit $$(dir $$@) | sed -E 's=(.*)/(.*)\.rkt=\1/compiled/\2_rkt.zo=' )
	$(call maybe_test, $(subst /compiled,,$(?:_rkt.zo=.rkt)), $@)
	touch $@

test: build $(TEST_OUTPUTS)

################################################################################
#
# Block for target "clean"
#
clean:
	(shopt -s globstar; rm -f **/*.zo **/*.dep racotest.out racotest.err racotest_all.out racotest_all.err)
	rm -f $(TEST_OUTPUTS)
