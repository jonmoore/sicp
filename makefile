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
# - Git bash and its associated unix commands


################################################################################
# Block for setup

.PHONY: default clean build test
default: build test

# disable built-in rules - this greatly reduces irrelevant output when debugging
MAKEFLAGS += --no-builtin-rules
MAKEFLAGS += --no-builtin-variables
.SUFFIXES:

# https://www.gnu.org/software/make/manual/html_node/Secondary-Expansion.html
.SECONDEXPANSION:

SHELL=C:\Program Files\Git\usr\bin\bash.exe

# Directories containing Racket source code we want to test
RKT_DIRS := utils chap1 chap2 chap3

################################################################################
# Block for target "build"
#
# Build compiled racket files

# newline: this definition is used to generate multiple commands from one make function
# (see e.g. https://stackoverflow.com/a/7040400)
define newline


endef

# The Racket files to test
RKT_TEST_FILES := $(shell rg --files-with-matches rackunit $(RKT_DIRS))

# Run "raco make" for all the Racket files to test.  The loop over $(RKT_DIRS) below is a
# workaround for https://github.com/racket/racket/issues/4915.
#
# Notes
#
# - .zo files depend on local and third-party racket modules.  These dependencies are
#   tracked by using "raco make", which only performs incremental compilations (or at
#   least incremental file updates).
#
# - raco make updates timestamps of compiled .so files in the natural way.  Also, it
#   regenerates .dep files only when needed - if the source files change timestamp but not
#   content the dep files are not regenerated.
build:
	$(foreach rkt_dir, $(RKT_DIRS), \
		raco make -j 4 $(filter $(rkt_dir)%, $(RKT_TEST_FILES)) $(newline))

################################################################################
# Block for target "test"
#
# In this block we try to run only those tests that need to be rerun

# maybe_test: invoke raco test on $(1) if it's non-empty
#
# $(1) : list of .rkt files to run tests for
# $(2) : tag to identify what's being skipped if $(1) is empty
#
# --table            : print table of results at the end
# --no-run-if-absent : skip if there are no tests in the Racket file
maybe_test = $(if $(strip $(1)), \
		raco test --make --jobs 4 --table --no-run-if-absent $(strip $(1)), \
		@echo skipping $(strip $(2)) - no tests need to be run) $(newline)

# Tests depend on .zo files compiled from .rkt files containing rackunit tests.  We
# determine which files have tests by searching for "rackunit".  We also could use the
# dependency information generated by running "raco make".  The current approach seems
# fine as we always require rackunit directly, which seems unlikely to change given the
# small size of the project and that some things in rackunit do not work as well if it is
# used indirectly.
#
# We use make's "shell" command and secondary expansion to generate the prerequisite list
# programmatically from the automatic variable $@.  $? is the list of prerequisites that
# are newer than the target.
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html
# https://www.gnu.org/software/make/manual/html_node/Secondary-Expansion.html
%/test.out : $$(shell rg --files-with-matches rackunit $$(dir $$@) | sed -E 's=(.*)/(.*)\.rkt=\1/compiled/\2_rkt.zo=' )
	$(call maybe_test, $(subst /compiled,,$(?:_rkt.zo=.rkt)), $@)
	touch $@

# TEST_OUTPUTS: the files to be created during test execution, one per top-level
# directory.
TEST_OUTPUTS = $(addsuffix /test.out, $(RKT_DIRS))

#
test: $(TEST_OUTPUTS)

################################################################################
#
# Block for target "clean"
#
clean:
	(shopt -s globstar; rm -f **/*.zo **/*.dep)
	rm -f $(TEST_OUTPUTS)
