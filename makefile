# A makefile to run only those tests that need to be re-run.
#
# raco make is used to keep track of the dependencies between racket
# files.  If and only if we see that a compiled (_rkt.zo) file has
# been rebuilt by a raco make pass then we want to re-run the tests
# from the corresponding .rkt file.
#
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html
.PHONY: default clean utils chap1 chap2 chap3

default: build

export RACO_MAKE_FLAGS = -j 4 -v

# Find all the .rkt files using rackunit (the test is not exact) in
# RKT_DIRS
RKT_DIRS := utils chap1 chap2 chap3
FIND_TEST_FILES=$(shell cmd /c "findstr /m /s rackunit  $(rkt_dir)\*.rkt")
RKT_TEST_FILES := $(foreach rkt_dir, $(RKT_DIRS), $(FIND_TEST_FILES))
ZO_TEST_FILES := $(join $(patsubst %, %compiled/, $(dir $(RKT_TEST_FILES))), \
			$(patsubst %.rkt, %_rkt.zo, $(notdir $(RKT_TEST_FILES))))


utils:
	@raco_make_dir.bat utils
chap1:
	@raco_make_dir.bat chap1
chap2:
	@raco_make_dir.bat chap2
chap3:
	@raco_make_dir.bat chap3

build: utils chap1 chap2 chap3

# -q:quiet, -t:print table, -x:skip if no tests
# $? prerequisites that are newer than the target
raco_test.dummy: $(ZO_TEST_FILES)
	@raco test -j 4 -t -q -x $(subst \compiled,,$(?:_rkt.zo=.rkt))
	echo testing_done > raco_test.dummy

test: raco_test.dummy

clean:
	del /s *.zo
	del raco_test.dummy
