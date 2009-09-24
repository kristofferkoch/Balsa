## Process this file with automake to produce Makefile.in

EXTRA_DIST = \
	types.balsa \
	constants.balsa \
	loops.balsa

.balsa.breeze:
	../src/balsa-c/balsa-c $<

test_blocks = \
	types \
	constants \
	loops

test_breeze_files = \
	types.breeze \
	constants.breeze \
	loops.breeze

all.breeze: all.balsa $(test_breeze_files)
	../src/balsa-c/balsa-c -F all

lines:

CLEANFILES = $(test_breeze_files) all.breeze
