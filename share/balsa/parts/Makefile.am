## Process this file with automake to produce Makefile.in

balsapartsdir = $(datadir)/balsa/parts

balsaparts_DATA = buffers.breeze counters.breeze

CLEANFILES = $(balsaparts_DATA)

EXTRA_DIST = buffers.balsa counters.balsa

DEBUGFLAGS =

buffers.breeze: buffers.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -b -I ../../ buffers.balsa

counters.breeze: counters.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -b -I ../../ counters.balsa

lines:
	$(top_srcdir)/bin/count-lines $(EXTRA_DIST)
