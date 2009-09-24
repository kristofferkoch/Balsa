## Process this file with automake to produce Makefile.in

balsasimdir = $(datadir)/balsa/sim

balsasim_DATA = fileio.breeze string.breeze memory.breeze \
	sim.breeze portio.breeze

CLEANFILES = $(balsasim_DATA)

EXTRA_DIST = fileio.balsa fileio.c \
	string.balsa string.c \
	memory.balsa memory.c \
	sim.balsa sim.c \
	portio.balsa

DEBUGFLAGS =

fileio.breeze: fileio.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../.. -b -i fileio.balsa

string.breeze: string.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../.. -b -i string.balsa

memory.breeze: memory.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../.. -b -i memory.balsa

sim.breeze: sim.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../.. -b -i sim.balsa

portio.breeze: portio.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../.. -b -i portio.balsa

install-exec-hook:
	$(SHELL) ../../../bin/balsa-make-builtin-lib -i ../../.. -j $(balsasimdir) fileio -I ../../../src/libs fileio.c
	$(SHELL) ../../../bin/balsa-make-builtin-lib -i ../../.. -j $(balsasimdir) string -I ../../../src/libs string.c
	$(SHELL) ../../../bin/balsa-make-builtin-lib -i ../../.. -j $(balsasimdir) memory -I ../../../src/libs memory.c
	$(SHELL) ../../../bin/balsa-make-builtin-lib -i ../../.. -j $(balsasimdir) sim -I ../../../src/libs sim.c

lines:
	$(top_srcdir)/bin/count-lines $(EXTRA_DIST)
