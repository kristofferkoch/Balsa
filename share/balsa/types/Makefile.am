## Process this file with automake to produce Makefile.in

balsatypesdir = $(datadir)/balsa/types

balsatypes_DATA = basic.breeze builtin.breeze synthesis.breeze types.breeze

CLEANFILES = $(balsatypes_DATA)

EXTRA_DIST = basic.balsa builtin.balsa synthesis.balsa types.balsa \
	builtin.c types.c

DEBUGFLAGS =

basic.breeze: basic.balsa builtin.breeze ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -b -I ../.. -i basic.balsa

builtin.breeze: builtin.balsa ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -b -i -j builtin.balsa

synthesis.breeze: synthesis.balsa builtin.breeze ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../../ -b -i synthesis.balsa

types.breeze: types.balsa basic.breeze ../../../src/balsa-c/balsa-c
	../../../src/balsa-c/balsa-c -e -I ../../ -b -i types.balsa

install-exec-hook:
	$(SHELL) ../../../bin/balsa-make-builtin-lib -i ../../.. -j $(balsatypesdir) builtin -I ../../../src/libs builtin.c
	$(SHELL) ../../../bin/balsa-make-builtin-lib -i ../../.. -j $(balsatypesdir) types -I ../../../src/libs types.c

lines:
	$(top_srcdir)/bin/count-lines $(EXTRA_DIST)
