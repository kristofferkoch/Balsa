## Process this file with automake to produce Makefile.in

schemegendir = $(datadir)/scheme/gen

schemegen_DATA = \
	gen-espresso.scm \
	gen-mofn-espresso.scm \
	gen-gates.scm \
	gen-balsa-netlist.scm \
	gen-make-helpers.scm \
	gen-hcs.scm \
	gen-implicants.scm \
	gen.scm \
	gen-undeclared.scm \
	gen-netlists.scm \
	gen-compositions.scm

EXTRA_DIST = $(schemegen_DATA)

lines:
	$(top_srcdir)/bin/count-lines $(schemegen_DATA)
