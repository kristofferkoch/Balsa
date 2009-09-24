## Process this file with automake to produce Makefile.in

schemescriptsdir = $(datadir)/scheme/scripts

schemescripts_DATA = \
	balsa-md.scm \
	balsa-make-makefile.scm \
	balsa-netlist.scm \
	balsa-list-builtins.scm \
	balsa-make-helpers.scm \
	balsa-make-impl-test.scm \
	balsa-make-test.scm \
	balsa-sim-impl.scm \
	breeze-opt.scm \
	breeze2c.scm \
	breeze2ps.scm \
	breeze-cost.scm \
	balsa-banner.scm \
	balsa-prog-banner.scm \
	balsa-split-lines.scm \
	balsa-net-tran-cost.scm \
	balsa-net-merge-netlists.scm

EXTRA_DIST = $(schemescripts_DATA)

lines:
	$(top_srcdir)/bin/count-lines $(schemescripts_DATA)
