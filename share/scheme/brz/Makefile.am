## Process this file with automake to produce Makefile.in

schemebrzdir = $(datadir)/scheme/brz

schemebrz_DATA = \
	brz-balsa-md.scm \
	brz-balsa-make-makefile.scm \
	brz-breeze2ps.scm \
	brz-breeze-opt.scm \
	brz-cost-function.scm \
	brz-parser.scm \
	brz-tech.scm \
	brz-list-builtins.scm \
	brz.scm

EXTRA_DIST = $(schemebrz_DATA)

lines:
	$(top_srcdir)/bin/count-lines $(schemebrz_DATA)
