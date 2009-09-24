## Process this file with automake to produce Makefile.in

schememiscdir = $(datadir)/scheme/misc

schememisc_DATA = \
	misc-banners.scm \
	misc-date.scm \
	misc-spans.scm \
	misc-implicants.scm \
	misc-switches.scm \
	misc-config.scm \
	misc.scm

EXTRA_DIST = $(schememisc_DATA)

lines:
	$(top_srcdir)/bin/count-lines $(schememisc_DATA)
