## Process this file with automake to produce Makefile.in

SUBDIRS = brz doc gen misc net sim scripts

schemedir = $(datadir)/scheme

scheme_DATA = base.scm globals.scm
EXTRA_DIST = base.scm.in base-local.scm.in globals.scm.in

lines:
	for a in $(SUBDIRS); do make -C $$a lines; done
