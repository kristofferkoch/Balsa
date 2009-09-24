## Process this file with automake to produce Makefile.in

SUBDIRS = scheme tech balsa misc

lines:
	for a in $(SUBDIRS); do make -C $$a lines; done
