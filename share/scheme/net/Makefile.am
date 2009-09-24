## Process this file with automake to produce Makefile.in

schemenetdir = $(datadir)/scheme/net

schemenet_DATA = \
	net-compass.scm \
	net-connections.scm \
	net-modify.scm \
	net-parser.scm \
	net-verilog.scm \
	net-edif.scm \
	net-drive.scm \
	net-merge-netlists.scm \
	net-insert-buffers.scm \
	net.scm

EXTRA_DIST = $(schemenet_DATA)

lines:
	$(top_srcdir)/bin/count-lines $(schemenet_DATA)
