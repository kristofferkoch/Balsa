## Process this file with automake to produce Makefile.in

INCLUDES = @GLIB_CFLAGS@ -Wno-char-subscripts -Wno-unused-label -Wall

bin_PROGRAMS = balsa-c

balsa_c_LDADD = @GLIB_LIBS@

hand_c_srcs = \
	Errors.c Errors.h \
	Positions.c Positions.h \
	BalsaScanSource.c BalsaScanSource.h \
	BreezeScanSource.c BreezeScanSource.h \
	accesses.c accesses.h \
	arith.c arith.h \
	block.c block.h \
	channels.c channels.h \
	commands.c commands.h \
	components.c components.h \
	contexts.c contexts.h \
	decls.c decls.h \
	exprs.c exprs.h \
	flags.c flags.h \
	instances.c instances.h \
	lists.c lists.h \
	lvalues.c lvalues.h \
	misc.c misc.h \
	misclists.c misclists.h \
	operators.c operators.h \
	optimise.c optimise.h \
	output.c output.h \
	parts.c parts.h \
	ports.c ports.h \
	pretty.c pretty.h \
	procedures.c procedures.h \
	spans.c spans.h \
	implicants.c implicants.h \
	types.c types.h \
	values.c values.h \
	wires.c wires.h \
	balsa-c.c \
	banner.h \
	Idents.c Idents.h \
	rMemory.h \
	DynArray.h \
	StringMem.h \
	rSystem.h \
	General.c General.h \
	Sets.c Sets.h \
	callcontexts.c callcontexts.h

hand_other_srcs = \
	Balsa.ag Balsa.pars Balsa.scan \
	Breeze.pars Breeze.scan

auto_c_srcs = \
	BalsaScan_fixed.c \
	BreezeScan_fixed.c \
	Balsa.c \
	Breeze.c \
	Semantics_fixed.c \
	Tree.c

auto_h_srcs = \
	BalsaScan.h \
	BreezeScan.h \
	Balsa.h \
	Breeze.h \
	Semantics.h \
	Tree.h

auto_other_srcs = \
	Balsa.rex Balsa.lalr \
	BalsaScan.rpp \
	Breeze.rex Breeze.lalr \
	BreezeScan.rpp \
	yyTree.w

balsa_c_SOURCES = $(hand_c_srcs) $(auto_c_srcs) $(auto_h_srcs)

EXTRA_balsa_c_SOURCES = $(hand_other_srcs) $(auto_other_srcs)

EXTRA_DIST = template.c template.h fixag.pl fix_rex_output.pl Semantics.c BalsaScan.c BreezeScan.c

MAINTAINERCLEANFILES = $(auto_c_srcs) $(auto_h_srcs) $(auto_other_srcs)

BalsaScanSource.c: BalsaScan.h BreezeScan.h Tree.h
BreezeScanSource.c: BreezeScan.h Tree.h

$(hand_c_srcs) $(auto_c_srcs): $(auto_h_srcs)

BalsaScan.rpp Balsa.lalr: Balsa.pars
	$(CG) -cxjz Balsa.pars

Balsa.rex: Balsa.scan BalsaScan.rpp
	$(RPP) BalsaScan.rpp < Balsa.scan > Balsa.rex;

BalsaScan.h BalsaScan.c: Balsa.rex 
	$(REX) -cd Balsa.rex

BalsaScan_fixed.c: BalsaScan.c
	./fix_rex_output.pl BalsaScan.c > BalsaScan_fixed.c

Balsa.h Balsa.c: Balsa.lalr
	$(LALR) -c -d Balsa.lalr 2>&1 | grep -v '^Information'; 

BreezeScan.rpp Breeze.lalr: Breeze.pars
	$(CG) -cxjz Breeze.pars

Breeze.rex: Breeze.scan BreezeScan.rpp
	$(RPP) BreezeScan.rpp < Breeze.scan > Breeze.rex;

BreezeScan.h BreezeScan.c: Breeze.rex 
	$(REX) -cd Breeze.rex

BreezeScan_fixed.c: BreezeScan.c
	./fix_rex_output.pl BreezeScan.c > BreezeScan_fixed.c

Breeze.h Breeze.c: Breeze.lalr
	$(LALR) -c -d Breeze.lalr

Tree.h Tree.c Semantics.h Semantics.c yyTree.w: Balsa.ag
	$(AST) -I -cdimyRD68 Balsa.ag

Semantics_fixed.c: Semantics.c
	./fixag.pl Semantics.c > Semantics_fixed.c

lines:
	$(top_srcdir)/bin/count-lines $(hand_c_srcs) $(hand_other_srcs)
