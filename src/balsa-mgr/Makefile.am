# Process this file with automake to produce Makefile.in

SUBDIRS = pixmaps

INCLUDES = @GTK_CFLAGS@ -I../libs -I../../extra/scintilla-wrapper/scintilla/include

bin_PROGRAMS = balsa-mgr

balsa_mgr_SOURCES = \
	widgets.c widgets.h \
	support.c support.h \
	signals.c signals.h \
	main.c main.h \
	project.c project.h \
	file.c file.h \
	miscgtk.c miscgtk.h \
	options.c options.h \
	paths.c paths.h \
	buffer.c buffer.h \
	commands.c commands.h \
	projopts.c projopts.h \
	testopts.c testopts.h \
	filemanager.c filemanager.h \
	utils.c utils.h \
	makefiletable.c makefiletable.h \
	executionmanager.c executionmanager.h \
	toolseditor.c toolseditor.h \
	addfiledialog.c addfiledialog.h \
	technology.c technology.h \
	workspace.c workspace.h \
	mainwindow.c mainwindow.h \
	executionwindow.c executionwindow.h \
	consolewindow.c consolewindow.h \
	aboutdialog.c aboutdialog.h \
	optionsdialog.c optionsdialog.h \
	toolsmenueditordialog.c toolsmenueditordialog.h \
	menu_project.c menu_project.h \
	menu_file.c menu_file.h \
	menu_selecteditem.c menu_selecteditem.h \
	menu_view.c menu_view.h \
	menu_build.c menu_build.h \
	menu_cvs.c menu_cvs.h \
	menu_tools.c menu_tools.h \
	menu_help.c menu_help.h \
	mainwindow_projecteditor.c mainwindow_projecteditor.h \
	mainwindow_fileseditor.c mainwindow_fileseditor.h \
	mainwindow_menus.c mainwindow_menus.h \
	icons.c icons.h \
	projectoptionsdialog.c projectoptionsdialog.h \
	imploptionsdialog.c imploptionsdialog.h \
	builtinliboptionsdialog.c builtinliboptionsdialog.h \
	editor.c editor.h \
	ScintillaLexBalsa.cxx


balsa_mgr_LDADD = ../libs/breeze/libbreeze.la ../libs/format/libformat.la ../../extra/scintilla-wrapper/.libs/libscintilla.a @GTK_LIBS@ -lgthread

EXTRA_DIST = balsa_mgr.gld

ScintillaLexBalsa.o: ScintillaLexBalsa.cxx
	g++ ${INCLUDES} -I../../extra/scintilla-wrapper -Wno-char-subscripts -DGTK -DSCI_LEXER -c ScintillaLexBalsa.cxx

lines:
	$(top_srcdir)/bin/count-lines $(balsa_mgr_SOURCES)
