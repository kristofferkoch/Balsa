/*
	The Balsa Asynchronous Hardware Synthesis System
	Copyright (C) 1995-2003 Department of Computer Science
	The University of Manchester, Oxford Road, Manchester, UK, M13 9PL

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

	`testopts.h'
	Test options dialogue

*/

#ifndef TESTOPTS_HEADER
#define TESTOPTS_HEADER

#include "main.h"
#include "file.h"

/* TEST_COMPONENT_{NEW,NO_SELECTION} : row values for unselected states */
#define TEST_COMPONENT_NEW (-2)
#define TEST_COMPONENT_NO_SELECTION (-1)

/* AddTestComponentToTestComponentList : insert a row into the testComponent list at the row stated */
extern void AddTestComponentToTestComponentList (GtkCList * list, PtrBalsaTestComponent testComponent, int atRow, gboolean selectAfterInsert);

/* AddTestComponentsToTestComponentList : add a list of testComponents to the test fixture testComponent list (at the end)
	but don't change the selection */
extern void AddTestComponentsToTestComponentList (GtkCList * list, GList * testComponents);

/* GetBalsaTestComponentListFromFileView : read from the displayed `file view' and make a new GList
	of BalsaTestComponent elements.  `node' should be the test entry in the file view */
extern GList *GetBalsaTestComponentListFromFileView (GtkCTree * fileView, GtkCTreeNode * testNode);

/* UpdateTestFixtureCTreeNode : update the CTree test fixture after edits */
extern void UpdateTestFixtureCTreeNode (GtkCTree * tree, GtkCTreeNode * testNode);

void ChangeTestComponentInTestComponentList (PtrBalsaTestComponent testComponent, GtkCList * list);

void CreateNewComponents (GtkCList * list, char *filename, char *procName);

void TestOpts_clearList (GtkWidget * dialogue);
void TestOpts_insertToList (GtkCList * list, int atRow, char **text);
void TestOpts_removeFromList (GtkCList * list, int row);
void TestOpts_UpdateForgottenPorts (GtkCList * list);
void TestOpts_FillDefinesList (GtkWidget * dialogue, char *commandLineOptions);

void UponTestOptionsDialogueOK (GtkObject * dialogue);
void UponTestOptionsDialogueCancel (GtkObject * dialogue);
void UponTestPortsNewButton (GtkObject * dialogue);
void UponTestPortsDelete (GtkObject * dialogue);
void UponTestPortsRefill (GtkButton * button, gpointer user_data);
void UponTestPortRadiobuttonToggle (GtkToggleButton * togglebutton);
void UponTestPortListSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event);
void UponTestPortListUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event);
void UponTestPortFileBrowseButton (GtkObject * dialogue);
void UponTestPortEntryChange (GtkEditable * editable);
void UponTestPortValueEntryChange (GtkEditable * editable);
void UponMemoryPortEntryChange (GtkEditable * editable);
void UponTestFformatEntryChange (GtkEditable * editable);
void UponTestPformatEntryChange (GtkEditable * editable);
gboolean UponTestOptionsDialogue_keyPressEvent (GtkObject * widget, GdkEventKey * event, gpointer user_data);
void UponTestPortFileFormatBrowseButton (GtkWidget * dialogue);
void UponTestPortPrintFormatBrowseButton (GtkWidget * dialogue);

/* TEST_OPTS_... : selection special cases */
#define TEST_OPTS_NO_SELECTION (-1)
#define TEST_OPTS_NEW (-2)

void UponTestOptionsDefinitionsUpButton (GtkObject * dialogue);
void UponTestOptionsDefinitionsDownButton (GtkObject * dialogue);
void UponTestOptionsDefinitionsNewButton (GtkObject * dialogue);
void UponTestOptionsDefinitionsDeleteButton (GtkObject * dialogue);
void UponTestOptionsDefinitionsSelectRow (GtkCList * list, gint row, gint column, GdkEvent * event);
void UponTestOptionsDefinitionsUnselectRow (GtkCList * list, gint row, gint column, GdkEvent * event);
void UponTestOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable);
void UponTestOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable);

#endif
/* TESTOPTS_HEADER */
