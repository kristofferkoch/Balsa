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

	`projopts.h'
	Project options dialogue handling

*/

#ifndef PROJOPTS_HEADER
#define PROJOPTS_HEADER

#include "main.h"

/* PROJ_OPTS_... : selection special cases */
#define PROJ_OPTS_NO_SELECTION (-1)
#define PROJ_OPTS_NEW (-2)

extern void ProjectOptions_UpdateCheckBoxes (void);

void ProjectOptions_DisplayWindow (gboolean newProject);

void UponProjectFileImport_ConvertRelAbs (GtkObject * dialogue);
void UponProjectFileImportPathUpButton (GtkObject * dialogue);
void UponProjectFileImportPathDownButton (GtkObject * dialogue);
void UponProjectFileImportPathNewButton (GtkObject * dialogue);
void UponProjectFileImportPathDeleteButton (GtkObject * dialogue);
void UponProjectOptionsDefinitionsUpButton (GtkObject * dialogue);
void UponProjectOptionsDefinitionsDownButton (GtkObject * dialogue);
void UponProjectOptionsDefinitionsNewButton (GtkObject * dialogue);
void UponProjectOptionsDefinitionsDeleteButton (GtkObject * dialogue);
void UponProjectOptionsDefinitionsSelectRow (GtkCList * list, gint row, gint column, GdkEvent * event);
void UponProjectOptionsDefinitionsUnselectRow (GtkCList * list, gint row, gint column, GdkEvent * event);
void UponProjectOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable);
void UponProjectOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable);
void UponProjectOptionsDialogueOK (GtkObject * dialogue);
void UponProjectOptionsDialogueCancel (GtkObject * dialogue);
void UponProjectFileImportPathListSelectChild (GtkCList * list, gint row, gint column, GdkEvent * event);
void UponProjectFileImportPathListUnselectChild (GtkCList * list, gint row, gint column, GdkEvent * event);
void UponProjectFileImportPathEntryChange (GtkEditable * editable);
void UponProjectDirectoryBrowse (GtkObject * dialogue);
void UponProjectFileImportPathBrowse (GtkObject * dialogue);
void UponProjectOptions_StructuralSimulation (GtkButton * button, gpointer user_data);
void UponProjectOptions_BehavioralSimulation (GtkButton * button, gpointer user_data);
void UponProjectOptions_FlattenedCompilation (GtkButton * button, gpointer user_data);
void UponProjectOptions_HierarchicalCompilation (GtkButton * button, gpointer user_data);
void UponProjectOptionsB2LOPTSentryChanged (GtkEditable * editable, gpointer user_data);
void UponProjectOptionsBALSACOPTSentryChanged (GtkEditable * editable, gpointer user_data);
void UponProjectOptionsLCDOPTSentryChanged (GtkEditable * editable, gpointer user_data);
void UponProjectOptionsLCDentryChanged (GtkEditable * editable, gpointer user_data);
gboolean UponProjectOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void UponProjectOptions_LardInterpretedSimulation (GtkButton * button, gpointer user_data);
void UponProjectOptions_Lard2C (GtkButton * button, gpointer user_data);
void UponProjectOptions_BreezeDirectSimulation (GtkButton * button, gpointer user_data);
void UponProjectOptions_Breeze2Lard (GtkButton * button, gpointer user_data);
void UponProjectOptions_TraceAllChannels (GtkButton * button, gpointer user_data);
void UponProjectOptions_TraceOnlyInterfacePorts (GtkButton * button, gpointer user_data);
void UponProjectOptions_NoTraceFlushing (GtkButton * button, gpointer user_data);
void UponProjectOptions_TraceFlushingDelaySpin (GtkEditable * editable, gpointer user_data);
void UponProjectOptions_TraceFlushingDelay (GtkButton * button, gpointer user_data);
gboolean UponProjectOptions_TraceFlushingDelaySpin_key (GtkWidget * widget, GdkEventKey * event, gpointer user_data);
void UponProjectOptions_SaveChannelNumbers (GtkToggleButton * togglebutton, gpointer user_data);
void UponProjectOptionsBREEZESIMOPTSentryChanged (GtkEditable * editable, gpointer user_data);
void UponProjectOptionsBREEZELINKOPTSentryChanged (GtkEditable * editable, gpointer user_data);
void UponProjectOptionsDialogueSaveAsDefaultTemplate (GtkButton * button, gpointer user_data);

#endif
/* PROJOPTS_HEADER */
