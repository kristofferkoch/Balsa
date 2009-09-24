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

	`toolseditor.h'

*/

#ifndef TOOLSEDITOR_HEADER
#define TOOLSEDITOR_HEADER

#include <gtk/gtk.h>

extern void UpdateToolsMenu (void);
extern void UpdateToolsMenuGreying (void);

void UponToolsMenu_AddTool (GtkMenuItem * menuitem, gpointer user_data);
void UponToolsMenu_ExecuteTool (GtkMenuItem * menuitem, gpointer user_data);
void UponToolsDialog_ButtonOK (GtkButton * button, gpointer user_data);
void UponToolsDialog_ButtonCancel (GtkButton * button, gpointer user_data);
void UponToolsMenu_AddTool (GtkMenuItem * menuitem, gpointer user_data);
void UponToolsMenu_ExecuteTool (GtkMenuItem * menuitem, gpointer user_data);
void UponToolsList_selectRow (GtkCList * toolsList, gint row, gint column, GdkEvent * event, gpointer user_data);
void UponToolsList_unselectRow (GtkCList * toolsList, gint row, gint column, GdkEvent * event, gpointer user_data);
void UponToolNameEntryChanged (GtkEditable * editable, gpointer user_data);
void UponToolCommandEntryChanged (GtkEditable * editable, gpointer user_data);
gboolean UponToolsMenuEditorDialog_delete (GtkWidget * widget, GdkEvent * event, gpointer user_data);
void UponToolsDialog_buttonNew (GtkButton * button, gpointer user_data);
void UponToolsDialog_buttonCopy (GtkButton * button, gpointer user_data);
void UponToolsDialog_buttonRemove (GtkButton * button, gpointer user_data);
void UponToolsDialog_buttonUp (GtkButton * button, gpointer user_data);
void UponToolsDialog_buttonDown (GtkButton * button, gpointer user_data);

#endif
/* TOOLSEDITOR_HEADER */
