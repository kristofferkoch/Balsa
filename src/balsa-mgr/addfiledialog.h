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

	`addfiledialog.h'

*/

#ifndef ADDFILEDIALOG_HEADER
#define ADDFILEDIALOG_HEADER

void showAddFileDialog (void);

void UponAddFileDialog_OKButton ();;
void UponAddFileDialog_ImportNewPathButton (GtkButton * button, gpointer user_data);
void UponAddFileDialog_OKButton_AfterRemoveConfirmation (void);
void UponAddFileDialog_OKButton ();
gboolean UponAddFileDialog_CancelButton ();
gboolean UponAddFileDialog_TreeSelectRow (GtkCTree * ctree, GtkCTreeNode * node, gint column, gpointer user_data);
void UponAddFileDialog_TreeUnselectRow (GtkCTree * ctree, GtkCTreeNode * node, gint column, gpointer user_data);
gboolean UponAddFileDialog_keyPressEvent (GdkEventKey * event);

#endif
/* ADDFILEDIALOG_HEADER */
