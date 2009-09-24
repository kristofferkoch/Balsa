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

*/

#ifndef MENU_FILE_HEADER
#define MENU_FILE_HEADER

#include <gtk/gtk.h>

void UponFileMenu_New (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Open (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_ReOpen (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Save (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_SaveAs (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Close (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_CloseAll (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_AddCurrentFileToProject (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_LaunchEditor (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Print (GtkMenuItem * button, gpointer user_data);

void UponFileMenu_New (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Open (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_ReOpen (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Save (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_SaveAs (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Close (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_CloseAll (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_AddCurrentFileToProject (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_LaunchEditor (GtkMenuItem * button, gpointer user_data);
void UponFileMenu_Print (GtkMenuItem * button, gpointer user_data);

#endif
