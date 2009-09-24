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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "workspace.h"
#include "mainwindow.h"
#include <gtk/gtk.h>
#include "icons.h"
#include "menu_file.h"
#include "filemanager.h"

static void mainWindow_filesEditor_buildToolbar (void)
{
    GtkWidget *toolbar = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (MainWindow), "FilesToolbar"));
    GtkWidget *button, *iconw;

//    GdkPixmap *icon;
//    GdkBitmap *mask;

    /* Icon New */
    iconw = gtk_pixmap_new (NewPixmap, NewMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Create New File",        // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponFileMenu_New,         // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_NewFile", button);

    /* Icon Open */
    iconw = gtk_pixmap_new (OpenPixmap, OpenMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Open File",              // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponFileMenu_Open,        // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_OpenFile", button);

    /* Icon Save */
    iconw = gtk_pixmap_new (SavePixmap, SaveMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Save File",              // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponFileMenu_Save,        // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_SaveFile", button);

    /* Icon Close */
    iconw = gtk_pixmap_new (ClosePixmap, CloseMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Close File",             // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponFileMenu_Close,       // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_CloseFile", button);

    /* Icon Edit */
    iconw = gtk_pixmap_new (EditPixmap, EditMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Launch Editor",          // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponFileMenu_LaunchEditor, // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_LaunchEditor", button);

    /* Icon Print */
    iconw = gtk_pixmap_new (PrintPixmap, PrintMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Print",                  // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponFileMenu_Print,       // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_Print", button);
}

void init_mainWindow_filesEditor (void)
{
    mainWindow_filesEditor_buildToolbar ();
    FileManager_Initialize ();
}
