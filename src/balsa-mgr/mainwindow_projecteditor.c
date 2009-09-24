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
#include "menu_project.h"

static void mainWindow_projectEditor_buildToolbar (void)
{
    GtkWidget *toolbar = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (MainWindow), "ProjectToolbar"));
    GtkWidget *button, *iconw;

//    GdkPixmap *icon;
//    GdkBitmap *mask;

    /* Icon New */
    iconw = gtk_pixmap_new (NewPixmap, NewMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Create New Project",     // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectMenu_New,      // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_CreateNewProject", button);

    /* Icon Open */
    iconw = gtk_pixmap_new (OpenRecentPixmap, OpenRecentMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Open Project",           // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectMenu_Open,     // callback
      NULL);                    // user_data
    gtk_signal_connect (GTK_OBJECT (button), "button_press_event", (GtkSignalFunc) OpenRecentButtonPressedCallback, NULL);
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_OpenProject", button);

    /* Icon Save */
    iconw = gtk_pixmap_new (SavePixmap, SaveMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Save Project",           // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectMenu_Save,     // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_SaveProject", button);

    /* Icon AddFile */
    iconw = gtk_pixmap_new (AddFilePixmap, AddFileMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Add Files into Project", // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectMenu_AddFile,  // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_AddFileToProject", button);

    /* Icon Edit */
    iconw = gtk_pixmap_new (EditPixmap, EditMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Launch Editor",          // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectToolbar_LaunchEditor, // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_LaunchEditor", button);

    /* Icon Project Options */
    iconw = gtk_pixmap_new (OptionsPixmap, OptionsMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Edit Project Options",   // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectMenu_ProjectOptions, // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_ProjectOptions", button);

    /* Icon Update */
    iconw = gtk_pixmap_new (UpdatePixmap, UpdateMask); /* icon widget */
    button = gtk_toolbar_append_item (GTK_TOOLBAR (toolbar), NULL, // text
      "Update Project Views",   // tooltip_text
      NULL,                     // tooltip_private_text
      iconw,                    // icon
      UponProjectMenu_Update,   // callback
      NULL);                    // user_data
    gtk_widget_show (button);
    gtk_object_set_data (GTK_OBJECT (toolbar), "Icon_Update", button);
}

void init_mainWindow_projectEditor (void)
{
    mainWindow_projectEditor_buildToolbar ();
}
