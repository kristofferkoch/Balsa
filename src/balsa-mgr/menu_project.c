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

#include "menu_project.h"
#include "project.h"
#include "projopts.h"
#include "workspace.h"
#include "widgets.h"

void UponProjectMenu_New_afterSaveConfirmation (GtkMenuItem * button, gpointer user_data)
{
    NewCurrentBalsaProject ("Project Name", g_strdup (StartupPWD));
    AddImportPathToCurrentBalsaProject (".");
    CurrentBalsaProject->dirty = FALSE;
    UpdateProjectTrees ();
    ProjectOptions_DisplayWindow (TRUE);
}

void UponProjectMenu_New (GtkMenuItem * button, gpointer user_data)
{
    ConfirmSaveCurrentBalsaProjectAndExecute (UponProjectMenu_New_afterSaveConfirmation);
}

void UponProjectMenu_ProjectOptions (GtkMenuItem * button, gpointer user_data)
{
    ProjectOptions_DisplayWindow (FALSE);
}

void UponProjectMenu_EnvironmentOptions (GtkMenuItem * button, gpointer user_data)
{
    if (workSpace.optionsDialog->dialog == NULL)
        workSpace.optionsDialog->dialog = create_OptionsDialogue ();

    {
        GtkWidget *BalsaHomeEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
          "BalsaHomeEntry");
        GtkWidget *TmpDirEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
          "TmpDirEntry");
        GtkWidget *EditorNameEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
          "EditorNameEntry");
        GtkWidget *PrintCommandEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
          "PrintCommandEntry");
        GtkWidget *PSViewerNameEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
          "PSViewerNameEntry");
        GtkWidget *HistoryMaxSizeValue = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
          "HistoryMaxSizeValue");

        /*
           GtkWidget *ImportPathsRadioButton1 = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog), "ImportPaths_radiobutton1");
           GtkWidget *ImportPathsRadioButton2 = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog), "ImportPaths_radiobutton2");
           GtkWidget *ImportPathsRadioButton3 = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog), "ImportPaths_radiobutton3");
         */

        gtk_entry_set_text (GTK_ENTRY (BalsaHomeEntry), EnvironmentOptions.BalsaHome ? EnvironmentOptions.BalsaHome : "");
        gtk_entry_set_text (GTK_ENTRY (TmpDirEntry), EnvironmentOptions.TmpDir ? EnvironmentOptions.TmpDir : "/tmp");
        gtk_entry_set_text (GTK_ENTRY (EditorNameEntry), EnvironmentOptions.editor ? EnvironmentOptions.editor : "");
        gtk_entry_set_text (GTK_ENTRY (PrintCommandEntry), EnvironmentOptions.PrintCommand ? EnvironmentOptions.PrintCommand : "");
        gtk_entry_set_text (GTK_ENTRY (PSViewerNameEntry), EnvironmentOptions.PSViewer ? EnvironmentOptions.PSViewer : "");
        gtk_spin_button_set_value (GTK_SPIN_BUTTON (HistoryMaxSizeValue), EnvironmentOptions.projectsHistoryMaxSize);

        /*
           switch (EnvironmentOptions.pathsAbsoluteRelativeOption)
           {
           case 1:
           gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ImportPathsRadioButton1), TRUE);
           break;
           case 2:
           gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ImportPathsRadioButton2), TRUE);
           break;
           case 3:
           default:
           gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ImportPathsRadioButton3), TRUE);
           break;
           }
         */

        gtk_widget_show (workSpace.optionsDialog->dialog);
        gdk_window_raise (workSpace.optionsDialog->dialog->window);
    }
}
