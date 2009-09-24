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

#include <gdk/gdkkeysyms.h>
#include "optionsdialog.h"
#include "workspace.h"
#include "main.h"
#include "options.h"

struct _OptionsDialog *new_optionsDialog (void)
{
    struct _OptionsDialog *obj = (struct _OptionsDialog *) g_malloc (sizeof (struct _OptionsDialog));

    obj->dialog = NULL;

    return obj;
}

void UponOptionsDialogueOK ()
{
    /* Copy entry values */
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
    GtkWidget *ImportPathsRadioButton1 =
        (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
        "ImportPaths_radiobutton1");
    GtkWidget *ImportPathsRadioButton2 =
        (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
        "ImportPaths_radiobutton2");
    GtkWidget *ImportPathsRadioButton3 =
        (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
        "ImportPaths_radiobutton3");
*/
    char *tmp;

    tmp = gtk_entry_get_text (GTK_ENTRY (BalsaHomeEntry));
    if (tmp && tmp[0])
    {
        //todo: free previous environment options
        EnvironmentOptions.BalsaHome = g_strdup (tmp);
    } else
    {
        EnvironmentOptions.BalsaHome = NULL;
    }
    tmp = gtk_entry_get_text (GTK_ENTRY (TmpDirEntry));
    if (tmp && tmp[0])
    {
        //todo: free previous environment options
        EnvironmentOptions.TmpDir = g_strdup (tmp);
    } else
    {
        EnvironmentOptions.TmpDir = g_strdup ("/tmp");
    }
    tmp = gtk_entry_get_text (GTK_ENTRY (EditorNameEntry));
    if (tmp && tmp[0])
    {
        //todo: free previous environment options
        EnvironmentOptions.editor = g_strdup (tmp);
    } else
    {
        EnvironmentOptions.editor = NULL;
    }
    tmp = gtk_entry_get_text (GTK_ENTRY (PrintCommandEntry));
    if (tmp && tmp[0])
    {
        //todo: free previous environment options
        EnvironmentOptions.PrintCommand = g_strdup (tmp);
    } else
    {
        EnvironmentOptions.PrintCommand = NULL;
    }
    tmp = gtk_entry_get_text (GTK_ENTRY (PSViewerNameEntry));
    if (tmp && tmp[0])
    {
        //todo: free previous environment options
        EnvironmentOptions.PSViewer = g_strdup (tmp);
    } else
    {
        EnvironmentOptions.PSViewer = NULL;
    }
    EnvironmentOptions.projectsHistoryMaxSize = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (HistoryMaxSizeValue));

    /*
       if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ImportPathsRadioButton1)))
       EnvironmentOptions.pathsAbsoluteRelativeOption = 1;
       else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ImportPathsRadioButton2)))
       EnvironmentOptions.pathsAbsoluteRelativeOption = 2;
       else                        //if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ImportPathsRadioButton3)))
       EnvironmentOptions.pathsAbsoluteRelativeOption = 3;
     */

    gtk_widget_hide (workSpace.optionsDialog->dialog);
    WriteEnvironmentOptions ();
}

gboolean UponOptionsDialogueCancel (void)
{
    gtk_widget_hide (workSpace.optionsDialog->dialog);
    return TRUE;
}

gboolean UponOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    if (event->keyval == GDK_Return)
        UponOptionsDialogueOK ();
    else if (event->keyval == GDK_Escape)
        UponOptionsDialogueCancel ();

    return FALSE;
}
