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

	`projopts.c'
	Project options dialogue handling

*/

#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gdk/gdkkeysyms.h>
#include "projopts.h"
#include "miscgtk.h"
#include "paths.h"
#include "file.h"
#include "workspace.h"
#include "widgets.h"
#include "signals.h"
#include "utils.h"
#include "project.h"
#include "options.h"

extern int NewProjectCount;
gboolean ProjectOptionsToggleActive = TRUE;

static gboolean DefinitionsEntryDisabled = 0;

void UponProjectFileImport_ConvertRelAbs (GtkObject * dialogue)
{
    GtkWidget *direntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
        "ProjectDirectoryEntry"));
    char *directoryRef = gtk_entry_get_text (GTK_ENTRY (direntry));

    GtkWidget *entry = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectFileImportPathEntry"));
    char *directory = gtk_entry_get_text (GTK_ENTRY (entry));

    if (directory && *directory)
    {
        if (*directory == '/')
            gtk_entry_set_text (GTK_ENTRY (entry), ConvertToRelativePath (directory, directoryRef));
        else
            gtk_entry_set_text (GTK_ENTRY (entry), ConvertToAbsolutePath (directory, directoryRef));
    }
}

void UponProjectFileImportPathUpButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectFileImportPathList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedPath"));

    if (selectedRowNo != PROJ_OPTS_NO_SELECTION)
    {
        gtk_object_set_data (GTK_OBJECT (dialogue), "PathChanged", (gpointer) TRUE);
        gtk_clist_unselect_row (GTK_CLIST (list), selectedRowNo, 0);
        gtk_clist_row_move (GTK_CLIST (list), selectedRowNo, selectedRowNo - 1);
        gtk_clist_select_row (GTK_CLIST (list), selectedRowNo - 1, 0);
    }
}

void UponProjectFileImportPathDownButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectFileImportPathList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedPath"));

    gtk_object_set_data (GTK_OBJECT (dialogue), "PathChanged", (gpointer) TRUE);
    gtk_clist_unselect_row (GTK_CLIST (list), selectedRowNo, 0);
    gtk_clist_row_move (GTK_CLIST (list), selectedRowNo, selectedRowNo + 1);
    gtk_clist_select_row (GTK_CLIST (list), selectedRowNo + 1, 0);
}

void UponProjectFileImportPathNewButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectFileImportPathList"));
    GtkWidget *entry = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectFileImportPathEntry"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedPath"));

    if (selectedRowNo == PROJ_OPTS_NO_SELECTION)
    {
        char *entryContents = gtk_entry_get_text (GTK_ENTRY (entry));

        if (!*entryContents)
            entryContents = ".";
        gtk_clist_insert (GTK_CLIST (list), 0, &entryContents);
        gtk_clist_select_row (GTK_CLIST (list), 0, 0);
    } else
    {
        char *path = ".";

        gtk_clist_insert (GTK_CLIST (list), selectedRowNo + 1, &path);
        gtk_clist_select_row (GTK_CLIST (list), selectedRowNo + 1, 0);
    }
    gtk_object_set_data (GTK_OBJECT (dialogue), "PathChanged", (gpointer) TRUE);
}

void UponProjectFileImportPathDeleteButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue,
        "ProjectFileImportPathList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedPath"));

    gtk_clist_remove (GTK_CLIST (list), selectedRowNo);
    gtk_clist_select_row (GTK_CLIST (list), selectedRowNo, 0);
}

void UponProjectOptionsDefinitionsUpButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue, "DefinesList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));

    if (selectedRowNo != PROJ_OPTS_NO_SELECTION)
    {
        gtk_object_set_data (GTK_OBJECT (dialogue), "DefinesChanged", (gpointer) TRUE);
        gtk_clist_unselect_row (GTK_CLIST (list), selectedRowNo, 0);
        gtk_clist_row_move (GTK_CLIST (list), selectedRowNo, selectedRowNo - 1);
        gtk_clist_select_row (GTK_CLIST (list), selectedRowNo - 1, 0);
    }
}

void UponProjectOptionsDefinitionsDownButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue, "DefinesList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));

    gtk_object_set_data (GTK_OBJECT (dialogue), "DefinesChanged", (gpointer) TRUE);
    gtk_clist_unselect_row (GTK_CLIST (list), selectedRowNo, 0);
    gtk_clist_row_move (GTK_CLIST (list), selectedRowNo, selectedRowNo + 1);
    gtk_clist_select_row (GTK_CLIST (list), selectedRowNo + 1, 0);
}

void UponProjectOptionsDefinitionsNewButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue, "DefinesList"));
    GtkWidget *nameEntry = GTK_WIDGET (gtk_object_get_data (dialogue, "DefineNameEntry"));
    GtkWidget *valueEntry = GTK_WIDGET (gtk_object_get_data (dialogue, "DefineValueEntry"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));

    if (selectedRowNo == PROJ_OPTS_NO_SELECTION)
    {
        char *strings[2];

        strings[0] = gtk_entry_get_text (GTK_ENTRY (nameEntry));
        strings[1] = gtk_entry_get_text (GTK_ENTRY (valueEntry));

        if (!*strings[0])
            strings[0] = "Name";

        gtk_clist_insert (GTK_CLIST (list), 0, strings);
        gtk_clist_select_row (GTK_CLIST (list), 0, 0);
    } else
    {
        char *textName, *textValue;

        textName = gtk_entry_get_text (GTK_ENTRY (nameEntry));
        textValue = gtk_entry_get_text (GTK_ENTRY (valueEntry));
        if (*textName || *textValue)
        {
            char *strings[2];

            strings[0] = "Name";
            strings[1] = "";

            gtk_clist_insert (GTK_CLIST (list), selectedRowNo + 1, strings);
            gtk_clist_select_row (GTK_CLIST (list), selectedRowNo + 1, 0);
        }
    }
    gtk_object_set_data (GTK_OBJECT (dialogue), "DefinesChanged", (gpointer) TRUE);

    /* Make name selected so we can directly overtype it */
    gtk_entry_select_region (GTK_ENTRY (nameEntry), 0, -1);
    gtk_widget_grab_focus (GTK_WIDGET (nameEntry));
}

void UponProjectOptionsDefinitionsDeleteButton (GtkObject * dialogue)
{
    GtkWidget *list = GTK_WIDGET (gtk_object_get_data (dialogue,
        "DefinesList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));

    gtk_clist_remove (GTK_CLIST (list), selectedRowNo);
    gtk_clist_select_row (GTK_CLIST (list), selectedRowNo, 0);
}

void UponProjectOptionsDefinitionsSelectRow (GtkCList * list, gint row, gint column, GdkEvent * event)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (list));
    char *text;
    GtkWidget *nameEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefineNameEntry"));
    GtkWidget *valueEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefineValueEntry"));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesUpButton")), (row == 0 ? FALSE : TRUE));
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesDownButton")), (row == list->rows - 1 ? FALSE : TRUE));
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesDeleteButton")), TRUE);

    gtk_object_set_data (GTK_OBJECT (dialogue), "SelectedDefine", GINT_TO_POINTER (row));

    gtk_clist_get_text (list, row, 0, &text);
    gtk_entry_set_text (GTK_ENTRY (nameEntry), text);
    gtk_clist_get_text (list, row, 1, &text);
    gtk_entry_set_text (GTK_ENTRY (valueEntry), text);
}

void UponProjectOptionsDefinitionsUnselectRow (GtkCList * list, gint row, gint column, GdkEvent * event)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (list));

//    int selectedRowNo = (int) gtk_object_get_data (GTK_OBJECT (dialogue),
//        "SelectedDefine");
    GtkWidget *nameEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefineNameEntry"));
    GtkWidget *valueEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefineValueEntry"));

    /*
       if (selectedRowNo != PROJ_OPTS_NO_SELECTION)
       {
       char *textName, *textValue;
       textName = gtk_entry_get_text (GTK_ENTRY (nameEntry));
       textValue = gtk_entry_get_text (GTK_ENTRY (valueEntry));
       if (!*textName && !*textValue)
       gtk_clist_remove (GTK_CLIST (list), selectedRowNo);
       }
     */

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesUpButton")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesDownButton")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesDeleteButton")), FALSE);

    gtk_object_set_data (GTK_OBJECT (dialogue), "SelectedDefine", GINT_TO_POINTER (PROJ_OPTS_NO_SELECTION));

    DefinitionsEntryDisabled = 1;
    gtk_entry_set_text (GTK_ENTRY (nameEntry), "");
    gtk_entry_set_text (GTK_ENTRY (valueEntry), "");
    DefinitionsEntryDisabled = 0;
}

void UponProjectOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
    GtkCList *list = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));

    if (DefinitionsEntryDisabled)
        return;

    if (selectedRowNo == PROJ_OPTS_NO_SELECTION)
    {
        UponProjectOptionsDefinitionsNewButton (GTK_OBJECT (dialogue));
        selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));
    }
    gtk_clist_set_text (list, selectedRowNo, 0, gtk_entry_get_text (GTK_ENTRY (editable)));
    gtk_object_set_data (GTK_OBJECT (dialogue), "PathChanged", GINT_TO_POINTER (TRUE));
}

void UponProjectOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
    GtkCList *list = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));

    if (DefinitionsEntryDisabled)
        return;

    if (selectedRowNo == PROJ_OPTS_NO_SELECTION)
    {
        UponProjectOptionsDefinitionsNewButton (GTK_OBJECT (dialogue));
        selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedDefine"));
    }
    gtk_clist_set_text (list, selectedRowNo, 1, gtk_entry_get_text (GTK_ENTRY (editable)));
    gtk_object_set_data (GTK_OBJECT (dialogue), "DefinesChanged", GINT_TO_POINTER (TRUE));
}

void UponProjectOptionsDialogueOK (GtkObject * dialogue)
{
    char *project_name;
    char *project_directory;
    char *project_B2LOPTS;
    char *project_BALSACOPTS;
    char *project_BREEZESIMOPTS;
    char *project_BREEZELINKOPTS;

    //    char *project_LCD;
    char *project_LCDOPTS;

//    char *project_BALSATECH;
    enum SimulationSystem project_simulationSystem;

    project_name = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectNameEntry")));
    project_directory = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectDirectoryEntry")));
    project_B2LOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsB2LOPTSentry")));
    project_BALSACOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsBALSACOPTSentry")));
    project_BREEZESIMOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsBREEZESIMOPTSentry")));
    project_BREEZELINKOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsBREEZELINKOPTSentry")));
    //    project_LCD = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsLCDentry")));
    project_LCDOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsLCDOPTSentry")));

    {
        int toggle_BreezeDirectSimulation = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_BreezeDirectSimulation")));
        int toggle_Breeze2Lard = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_Breeze2Lard")));
        int toggle_LardInterpretedSimulation = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_LardInterpretedSimulation")));
        int toggle_Lard2C = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue, "ProjectOptions_Lard2C")));

        if (toggle_Breeze2Lard && toggle_LardInterpretedSimulation)
            project_simulationSystem = interpreter;
        else if (toggle_Breeze2Lard && toggle_Lard2C)
            project_simulationSystem = lard2c;
        else if (toggle_BreezeDirectSimulation && toggle_LardInterpretedSimulation)
            project_simulationSystem = breezesim;
        else                    //if (toggle_BreezeDirectSimulation && toggle_Lard2C)
        {
            printfConsole
              ("Warning: BreezeDirectSimulation & Lard2C not available yet. Automatic switching to BreezeDirectSimulation & LardInterpredSimulation\n");
            project_simulationSystem = breezesim; //development_breezesim_with_lard2c
        }
    }

    /* Check the project directory is OK */
    {
        struct stat dirDetails;
        gboolean badDir = FALSE;

        errno = 0;
        stat (project_directory, &dirDetails);

        if (errno == ENOENT)
        {                       /* File doesn't exist */
            printfConsole ("Project directory must already exist");
            badDir = TRUE;
        }
        /* All other errors, just moan about perms */
        else if (errno || !S_ISDIR (dirDetails.st_mode) || (dirDetails.st_mode & S_IRWXU) != S_IRWXU)
        {
            printfConsole ("Problem with project directory. Need rwx permissions");
            badDir = TRUE;
        }

        if (badDir)
            return;
    }

    /* Check if we are in the case of "New Project" or "Edit Project Options" */
    if (gtk_object_get_data (GTK_OBJECT (dialogue), "Project"))
    {
        // Project Options
        if (strcmp (CurrentBalsaProject->name, project_name))
        {
            CurrentBalsaProject->name = g_strdup (project_name);
            CurrentBalsaProject->dirty = TRUE;
        }
        if (CurrentBalsaProject->simulationSystem != project_simulationSystem)
        {
            CurrentBalsaProject->simulationSystem = project_simulationSystem;
            CurrentBalsaProject->dirty = TRUE;
            CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
        }
        if (strcmp (CurrentBalsaProject->directory, project_directory))
        {
            CurrentBalsaProject->directory = g_strdup (project_directory);
            CurrentBalsaProject->dirty = TRUE;
            chdir (CurrentBalsaProject->directory);
        }
        if ((CurrentBalsaProject->B2LOPTS
            && strcmp (CurrentBalsaProject->B2LOPTS, project_B2LOPTS)) || (!CurrentBalsaProject->B2LOPTS && project_B2LOPTS[0]))
        {
            CurrentBalsaProject->B2LOPTS = g_strdup (project_B2LOPTS);
            CurrentBalsaProject->dirty = TRUE;
            CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
        }
        if ((CurrentBalsaProject->BALSACOPTS
            && strcmp (CurrentBalsaProject->BALSACOPTS, project_BALSACOPTS)) || (!CurrentBalsaProject->BALSACOPTS && project_BALSACOPTS[0]))
        {
            CurrentBalsaProject->BALSACOPTS = g_strdup (project_BALSACOPTS);
            CurrentBalsaProject->dirty = TRUE;
            CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
        }
        if ((CurrentBalsaProject->BREEZESIMOPTS
            && strcmp (CurrentBalsaProject->BREEZESIMOPTS,
              project_BREEZESIMOPTS)) || (!CurrentBalsaProject->BREEZESIMOPTS && project_BREEZESIMOPTS[0]))
        {
            CurrentBalsaProject->BREEZESIMOPTS = g_strdup (project_BREEZESIMOPTS);
            CurrentBalsaProject->dirty = TRUE;
            CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
        }
        if ((CurrentBalsaProject->BREEZELINKOPTS
            && strcmp (CurrentBalsaProject->BREEZELINKOPTS,
              project_BREEZELINKOPTS)) || (!CurrentBalsaProject->BREEZELINKOPTS && project_BREEZELINKOPTS[0]))
        {
            CurrentBalsaProject->BREEZELINKOPTS = g_strdup (project_BREEZELINKOPTS);
            CurrentBalsaProject->dirty = TRUE;
            CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
        }
        /*
           if ((CurrentBalsaProject->LCD
           && strcmp (CurrentBalsaProject->LCD,
           project_LCD)) || (!CurrentBalsaProject->LCD && project_LCD[0]))
           {
           CurrentBalsaProject->LCD = g_strdup (project_LCD);
           CurrentBalsaProject->dirty = TRUE;
           CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
           }
         */
        if ((CurrentBalsaProject->LCDOPTS
            && strcmp (CurrentBalsaProject->LCDOPTS, project_LCDOPTS)) || (!CurrentBalsaProject->LCDOPTS && project_LCDOPTS[0]))
        {
            CurrentBalsaProject->LCDOPTS = g_strdup (project_LCDOPTS);
            CurrentBalsaProject->dirty = TRUE;
            CurrentBalsaProject->dirty_and_need_make_clean = TRUE;
        }
        /*
           if ( CurrentBalsaProject->technology
           && strcmp( CurrentBalsaProject->technology,
           project_BALSATECH ) != 0
           && *project_BALSATECH ) {
           CurrentBalsaProject->technology =
           g_strdup( project_BALSATECH );
           CurrentBalsaProject->dirty = TRUE;
           CurrentBalsaProject->dirty_and_need_make_clean =
           TRUE;
           }
         */
        if ((GtkWidget *) dialogue == workSpace.projectOptionsDialog->dialog)
            workSpace.projectOptionsDialog->dialog = NULL;
    } else
    {
        // New Project
        NewCurrentBalsaProject (project_name, project_directory);
        CurrentBalsaProject->B2LOPTS = g_strdup (project_B2LOPTS);
        CurrentBalsaProject->BALSACOPTS = g_strdup (project_BALSACOPTS);
        CurrentBalsaProject->BREEZESIMOPTS = g_strdup (project_BREEZESIMOPTS);
        CurrentBalsaProject->BREEZELINKOPTS = g_strdup (project_BREEZELINKOPTS);
        //        CurrentBalsaProject->LCD = g_strdup (project_LCD);
        CurrentBalsaProject->LCDOPTS = g_strdup (project_LCDOPTS);
        NewProjectCount++;
        /*
           if ((GtkWidget *) dialogue == workSpace.projectOptionsDialog->dialog_NewProject)
           workSpace.projectOptionsDialog->dialog_NewProject = NULL;
         */
    }

    /* Update project import list */
    if ((gboolean) GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "PathChanged")))
    {
        GtkCList *ImportPathList = GTK_CLIST (gtk_object_get_data (dialogue, "ProjectFileImportPathList"));
        int rowNo;

        // Clear the previous import paths list
        // TODO: free each element
        CurrentBalsaProject->importPath = NULL;

        for (rowNo = ImportPathList->rows - 1; rowNo >= 0; rowNo--)
        {
            char *directory;

            gtk_clist_get_text (ImportPathList, rowNo, 0, &directory);
            AddImportPathToCurrentBalsaProject (directory);
        }

        CurrentBalsaProject->dirty = TRUE;
    }

    /* Update defines list */
    if ((gboolean) GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "DefinesChanged")))
    {
        GtkCList *DefinesList = GTK_CLIST (gtk_object_get_data (dialogue, "DefinesList"));
        int rowNo;

        // Clear the previous defines list
        // TODO: free each element
        CurrentBalsaProject->defines = NULL;

        for (rowNo = DefinesList->rows - 1; rowNo >= 0; rowNo--)
        {
            char *name, *value;

            gtk_clist_get_text (DefinesList, rowNo, 0, &name);
            gtk_clist_get_text (DefinesList, rowNo, 1, &value);

            if (*name)
                AddDefineToCurrentBalsaProject (name, value);
        }

        CurrentBalsaProject->dirty = TRUE;
    }

    /* Close Options Dialog */
    gtk_clist_unselect_all (GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathList")));

    gtk_widget_hide (GTK_WIDGET (dialogue));
    gtk_widget_destroy (GTK_WIDGET (dialogue));

    UpdateProjectTrees ();
}

void UponProjectOptionsDialogueCancel (GtkObject * dialogue)
{
    gtk_clist_unselect_all (GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathList")));

    workSpace.projectOptionsDialog->dialog = NULL;
    //    workSpace.projectOptionsDialog->dialog_NewProject = NULL;
    gtk_widget_hide (GTK_WIDGET (dialogue));
    gtk_widget_destroy (GTK_WIDGET (dialogue));
}

void UponProjectFileImportPathListSelectChild (GtkCList * list, gint row, gint column, GdkEvent * event)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (list));
    char *text;
    GtkWidget *entry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue),
        "ProjectFileImportPathEntry"));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathUpButton")), (row == 0 ? FALSE : TRUE));
    gtk_widget_set_sensitive (GTK_WIDGET
      (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathDownButton")), (row == list->rows - 1 ? FALSE : TRUE));
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathDeleteButton")), TRUE);

    gtk_clist_get_text (list, row, 0, &text);
    gtk_object_set_data (GTK_OBJECT (dialogue), "SelectedPath", GINT_TO_POINTER (row));
    gtk_entry_set_text (GTK_ENTRY (entry), text);
}

void UponProjectFileImportPathListUnselectChild (GtkCList * list, gint row, gint column, GdkEvent * event)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (list));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathUpButton")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathDownButton")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectFileImportPathDeleteButton")), FALSE);

    gtk_object_set_data (GTK_OBJECT (dialogue), "SelectedPath", GINT_TO_POINTER (PROJ_OPTS_NO_SELECTION));
}

void UponProjectFileImportPathEntryChange (GtkEditable * editable)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
    GtkCList *list = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue),
        "ProjectFileImportPathList"));
    int selectedRowNo = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (dialogue), "SelectedPath"));
    char *text;

    if (selectedRowNo == PROJ_OPTS_NO_SELECTION)
    {
        UponProjectFileImportPathNewButton (GTK_OBJECT (dialogue));
        selectedRowNo = 0;
    }

    text = gtk_entry_get_text (GTK_ENTRY (editable));
    gtk_clist_set_text (list, selectedRowNo, 0, text);
    gtk_object_set_data (GTK_OBJECT (dialogue), "PathChanged", GINT_TO_POINTER (TRUE));

    /* Change the conversion button (relative/absolute path) appearance */
    {
        GtkButton *button = GTK_BUTTON (gtk_object_get_data (GTK_OBJECT (dialogue),
            "ConvertButton"));

        if (text && *text)
        {
            GtkLabel *label = GTK_LABEL (GTK_BIN (button)->child);

            if (*text == '/')
                gtk_label_set_text (label, "->Relative path");
            else
                gtk_label_set_text (label, "->Absolute path");

            gtk_widget_show (GTK_WIDGET (button));
        } else
            gtk_widget_hide (GTK_WIDGET (button));
    }
}

void UponProjectDirectoryBrowse (GtkObject * dialogue)
{
    GtkWidget *entry = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectDirectoryEntry"));

    MakeDirectorySelectionDialogue ("Select A Project Directory",
      gtk_entry_get_text (GTK_ENTRY
        (entry)), (FileSelectionCallback) SetEntryStringCallback, (gpointer) GTK_WIDGET (entry), NULL, GTK_WIDGET (dialogue));
}

/*
void AddAbsoluteDirectory (char *directory, GtkWidget * entry)
{
    gtk_entry_set_text (GTK_ENTRY (entry), ImportPaths_ConvertToAbsolutePath (directory));
    gtk_entry_set_position (GTK_ENTRY (entry), -1);
}
*/
void AddRelativeDirectory (char *directory, GtkWidget * entry)
{
    GtkWidget *direntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
        "ProjectDirectoryEntry"));
    char *directoryRef = gtk_entry_get_text (GTK_ENTRY (direntry));

    char *relativePath = ConvertToRelativePath (directory, directoryRef);

    //  printf("path=%s\n", relativePath);
    gtk_entry_set_text (GTK_ENTRY (entry), relativePath);
    gtk_entry_set_position (GTK_ENTRY (entry), -1);
}

gboolean SetDirectoryCallback (char *directory, GtkWidget * entry)
{
    AddRelativeDirectory (directory, entry);
    /*
       switch (EnvironmentOptions.pathsAbsoluteRelativeOption)
       {
       case 1:                    //absolute path
       AddAbsoluteDirectory (directory, entry);
       break;
       case 2:                    //relative path
       AddRelativeDirectory (directory, entry);
       break;
       case 3:                    //ask absolute/relative
       default:
       break;
       }
     */
    return TRUE;                //destroy dialogue
}

void UponProjectFileImportPathBrowse (GtkObject * dialogue)
{
    GtkWidget *entry = GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectFileImportPathEntry"));

    gtk_object_set_data (GTK_OBJECT (dialogue), "PathChanged", GINT_TO_POINTER (TRUE));
    MakeDirectorySelectionDialogue ("Select A File Import Directory",
      gtk_entry_get_text (GTK_ENTRY
        (entry)), (FileSelectionCallback) SetDirectoryCallback, (gpointer) GTK_WIDGET (entry), NULL, GTK_WIDGET (dialogue));
}

void AddOptionTextFrom (GtkEntry * edit, char *optionText)
{
    char *text = gtk_entry_get_text (edit);
    long containsValue = (long) strchr (optionText, '=');

    if (containsValue)
        containsValue -= (long) optionText;

    ProjectOptionsToggleActive = FALSE;
    if (text && *text)
    {
        char **tmp = g_strsplit (text, " ", 1000);
        char **ptr = tmp;

        for (; *ptr; ptr++)
            if ((!containsValue && !strcmp (*ptr, optionText)) || (containsValue && !strncmp (*ptr, optionText, containsValue + 1)))
                goto a;

        text = g_strconcat (text, " ", optionText, NULL);
        gtk_entry_set_text (edit, g_strstrip (text));
      a:
        g_strfreev (tmp);
    } else
        gtk_entry_set_text (edit, optionText);
    ProjectOptionsToggleActive = TRUE;
}

void RemoveOptionTextFrom (GtkEntry * edit, char *optionText)
{
    char *text = gtk_entry_get_text (edit);
    long containsValue = (long) strchr (optionText, '=');

    if (containsValue)
        containsValue -= (long) optionText;

    ProjectOptionsToggleActive = FALSE;
    if (text && *text)
    {
        char **tmp = g_strsplit (text, " ", 1000);
        char **ptr = tmp;

        for (; *ptr; ptr++)
            if ((!containsValue && !strcmp (*ptr, optionText)) || (containsValue && !strncmp (*ptr, optionText, containsValue + 1)))
            {
                g_free (*ptr);
                *ptr = g_strdup ("");
            }

        text = g_strjoinv (" ", tmp);
        gtk_entry_set_text (edit, g_strstrip (text));
        g_strfreev (tmp);
    }
    ProjectOptionsToggleActive = TRUE;
}

void UponProjectOptions_StructuralSimulation (GtkButton * button, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));
        GtkWidget *project_B2LOPTS = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptionsB2LOPTSentry");

        AddOptionTextFrom (GTK_ENTRY (project_B2LOPTS), "-s");
    }
}

void UponProjectOptions_BehavioralSimulation (GtkButton * button, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));
        GtkWidget *project_B2LOPTS = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptionsB2LOPTSentry");

        RemoveOptionTextFrom (GTK_ENTRY (project_B2LOPTS), "-s");
    }
}

void UponProjectOptions_FlattenedCompilation (GtkButton * button, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));
        GtkWidget *project_BALSACOPTS = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptionsBALSACOPTSentry");

        AddOptionTextFrom (GTK_ENTRY (project_BALSACOPTS), "-f");
    }
}

void UponProjectOptions_HierarchicalCompilation (GtkButton * button, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));
        GtkWidget *project_BALSACOPTS = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptionsBALSACOPTSentry");

        RemoveOptionTextFrom (GTK_ENTRY (project_BALSACOPTS), "-f");
    }
}

int ProjectOptions_UpdateCheckBox (GtkToggleButton * toggle1, GtkToggleButton * toggle2, GtkEntry * edit, char *keyword)
{
    int value = 0;
    gboolean found = FALSE;
    char *text = gtk_entry_get_text (edit);
    long containsValue = (long) strchr (keyword, '=');

    if (containsValue)
        containsValue -= (long) keyword;

    if (text && *text)
    {
        char **tmp = g_strsplit (text, " ", 1000);
        char **ptr = tmp;

        for (; *ptr; ptr++)
            if ((!containsValue && !strcmp (*ptr, keyword)) || (containsValue && !strncmp (*ptr, keyword, containsValue + 1)))
            {
                found = TRUE;
                if (containsValue)
                    value = atoi ((*ptr) + containsValue + 1);
                goto a;
            }

      a:
        g_strfreev (tmp);
    }

    if (text)
    {
        ProjectOptionsToggleActive = FALSE;
        gtk_toggle_button_set_active (found ? toggle1 : toggle2, TRUE);
        ProjectOptionsToggleActive = TRUE;
    }

    return value;
}

int ProjectOptions_UpdateToggle (GtkToggleButton * toggle, GtkEntry * edit, char *keyword)
{
    int value = 0;
    gboolean found = FALSE;
    char *text = gtk_entry_get_text (edit);
    long containsValue = (long) strchr (keyword, '=');

    if (containsValue)
        containsValue -= (long) keyword;

    if (text && *text)
    {
        char **tmp = g_strsplit (text, " ", 1000);
        char **ptr = tmp;

        for (; *ptr; ptr++)
            if ((!containsValue && !strcmp (*ptr, keyword)) || (containsValue && !strncmp (*ptr, keyword, containsValue + 1)))
            {
                found = TRUE;
                if (containsValue)
                    value = atoi ((*ptr) + containsValue + 1);
                goto a;
            }

      a:
        g_strfreev (tmp);
    }

    if (text)
    {
        ProjectOptionsToggleActive = FALSE;
        gtk_toggle_button_set_active (toggle, found);
        ProjectOptionsToggleActive = TRUE;
    }

    return value;
}

void UponProjectOptionsB2LOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
        GtkWidget *project_StructuralSimulation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_StructuralSimulation");
        GtkWidget *project_BehavioralSimulation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_BehavioralSimulation");

        ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
          (project_StructuralSimulation_checkbox), GTK_TOGGLE_BUTTON (project_BehavioralSimulation_checkbox), GTK_ENTRY (editable), "-s");
    }
}

void UponProjectOptionsBALSACOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
        GtkWidget *project_FlattenedCompilation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_FlattenedCompilation");
        GtkWidget *project_HierarchicalCompilation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_HierarchicalCompilation");

        ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
          (project_FlattenedCompilation_checkbox), GTK_TOGGLE_BUTTON (project_HierarchicalCompilation_checkbox), GTK_ENTRY (editable), "-f");
    }
}

void UponProjectOptionsLCDOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    /*
       if (ProjectOptionsToggleActive)
       {
       GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
       GtkWidget *project_InterpretedCompilation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
       "ProjectOptions_InterpretedSimulation");
       GtkWidget *project_Breeze2Lard2C_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
       "ProjectOptions_Breeze2Lard2C");
       GtkWidget *project_Breeze2C_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
       "ProjectOptions_Breeze2C");

       GtkEntry *LCDentry = gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectOptionsLCDentry");
       char *text = gtk_entry_get_text (LCDentry);
       if (text && *text && !strcmp(text,"balsa-l2cd")) {
       ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
       (project_Breeze2C_checkbox),
       GTK_TOGGLE_BUTTON (project_Breeze2Lard2C_checkbox), GTK_ENTRY (editable), "-breeze2c");
       } else {
       ProjectOptionsToggleActive = FALSE;
       gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(project_InterpretedCompilation_checkbox), TRUE);
       ProjectOptionsToggleActive = TRUE;
       }
       }
     */
}

void UponProjectOptionsLCDentryChanged (GtkEditable * editable, gpointer user_data)
{
    /*
       // same as LCDOPTS
       UponProjectOptionsLCDOPTSentryChanged (editable, user_data);
     */
}

void ProjectOptions_UpdateCheckBoxes (void)
{
    GtkWidget *dialogue = workSpace.projectOptionsDialog->dialog;
    GtkWidget *project_StructuralSimulation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_StructuralSimulation");
    GtkWidget *project_BehavioralSimulation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_BehavioralSimulation");
    GtkWidget *project_FlattenedCompilation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_FlattenedCompilation");
    GtkWidget *project_HierarchicalCompilation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_HierarchicalCompilation");
    GtkWidget *project_BreezeDirectSimulation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_BreezeDirectSimulation");
    GtkWidget *project_Breeze2Lard_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_Breeze2Lard");
    GtkWidget *project_LardInterpretedSimulation_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_LardInterpretedSimulation");
    GtkWidget *project_Lard2C_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_Lard2C");
    GtkWidget *project_TraceAllChannels_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_TraceAllChannels");
    GtkWidget *project_TraceOnlyInterfacePorts_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_TraceOnlyInterfacePorts");
    GtkWidget *project_NoTraceFlushing_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_NoTraceFlushing");
    GtkWidget *project_TraceFlushingDelay_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_TraceFlushingDelay");
    GtkWidget *project_SaveChannelNumbers = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptions_SaveChannelNumbers");
    GtkWidget *B2LOPTSentry = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptionsB2LOPTSentry");
    GtkWidget *BALSACOPTSentry = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptionsBALSACOPTSentry");
    GtkWidget *BREEZESIMOPTSentry = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptionsBREEZESIMOPTSentry");
    GtkWidget *BREEZELINKOPTSentry = gtk_object_get_data (GTK_OBJECT (dialogue),
      "ProjectOptionsBREEZELINKOPTSentry");

//    GtkWidget *LCDOPTSentry = gtk_object_get_data (GTK_OBJECT (dialogue),
//        "ProjectOptionsLCDOPTSentry");

    ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
      (project_StructuralSimulation_checkbox), GTK_TOGGLE_BUTTON (project_BehavioralSimulation_checkbox), GTK_ENTRY (B2LOPTSentry), "-s");
    ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
      (project_FlattenedCompilation_checkbox), GTK_TOGGLE_BUTTON (project_HierarchicalCompilation_checkbox), GTK_ENTRY (BALSACOPTSentry), "-f");
    ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
      (project_TraceAllChannels_checkbox),
      GTK_TOGGLE_BUTTON (project_TraceOnlyInterfacePorts_checkbox), GTK_ENTRY (BREEZESIMOPTSentry), "--traceallchans");
    ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
      (project_TraceFlushingDelay_checkbox), GTK_TOGGLE_BUTTON (project_NoTraceFlushing_checkbox), GTK_ENTRY (BREEZESIMOPTSentry), "--flush=");
    ProjectOptions_UpdateToggle (GTK_TOGGLE_BUTTON (project_SaveChannelNumbers), GTK_ENTRY (BREEZELINKOPTSentry), "--save-channel-numbers");

    /*
       {
       GtkEntry *LCDentry = gtk_object_get_data (GTK_OBJECT (dialogue), "ProjectOptionsLCDentry");
       char *text = gtk_entry_get_text (LCDentry);
       if (text && *text && !strcmp(text,"balsa-l2cd")) {
       ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
       (project_Breeze2C_checkbox),
       GTK_TOGGLE_BUTTON (project_Breeze2Lard2C_checkbox), GTK_ENTRY (LCDOPTSentry), "-breeze2c");
       } else {
       ProjectOptionsToggleActive = FALSE;
       gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(project_InterpretedCompilation_checkbox), TRUE);
       ProjectOptionsToggleActive = TRUE;
       }
       }
     */

    ProjectOptionsToggleActive = FALSE;
    switch (CurrentBalsaProject->simulationSystem)
    {
    case interpreter:
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project_Breeze2Lard_checkbox), TRUE);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project_LardInterpretedSimulation_checkbox), TRUE);
        break;
    case lard2c:
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project_Breeze2Lard_checkbox), TRUE);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project_Lard2C_checkbox), TRUE);
        break;
    case breezesim:
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project_BreezeDirectSimulation_checkbox), TRUE);
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (project_LardInterpretedSimulation_checkbox), TRUE);
    }
    ProjectOptionsToggleActive = TRUE;
}

void ProjectOptions_DisplayWindow (gboolean newProject)
{
    PtrBalsaProject project = CurrentBalsaProject;

    if (!project)
        return;

    if (GTK_IS_DIALOG (workSpace.projectOptionsDialog->dialog))
    {
        gdk_window_raise (workSpace.projectOptionsDialog->dialog->window);
        /*
           gtk_widget_hide (workSpace.projectOptionsDialog->dialog);
           gtk_widget_show (workSpace.projectOptionsDialog->dialog);
         */
        return;
    }

    workSpace.projectOptionsDialog->dialog = create_ProjectOptionsDialogue ();

    /*
       if (newProject) {
       gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectDirectoryEntry")), TRUE);
       gtk_widget_show (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectDirectoryBrowseButton")) );
       } else {
       gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectDirectoryEntry")), FALSE);
       gtk_widget_hide (GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectDirectoryBrowseButton")) );
       }
     */

    /* This is a dialogue for an existing project */
    gtk_object_set_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "Project", GINT_TO_POINTER (project));
    gtk_object_set_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "SelectedPath", GINT_TO_POINTER (PROJ_OPTS_NO_SELECTION));
    gtk_object_set_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "PathChanged", GINT_TO_POINTER (FALSE));
    gtk_object_set_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "SelectedDefine", GINT_TO_POINTER (PROJ_OPTS_NO_SELECTION));
    gtk_object_set_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "DefinesChanged", GINT_TO_POINTER (FALSE));

    /* update the file list here to get directory updates right */
    //UpdateBalsaProjectFromFileView (project);

    /* Set fields from the Project structure */
    gtk_entry_set_text (GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectNameEntry")), project->name);
    gtk_entry_set_text (GTK_ENTRY
      (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectDirectoryEntry")), project->directory);
    gtk_entry_set_text (GTK_ENTRY
      (gtk_object_get_data
        (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectOptionsB2LOPTSentry")), project->B2LOPTS ? project->B2LOPTS : "");
    gtk_entry_set_text (GTK_ENTRY
      (gtk_object_get_data
        (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectOptionsBALSACOPTSentry")), project->BALSACOPTS ? project->BALSACOPTS : "");
    gtk_entry_set_text (GTK_ENTRY
      (gtk_object_get_data
        (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
          "ProjectOptionsBREEZESIMOPTSentry")), project->BREEZESIMOPTS ? project->BREEZESIMOPTS : "");
    gtk_entry_set_text (GTK_ENTRY
      (gtk_object_get_data
        (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
          "ProjectOptionsBREEZELINKOPTSentry")), project->BREEZELINKOPTS ? project->BREEZELINKOPTS : "");
    /*
       gtk_entry_set_text (GTK_ENTRY
       (gtk_object_get_data
       (GTK_OBJECT
       (workSpace.projectOptionsDialog->dialog),
       "ProjectOptionsLCDentry")), project->LCD ? project->LCD : "");
     */
    gtk_entry_set_text (GTK_ENTRY
      (gtk_object_get_data
        (GTK_OBJECT (workSpace.projectOptionsDialog->dialog), "ProjectOptionsLCDOPTSentry")), project->LCDOPTS ? project->LCDOPTS : "");

    ProjectOptions_UpdateCheckBoxes ();

    /* Set file import path */
    {
        GtkCList *ImportPathList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
            "ProjectFileImportPathList"));
        GList *strings = project->importPath;

        while (strings)
        {
            gtk_clist_insert (ImportPathList, ImportPathList->rows, (char **) &(strings->data));
            strings = strings->next;
        }
    }

    /* Add defines */
    {
        GtkCList *DefinesList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
            "DefinesList"));
        GList *defines = project->defines;
        char *strings[2];

        while (defines)
        {
            strings[0] = ((PtrDefine) defines->data)->name;
            strings[1] = ((PtrDefine) defines->data)->value;
            gtk_clist_insert (DefinesList, DefinesList->rows, (char **) strings);
            defines = defines->next;
        }
    }

    /* Make name selected so we can directly overtype it */
    {
        GtkEntry *nameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (workSpace.projectOptionsDialog->dialog),
            "ProjectNameEntry"));

        gtk_entry_select_region (GTK_ENTRY (nameEntry), 0, -1);
        gtk_widget_grab_focus (GTK_WIDGET (nameEntry));
    }

    gtk_widget_show (workSpace.projectOptionsDialog->dialog);
}

gboolean UponProjectOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    if (event->keyval == GDK_Return)
        UponProjectOptionsDialogueOK (GTK_OBJECT (widget));
    else if (event->keyval == GDK_Escape)
        UponProjectOptionsDialogueCancel (GTK_OBJECT (widget));

    return FALSE;
}

void UponProjectOptions_LardInterpretedSimulation (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_BreezeDirectSimulation")), TRUE);
}

void UponProjectOptions_Lard2C (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_BreezeDirectSimulation")), FALSE);
}

void UponProjectOptions_BreezeDirectSimulation (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_Lard2C")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "FrameStructuralBehavioralChoice")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_SaveChannelNumbers")), TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptionsLCDOPTSentry")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptionsB2LOPTSentry")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptionsBREEZELINKOPTSentry")), TRUE);
}

void UponProjectOptions_Breeze2Lard (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_Lard2C")), TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "FrameStructuralBehavioralChoice")), TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_SaveChannelNumbers")), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptionsLCDOPTSentry")), TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptionsB2LOPTSentry")), TRUE);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptionsBREEZELINKOPTSentry")), FALSE);
}

void UponProjectOptions_TraceAllChannels (GtkButton * button, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));
        GtkWidget *project_BREEZESIMOPTS = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptionsBREEZESIMOPTSentry");

        AddOptionTextFrom (GTK_ENTRY (project_BREEZESIMOPTS), "--traceallchans");
    }
}

void UponProjectOptions_TraceOnlyInterfacePorts (GtkButton * button, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));
        GtkWidget *project_BREEZESIMOPTS = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptionsBREEZESIMOPTSentry");

        RemoveOptionTextFrom (GTK_ENTRY (project_BREEZESIMOPTS), "--traceallchans");
    }
}

void UponProjectOptions_NoTraceFlushing (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_TraceFlushingDelaySpin")), FALSE);

    if (ProjectOptionsToggleActive)
    {
        GtkWidget *project_BREEZESIMOPTS = gtk_object_get_data (dialogue,
          "ProjectOptionsBREEZESIMOPTSentry");

        RemoveOptionTextFrom (GTK_ENTRY (project_BREEZESIMOPTS), "--flush=");
    }
}

void UponProjectOptions_TraceFlushingDelaySpin (GtkEditable * editable, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (editable)));
        GtkWidget *project_BREEZESIMOPTS = gtk_object_get_data (dialogue,
          "ProjectOptionsBREEZESIMOPTSentry");
        char *valueStr = gtk_editable_get_chars (editable, 0, -1);
        int value = atoi (valueStr);
        char *text = g_strdup_printf ("--flush=%d", value);

        ProjectOptionsToggleActive = FALSE;
        RemoveOptionTextFrom (GTK_ENTRY (project_BREEZESIMOPTS), "--flush=");
        AddOptionTextFrom (GTK_ENTRY (project_BREEZESIMOPTS), text);
        ProjectOptionsToggleActive = TRUE;

        g_free (text);
    }
}

void UponProjectOptions_TraceFlushingDelay (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (dialogue, "ProjectOptions_TraceFlushingDelaySpin")), TRUE);

    if (ProjectOptionsToggleActive)
    {
//        GtkWidget *project_BREEZESIMOPTS = gtk_object_get_data (dialogue,
//            "ProjectOptionsBREEZESIMOPTSentry");

        UponProjectOptions_TraceFlushingDelaySpin (gtk_object_get_data (dialogue, "ProjectOptions_TraceFlushingDelaySpin"), NULL);
    }
}

gboolean UponProjectOptions_TraceFlushingDelaySpin_key (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    //    UponProjectOptions_TraceFlushingDelaySpin (GTK_EDITABLE (widget), user_data);
    return TRUE;
}

void UponProjectOptions_SaveChannelNumbers (GtkToggleButton * togglebutton, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (togglebutton)));

    if (ProjectOptionsToggleActive)
    {
        GtkWidget *project_BREEZELINKOPTS = gtk_object_get_data (dialogue,
          "ProjectOptionsBREEZELINKOPTSentry");
        int active = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_SaveChannelNumbers")));

        if (active)
            AddOptionTextFrom (GTK_ENTRY (project_BREEZELINKOPTS), "--save-channel-numbers");
        else
            RemoveOptionTextFrom (GTK_ENTRY (project_BREEZELINKOPTS), "--save-channel-numbers");
    }
}

void UponProjectOptionsBREEZESIMOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
        GtkWidget *project_TraceAllChannels_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_TraceAllChannels");
        GtkWidget *project_TraceOnlyInterfacePorts_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_TraceOnlyInterfacePorts");
        GtkWidget *project_NoTraceFlushing_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_NoTraceFlushing");
        GtkWidget *project_TraceFlushingDelay_checkbox = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_TraceFlushingDelay");

        ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON
          (project_TraceAllChannels_checkbox), GTK_TOGGLE_BUTTON (project_TraceOnlyInterfacePorts_checkbox), GTK_ENTRY (editable), "--traceallchans");
        int value = ProjectOptions_UpdateCheckBox (GTK_TOGGLE_BUTTON (project_TraceFlushingDelay_checkbox),
          GTK_TOGGLE_BUTTON (project_NoTraceFlushing_checkbox),
          GTK_ENTRY (editable), "--flush=");

        if (value > 0)
        {
            GtkWidget *project_TraceFlushingDelay_spin = gtk_object_get_data (GTK_OBJECT (dialogue),
              "ProjectOptions_TraceFlushingDelaySpin");

            ProjectOptionsToggleActive = FALSE;
            gtk_spin_button_set_value (GTK_SPIN_BUTTON (project_TraceFlushingDelay_spin), (gfloat) value);
            ProjectOptionsToggleActive = TRUE;
        }
    }
}

void UponProjectOptionsBREEZELINKOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    if (ProjectOptionsToggleActive)
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
        GtkWidget *project_SaveChannelNumbers = gtk_object_get_data (GTK_OBJECT (dialogue),
          "ProjectOptions_SaveChannelNumbers");

        ProjectOptions_UpdateToggle (GTK_TOGGLE_BUTTON (project_SaveChannelNumbers), GTK_ENTRY (editable), "--save-channel-numbers");
    }
}

void UponProjectOptionsDialogueSaveAsDefaultTemplate (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));
    char *project_B2LOPTS;
    char *project_BALSACOPTS;
    char *project_BREEZESIMOPTS;
    char *project_BREEZELINKOPTS;

    //    char *project_LCD;
    char *project_LCDOPTS;

//    char *project_BALSATECH;
    char *project_simulationSystem = NULL;

    project_B2LOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsB2LOPTSentry")));
    project_BALSACOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsBALSACOPTSentry")));
    project_BREEZESIMOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsBREEZESIMOPTSentry")));
    project_BREEZELINKOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsBREEZELINKOPTSentry")));
    //    project_LCD = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsLCDentry")));
    project_LCDOPTS = gtk_entry_get_text (GTK_ENTRY (gtk_object_get_data (dialogue, "ProjectOptionsLCDOPTSentry")));
    {
        int toggle_BreezeDirectSimulation = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_BreezeDirectSimulation")));
        int toggle_Breeze2Lard = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_Breeze2Lard")));
        int toggle_LardInterpretedSimulation = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue,
              "ProjectOptions_LardInterpretedSimulation")));
        int toggle_Lard2C = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (gtk_object_get_data (dialogue, "ProjectOptions_Lard2C")));

        if (toggle_Breeze2Lard && toggle_LardInterpretedSimulation)
            project_simulationSystem = "interpreter";
        else if (toggle_Breeze2Lard && toggle_Lard2C)
            project_simulationSystem = "lard2c";
        else if (toggle_BreezeDirectSimulation && toggle_LardInterpretedSimulation)
            project_simulationSystem = "breezesim";
    }

    if (EnvironmentOptions.newProjectTemplate)
    {
        g_slist_free (EnvironmentOptions.newProjectTemplate);
        EnvironmentOptions.newProjectTemplate = NULL;
    }

    GSList *list = NULL;

    if (project_B2LOPTS && *project_B2LOPTS)
        list = g_slist_prepend (list, g_strdup_printf ("B2LOPTS=%s", project_B2LOPTS));
    if (project_BALSACOPTS && *project_BALSACOPTS)
        list = g_slist_prepend (list, g_strdup_printf ("BALSACOPTS=%s", project_BALSACOPTS));
    //    if (project_BREEZESIMOPTS && *project_BREEZESIMOPTS)
    //        list = g_slist_prepend (list, g_strdup_printf ("BREEZESIMOPTS=%s", project_BREEZESIMOPTS));
    if (project_BREEZELINKOPTS && *project_BREEZELINKOPTS)
        list = g_slist_prepend (list, g_strdup_printf ("BREEZELINKOPTS=%s", project_BREEZELINKOPTS));
    //    if (project_LCD && *project_LCD)
    //        list = g_slist_prepend (list, g_strdup_printf ("LCD=%s", project_LCD));
    if (project_LCDOPTS && *project_LCDOPTS)
        list = g_slist_prepend (list, g_strdup_printf ("LCDOPTS=%s", project_LCDOPTS));
    if (project_simulationSystem)
        list = g_slist_prepend (list, g_strdup_printf ("simulation-system=%s", project_simulationSystem));

    EnvironmentOptions.newProjectTemplate = list;
    WriteEnvironmentOptions ();
}
