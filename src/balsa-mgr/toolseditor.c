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

	`toolseditor.c'

*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>

#include "toolseditor.h"
#include "main.h"
#include "options.h"
#include "utils.h"
#include "project.h"
#include "file.h"
#include "filemanager.h"
#include "paths.h"
#include "executionmanager.h"

void UponToolsMenu_AddTool (GtkMenuItem * menuitem, gpointer user_data);
void UponToolsMenu_ExecuteTool (GtkMenuItem * menuitem, gpointer user_data);
void UpdateToolsDialogGreying (gboolean greying);

void UpdateToolsMenu (void)
{
    GtkMenu *ToolsMenu = GTK_MENU (gtk_menu_new ());
    GtkAccelGroup *menu_accels = gtk_menu_ensure_uline_accel_group (ToolsMenu);
    GtkWidget *menuItem;
    guint accel_key;

    // add each tool (defined in .balsa-mgr and read in the structure EnvironmentOptions)
    int toolnum = 0;
    GSList *tmp = EnvironmentOptions.tools;

    for (; tmp; tmp = tmp->next, toolnum++)
    {
        char **tool = g_strsplit (tmp->data, ",", 2);

        menuItem = gtk_menu_item_new_with_label ("");
        accel_key = gtk_label_parse_uline (GTK_LABEL (GTK_BIN (menuItem)->child), tool[0]);
        gtk_widget_add_accelerator (menuItem, "activate_item", menu_accels, accel_key, 0, 0);
        gtk_widget_show (menuItem);
        gtk_menu_append (ToolsMenu, menuItem);

        gtk_signal_connect (GTK_OBJECT (menuItem), "activate", GTK_SIGNAL_FUNC (UponToolsMenu_ExecuteTool), tool[1]);

        gtk_object_set_data (MainWindowObject, g_strdup_printf ("ToolsMenu_ToolNum%d", toolnum), menuItem);
    }

    // add separator
    {
        menuItem = gtk_menu_item_new ();
        gtk_widget_show (menuItem);
        gtk_menu_append (ToolsMenu, menuItem);
        gtk_widget_set_sensitive (menuItem, FALSE);
    }
    // add "Add/Remove tools..." entry
    {
        menuItem = gtk_menu_item_new_with_label ("");
        accel_key = gtk_label_parse_uline (GTK_LABEL (GTK_BIN (menuItem)->child), "Add/Remove _Tools...");
        gtk_widget_add_accelerator (menuItem, "activate_item", menu_accels, accel_key, 0, 0);
        gtk_widget_show (menuItem);
        gtk_menu_append (ToolsMenu, menuItem);

        gtk_signal_connect (GTK_OBJECT (menuItem), "activate", GTK_SIGNAL_FUNC (UponToolsMenu_AddTool), NULL);
    }

    /* Remove the previous Tools menu 
       and add the new one */
    {
        GtkMenuItem *toolsMenuItem = GTK_MENU_ITEM (gtk_object_get_data (MainWindowObject, "ToolsMenu"));

        //   printf("toolsMenuItem=%d\n",toolsMenuItem);
        gtk_menu_item_remove_submenu (toolsMenuItem);
        //   printf("toolsMenu=%d\n",ToolsMenu);
        gtk_menu_item_set_submenu (toolsMenuItem, GTK_WIDGET (ToolsMenu));
    }

    UpdateToolsMenuGreying ();
}

void FillToolsEditorListWithEnvironmentOptions (void)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    GSList *tmp;

    gtk_clist_clear (toolsList);

    for (tmp = EnvironmentOptions.tools; tmp; tmp = tmp->next)
    {
        char **tool = g_strsplit (tmp->data, ",", 2);

        if (tool[1] == NULL)
            tool[1] = g_strdup ("");

        gtk_clist_append (toolsList, tool);
    }
}

void FillEnvironmentOptionsWithToolsEditorList (void)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    unsigned int row = 0;

    g_slist_free (EnvironmentOptions.tools);
    EnvironmentOptions.tools = NULL;

    for (;; row++)
    {
        GtkCellType cellType = gtk_clist_get_cell_type (toolsList, row, 0);

        if ((cellType == GTK_CELL_EMPTY) || ((int) cellType == -1) || (row > 100))
            break;
        {
            char *toolName, *toolCommand, *toolLine;

            gtk_clist_get_text (toolsList, row, 0, &toolName);
            gtk_clist_get_text (toolsList, row, 1, &toolCommand);
            toolLine = g_strdup_printf ("%s,%s", toolName, toolCommand);
            EnvironmentOptions.tools = g_slist_append (EnvironmentOptions.tools, toolLine);
        }
    }

    WriteEnvironmentOptions ();
}

void UponToolsDialog_ButtonOK (GtkButton * button, gpointer user_data)
{
    FillEnvironmentOptionsWithToolsEditorList ();
    UpdateToolsMenu ();
    gtk_widget_hide (ToolsMenuEditorDialog);
}

void UponToolsDialog_ButtonCancel (GtkButton * button, gpointer user_data)
{
    gtk_widget_hide (ToolsMenuEditorDialog);
}

void UponToolsMenu_AddTool (GtkMenuItem * menuitem, gpointer user_data)
{
    FillToolsEditorListWithEnvironmentOptions ();
    UpdateToolsDialogGreying (TRUE);
    gtk_widget_show (ToolsMenuEditorDialog);
}

void UponToolsMenu_ExecuteTool (GtkMenuItem * menuitem, gpointer user_data)
{
    char *command;
    char *Hour, *Min, *Sec, *Day, *moNth, *Year, *File, *pRoc, *Test, *Port, *openedFile;
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;
    time_t tt;
    struct tm *tm;

    Hour = Min = Sec = Day = moNth = Year = File = pRoc = Test = Port = openedFile = "";

    time (&tt);
    tm = localtime (&tt);
    Hour = g_strdup_printf ("%s%d", (tm->tm_hour / 10) ? "" : "0", tm->tm_hour);
    Min = g_strdup_printf ("%s%d", (tm->tm_min / 10) ? "" : "0", tm->tm_min);
    Sec = g_strdup_printf ("%s%d", (tm->tm_sec / 10) ? "" : "0", tm->tm_sec);
    Day = g_strdup_printf ("%s%d", (tm->tm_mday / 10) ? "" : "0", tm->tm_mday);
    moNth = g_strdup_printf ("%s%d", ((tm->tm_mon + 1) / 10) ? "" : "0", tm->tm_mon + 1);
    Year = g_strdup_printf ("%d", tm->tm_year + 1900);
    openedFile = FileManager_GetCurrentFileName ();

    if (entry)
        switch (entry->nature)
        {
        case BalsaProjectEntry_TestComponent:
            {
                PtrBalsaTestComponent testComponent = BALSA_TESTCOMPONENT (entry->data);

                Port = testComponent->portNames->data;
            }
            {
                GtkCTree *tree = GTK_CTREE (gtk_object_get_data (MainWindowObject,
                    "TreeFilesView"));
                GtkCTreeNode *treeNode = GTK_CTREE_NODE (gtk_object_get_data (GTK_OBJECT (tree),
                    "SelectedNode"));
                GtkCTreeNode *parentNode = GTK_CTREE_ROW (treeNode)->parent;

                entry = BALSA_PROJECT_ENTRY (gtk_ctree_node_get_row_data (tree, parentNode));
            }
            // continue on next case.
        case BalsaProjectEntry_Test:
            {
                PtrBalsaTest test = BALSA_TEST (entry->data);

                Test = test->name;
                pRoc = test->procName;
            }
            // continue on next case.
        case BalsaProjectEntry_File:
            {
                PtrBalsaFile file = BALSA_FILE (entry->data);

                File = file->name;
            }
            break;
        case BalsaProjectEntry_Procedure:
            {
                PtrBalsaProcedure proc = BALSA_PROCEDURE (entry->data);

                pRoc = proc->associated_filename;
                File = proc->name;
            }
            break;
        default:
            break;
        }

    if (File)
        File = ConvertToPathedBalsaFilename (File);

    //replace...
    command = g_strdup (user_data);
    command = str_replace_and_free (command, "%hour", Hour);
    command = str_replace_and_free (command, "%h", Hour);
    command = str_replace_and_free (command, "%min", Min);
    command = str_replace_and_free (command, "%m", Min);
    command = str_replace_and_free (command, "%sec", Sec);
    command = str_replace_and_free (command, "%s", Sec);
    command = str_replace_and_free (command, "%day", Day);
    command = str_replace_and_free (command, "%d", Day);
    command = str_replace_and_free (command, "%month", moNth);
    command = str_replace_and_free (command, "%n", moNth);
    command = str_replace_and_free (command, "%year", Year);
    command = str_replace_and_free (command, "%y", Year);
    command = str_replace_and_free (command, "%file", File);
    command = str_replace_and_free (command, "%f", File);
    command = str_replace_and_free (command, "%proc", pRoc);
    command = str_replace_and_free (command, "%r", pRoc);
    command = str_replace_and_free (command, "%test", Test);
    command = str_replace_and_free (command, "%t", Test);
    command = str_replace_and_free (command, "%port", Port);
    command = str_replace_and_free (command, "%p", Port);
    command = str_replace_and_free (command, "%openedFile", openedFile);
    command = str_replace_and_free (command, "%F", openedFile);

    free (Hour);
    free (Min);
    free (Sec);
    free (Day);
    free (moNth);
    free (Year);
    free (openedFile);
    free (File);

    //  printf("Execution of <%s> => <%s>\n", (char*)user_data, command);
    {
        GList *commandList = g_list_append (NULL, command);

        ExecutionManager_RunCommandList (commandList);
    }
}

void UpdateToolsDialogGreying (gboolean greying)
{
    GtkWidget *toolNameEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolNameEntry"));
    GtkWidget *toolCommandEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolCommandEntry"));

    gtk_widget_set_sensitive (toolNameEntry, !greying);
    gtk_widget_set_sensitive (toolCommandEntry, !greying);
}

void UponToolsList_selectRow (GtkCList * toolsList, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    GtkWidget *toolNameEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolNameEntry"));
    GtkWidget *toolCommandEntry = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolCommandEntry"));

    char *toolName, *toolCommand;

    gtk_object_set_data (GTK_OBJECT (toolsList), "SelectedRow", GINT_TO_POINTER (row));

    gtk_clist_get_text (toolsList, row, 0, &toolName);
    gtk_clist_get_text (toolsList, row, 1, &toolCommand);

    gtk_entry_set_text (GTK_ENTRY (toolNameEntry), toolName);
    gtk_entry_set_text (GTK_ENTRY (toolCommandEntry), toolCommand);

    UpdateToolsDialogGreying (FALSE);
}

void UponToolsList_unselectRow (GtkCList * toolsList, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UpdateToolsDialogGreying (TRUE);
}

void UponToolNameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    int row = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (toolsList), "SelectedRow"));
    char *text = gtk_entry_get_text (GTK_ENTRY (editable));

    gtk_clist_set_text (toolsList, row, 0, text);
}

void UponToolCommandEntryChanged (GtkEditable * editable, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    int row = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (toolsList), "SelectedRow"));
    char *text = gtk_entry_get_text (GTK_ENTRY (editable));

    gtk_clist_set_text (toolsList, row, 1, text);
}

gboolean UponToolsMenuEditorDialog_delete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    gtk_widget_hide (ToolsMenuEditorDialog);
    return TRUE;
}

void UponToolsDialog_buttonNew (GtkButton * button, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));

    int row = -1;
    GtkCellType cellType;

    do
    {
        row++;
        cellType = gtk_clist_get_cell_type (toolsList, row, 0);
    }
    while ((cellType != GTK_CELL_EMPTY) && ((int) cellType != -1) && (row <= 100));

    {
        char *newTool[] = { "<New Tool>", "" };

        gtk_clist_append (toolsList, newTool);
    }
    gtk_clist_select_row (toolsList, row, 0);
}

void UponToolsDialog_buttonCopy (GtkButton * button, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));

    int row = -1;
    GtkCellType cellType;

    do
    {
        row++;
        cellType = gtk_clist_get_cell_type (toolsList, row, 0);
    }
    while ((cellType != GTK_CELL_EMPTY) && ((int) cellType != -1) && (row <= 100));

    {
        int oldRow = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (toolsList), "SelectedRow"));
        char *newTool[2];

        gtk_clist_get_text (toolsList, oldRow, 0, &newTool[0]);
        gtk_clist_get_text (toolsList, oldRow, 1, &newTool[1]);
        newTool[0] = g_strdup (newTool[0]);
        newTool[1] = g_strdup (newTool[1]);

        gtk_clist_append (toolsList, newTool);
    }
    gtk_clist_select_row (toolsList, row, 0);
}

void UponToolsDialog_buttonRemove (GtkButton * button, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    int row = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (toolsList), "SelectedRow"));

    gtk_clist_remove (toolsList, row);
    gtk_clist_select_row (toolsList, row ? row - 1 : 0, 0);
}

void UponToolsDialog_buttonUp (GtkButton * button, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    int row = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (toolsList), "SelectedRow"));

    gtk_clist_row_move (toolsList, row, row - 1);
    gtk_clist_select_row (toolsList, row - 1, 0);
}

void UponToolsDialog_buttonDown (GtkButton * button, gpointer user_data)
{
    GtkCList *toolsList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (ToolsMenuEditorDialog),
        "ToolsDialogList"));
    int row = GPOINTER_TO_INT (gtk_object_get_data (GTK_OBJECT (toolsList), "SelectedRow"));

    gtk_clist_row_move (toolsList, row, row + 1);
    gtk_clist_select_row (toolsList, row + 1, 0);
}

void UpdateToolsMenuGreying (void)
{
    int toolnum;
    GSList *tmp;

    gboolean projectActive = CurrentBalsaProject ? TRUE : FALSE;
    gboolean fileActiveNamed = (FileManager_GetCurrentFileName () != NULL);
    gboolean selectionContainsFile = FALSE;
    gboolean selectionContainsProc = FALSE;
    gboolean selectionContainsTest = FALSE;
    gboolean selectionContainsPort = FALSE;

    if (projectActive && ProjectFileViewSelectedEntry)
    {
        PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

        switch (entry->nature)
        {
        case BalsaProjectEntry_TestComponent:
            selectionContainsPort = TRUE;
        case BalsaProjectEntry_Test:
            selectionContainsTest = TRUE;
        case BalsaProjectEntry_Procedure:
            selectionContainsProc = TRUE;
        case BalsaProjectEntry_File:
            selectionContainsFile = TRUE;
        default:
            break;
        }
    }

    for (tmp = EnvironmentOptions.tools, toolnum = 0; tmp; tmp = tmp->next, toolnum++)
    {
        char *tool = strstr (tmp->data, ",");
        gboolean sensitive = TRUE;

        if (strstr (tool, "%f") || strstr (tool, "%file"))
        {
            if (!selectionContainsFile)
                sensitive = FALSE;
        }

        if (strstr (tool, "%r") || strstr (tool, "%proc"))
        {
            if (!selectionContainsProc)
                sensitive = FALSE;
        }

        if (strstr (tool, "%t") || strstr (tool, "%test"))
        {
            if (!selectionContainsTest)
                sensitive = FALSE;
        }

        if (strstr (tool, "%p") || strstr (tool, "%port"))
        {
            if (!selectionContainsPort)
                sensitive = FALSE;
        }

        if (strstr (tool, "%F") || strstr (tool, "%openedFile"))
        {
            if (!fileActiveNamed)
                sensitive = FALSE;
        }

        {
            GtkWidget *menuItem = gtk_object_get_data (MainWindowObject,
              g_strdup_printf ("ToolsMenu_ToolNum%d",
                toolnum));

            if (menuItem)
                gtk_widget_set_sensitive (menuItem, sensitive);
        }
    }
}
