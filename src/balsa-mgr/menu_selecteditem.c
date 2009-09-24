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

#include <glib.h>
#include <string.h>

#include "menu_selecteditem.h"
#include "project.h"
#include "file.h"
#include "testopts.h"
#include "utils.h"
#include "widgets.h"
#include "paths.h"
#include "main.h"
#include "file.h"
#include "imploptionsdialog.h"
#include "builtinliboptionsdialog.h"
#include "makefiletable.h"

void UponSelectionMenu_Delete_AfterConfirmation (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

    switch (entry->nature)
    {
    case BalsaProjectEntry_File:
        {
            PtrBalsaFile file = BALSA_FILE (entry->data);

            CurrentBalsaProject->files = g_list_remove (CurrentBalsaProject->files, file);
            CurrentBalsaProject->dirty = TRUE;
            break;
        }
    case BalsaProjectEntry_Directory:
        break;
    case BalsaProjectEntry_Procedure:
        break;
    case BalsaProjectEntry_Test:
        {
            PtrBalsaTest test = BALSA_TEST (entry->data);
            PtrBalsaFile file = test->matchingBalsaFile;

            file->info.balsaFile.testFixtures = g_list_remove (file->info.balsaFile.testFixtures, test);
            CurrentBalsaProject->dirty = TRUE;
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            PtrLardTestFile test = LARDTEST_FILE (entry->data);
            PtrBalsaFile file = test->matchingBalsaFile;

            file->info.balsaFile.lardTestFiles = g_list_remove (file->info.balsaFile.lardTestFiles, test);
            CurrentBalsaProject->dirty = TRUE;
            break;
        }
    case BalsaProjectEntry_Implementation:
        {
            PtrImplementation impl = IMPLEMENTATION (entry->data);
            PtrBalsaFile file = impl->matchingBalsaFile;
            PtrBalsaTest test = impl->matchingBalsaTest;

            if (test)
                test->implementations = g_list_remove (test->implementations, impl);
            else
                file->info.balsaFile.implementations = g_list_remove (file->info.balsaFile.implementations, impl);
            CurrentBalsaProject->dirty = TRUE;
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            PtrBuiltinLib lib = BUILTINLIB (entry->data);
            PtrBalsaFile file = lib->matchingBalsaFile;

            file->info.balsaFile.builtinLib = NULL;
            CurrentBalsaProject->dirty = TRUE;
            break;
        }
    case BalsaProjectEntry_TestComponent:
        break;
    }

    //  CurrentBalsaProject->needToRebuildMakefile = TRUE;
    UpdateProjectTrees ();
}

void UponSelectionMenu_Delete (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

    switch (entry->nature)
    {
    case BalsaProjectEntry_Directory:
        printfConsole ("Cannot delete a Directory!\n");
        return;
    case BalsaProjectEntry_Procedure:
        printfConsole ("Cannot delete a Procedure!\n");
        return;
    case BalsaProjectEntry_TestComponent:
        printfConsole ("Cannot delete a Test component!\n");
        return;

    case BalsaProjectEntry_File:
    case BalsaProjectEntry_Test:
    case BalsaProjectEntry_LardTestFile:
    case BalsaProjectEntry_Implementation:
    case BalsaProjectEntry_BuiltinLib:
        break;
    }

    GtkWidget *dialog;
    const gchar *buttons[] = { "Yes", "No" };
    GtkSignalFunc handlers[] = { UponSelectionMenu_Delete_AfterConfirmation, NULL };

    dialog = util_create_dialogue_with_buttons ("Are you sure you want to delete this item?", 2, buttons, 1, handlers, NULL);
    gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
    gtk_widget_show (dialog);
}

void UponSelectionMenu_Unselect (GtkMenuItem * menuitem, gpointer user_data)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "Tree1");

    gtk_ctree_unselect_recursive (GTK_CTREE (TreeWidget), NULL);
    gtk_object_set_data (GTK_OBJECT (TreeWidget), "SelectedNode", NULL);
}

void CallTestOptionsWindow (char *filename, char *procName, PtrBalsaTest editedTest)
{
    GtkWidget *dialogue = create_TestOptionsDialogue ();
    GtkEntry *nameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestNameEntry"));

    //   GtkCombo *filenameCombo = GTK_COMBO (gtk_object_get_data (GTK_OBJECT (dialogue), "TestFilenameCombo"));
    GtkEntry *filenameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestFilenameEntry"));
    GtkCList *list = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue), "TestPortList"));

    AddToOtherWindows_CurrentProject (dialogue);

    /* Adjust some properties */
    gtk_object_set_data (GTK_OBJECT (dialogue), "SelectedTestComponent", NULL);
    gtk_object_set_data (GTK_OBJECT (dialogue), "EditedTest", editedTest);

    /* Fill the Filenames combo box */
    {
        GList *filenames = NULL;
        GList *file;

        for (file = CurrentBalsaProject->files; file; file = file->next)
        {
            filenames = g_list_append (filenames, ConvertToBalsaFilename (BALSA_FILE (file->data)->name));
        }

        if (!filenames)
            filenames = g_list_append (filenames, g_strdup ("<no files>"));

        disable_filenameEntry = TRUE;
        //        gtk_combo_set_popdown_strings (filenameCombo, filenames);
        disable_filenameEntry = FALSE;
    }

    /* Select the right filename in the combo box => it will call UponTestFilenameEntryChanged(...) */
    if (filename)
    {
        gtk_entry_set_text (GTK_ENTRY (filenameEntry), ConvertToBalsaFilename (filename));
    }
    if (procName)
    {
        GtkEntry *procEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue),
            "TestProcNameEntry"));

        gtk_entry_set_text (procEntry, g_strdup (procName));
    }

    if (!editedTest)
    {                           // find a unused name for the test
        char *test_name = NULL;
        int num = 1;

      retry:
        g_free (test_name);
        test_name = g_strdup_printf ("test%d", num);
        {                       // Check the test name doesn't already exist
            GList *files;

            for (files = CurrentBalsaProject->files; files; files = files->next)
            {
                PtrBalsaFile file = (PtrBalsaFile) files->data;
                GList *tests = file->info.balsaFile.testFixtures;

                for (; tests; tests = tests->next)
                {
                    PtrBalsaTest test = (PtrBalsaTest) tests->data;

                    if (!strcmp (test_name, test->name))
                    {
                        num++;
                        goto retry;
                    }
                }

                tests = file->info.balsaFile.lardTestFiles;
                for (; tests; tests = tests->next)
                {
                    PtrLardTestFile test = (PtrLardTestFile) tests->data;

                    if (!strcmp (test_name, test->name))
                    {
                        num++;
                        goto retry;
                    }
                }
            }
        }

        gtk_entry_set_text (GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestNameEntry")), test_name);

        // Create new components
        //      CreateNewComponents (list, filename, procName);
    } else
    {
        TestOpts_clearList (dialogue);
        AddTestComponentsToTestComponentList (list, editedTest->testComponents);

        gtk_entry_set_text (GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestNameEntry")), editedTest->name);

        TestOpts_FillDefinesList (dialogue, editedTest->commandLineOptions);
    }

    TestOpts_UpdateForgottenPorts (list);
    gtk_clist_select_row (GTK_CLIST (list), 0, 0);

    /* Make name selected so we can directly overtype it */
    gtk_entry_select_region (nameEntry, 0, -1);
    gtk_widget_grab_focus (GTK_WIDGET (nameEntry));

    gtk_widget_show (dialogue);
}

void UponSelectionMenu_AddTest (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

    switch (entry->nature)
    {                           /* different types of editing */
    case BalsaProjectEntry_File:
        {
            PtrBalsaFile file = BALSA_FILE (entry->data);

            CallTestOptionsWindow (file->name, NULL, NULL);
            break;
        }
    case BalsaProjectEntry_Procedure:
        {
            PtrBalsaProcedure proc = BALSA_PROCEDURE (entry->data);

            CallTestOptionsWindow (proc->associated_filename, proc->name, NULL);
            break;
        }
        // TODO
        break;
    case BalsaProjectEntry_TestComponent: /* edit the whole test fixture */
        {
            //GtkCTreeNode *treeNode = BALSA_PROCEDURE(entry->data)->treeNode;
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
            PtrBalsaFile file = test->matchingBalsaFile;

            CallTestOptionsWindow (file->name, test->procName, NULL);
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            PtrLardTestFile file = LARDTEST_FILE (entry->data);

            CallTestOptionsWindow (file->matchingBalsaFile->name, NULL, NULL);
            break;
        }
    case BalsaProjectEntry_Implementation:
        {
            PtrImplementation file = IMPLEMENTATION (entry->data);

            CallTestOptionsWindow (file->matchingBalsaFile->name, NULL, NULL);
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            PtrBuiltinLib file = BUILTINLIB (entry->data);

            CallTestOptionsWindow (file->matchingBalsaFile->name, NULL, NULL);
            break;
        }
    case BalsaProjectEntry_Directory:
    default:
        printfConsole ("No Edition instructions for this type.\n");
    }
}

void UponSelectionMenu_AddLardTestFile (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;
    PtrBalsaFile file = NULL;

    switch (entry->nature)
    {                           /* different types of editing */
    case BalsaProjectEntry_File:
        {
            file = BALSA_FILE (entry->data);
            break;
        }
    case BalsaProjectEntry_Procedure:
        {
            PtrBalsaProcedure proc = BALSA_PROCEDURE (entry->data);
            char *filename = proc->associated_filename;

            GList *files;

            for (files = CurrentBalsaProject->files; files; files = files->next)
            {
                file = (PtrBalsaFile) files->data;
                if (!strcmp (filename, file->name))
                    break;
            }

            break;
        }
        break;
    case BalsaProjectEntry_TestComponent: // edit the whole test fixture
        {
            //GtkCTreeNode *treeNode = BALSA_PROCEDURE(entry->data)->treeNode;
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

            file = test->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            file = LARDTEST_FILE (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_Implementation:
        {
            file = IMPLEMENTATION (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            file = BUILTINLIB (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_Directory:
    default:
        printfConsole ("No Edition instructions for this type.\n");
    }

    if (file)
        CallLardTestFileSelectionWindow (file, NULL);
}

void UponSelectionMenu_AddImplementation (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;
    PtrBalsaFile file = NULL;
    PtrBalsaTest test = NULL;

    switch (entry->nature)
    {                           /* different types of editing */
    case BalsaProjectEntry_File:
        {
            file = BALSA_FILE (entry->data);
            break;
        }
    case BalsaProjectEntry_Procedure:
        {
            PtrBalsaProcedure proc = BALSA_PROCEDURE (entry->data);
            char *filename = proc->associated_filename;

            GList *files;

            for (files = CurrentBalsaProject->files; files; files = files->next)
            {
                file = (PtrBalsaFile) files->data;
                if (!strcmp (filename, file->name))
                    break;
            }

            break;
        }
        break;
    case BalsaProjectEntry_TestComponent: // edit the whole test fixture
        {
            //GtkCTreeNode *treeNode = BALSA_PROCEDURE(entry->data)->treeNode;
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
            PtrBalsaTest test2 = BALSA_TEST (entry->data);

            file = test2->matchingBalsaFile;
            test = test2;
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            file = LARDTEST_FILE (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_Implementation:
        {
            file = IMPLEMENTATION (entry->data)->matchingBalsaFile;
            test = IMPLEMENTATION (entry->data)->matchingBalsaTest;
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            file = BUILTINLIB (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_Directory:
    default:
        printfConsole ("No Edition instructions for this type.\n");
    }

    if (file)
        CallImplementationSelectionWindow (file, test, NULL);
}

void UponSelectionMenu_AddBuiltinLib (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;
    PtrBalsaFile file = NULL;

    switch (entry->nature)
    {                           /* different types of editing */
    case BalsaProjectEntry_File:
        {
            file = BALSA_FILE (entry->data);
            break;
        }
    case BalsaProjectEntry_Procedure:
        {
            PtrBalsaProcedure proc = BALSA_PROCEDURE (entry->data);
            char *filename = proc->associated_filename;

            GList *files;

            for (files = CurrentBalsaProject->files; files; files = files->next)
            {
                file = (PtrBalsaFile) files->data;
                if (!strcmp (filename, file->name))
                    break;
            }

            break;
        }
        break;
    case BalsaProjectEntry_TestComponent: // edit the whole test fixture
        {
            //GtkCTreeNode *treeNode = BALSA_PROCEDURE(entry->data)->treeNode;
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

            file = test->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            file = LARDTEST_FILE (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            file = BUILTINLIB (entry->data)->matchingBalsaFile;
            break;
        }
    case BalsaProjectEntry_Directory:
    default:
        printfConsole ("No Edition instructions for this type.\n");
    }

    if (file)
        CallBuiltinLibSelectionWindow (file, NULL);
}

void UponSelectionMenu_Edit (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

    switch (entry->nature)
    {                           /* different types of editing */
    case BalsaProjectEntry_File:
    case BalsaProjectEntry_Procedure:
        UponProjectToolbar_LaunchEditor (NULL, NULL);
        break;
    case BalsaProjectEntry_Test:
        {
            PtrBalsaTest test = BALSA_TEST (entry->data);
            PtrBalsaFile file = test->matchingBalsaFile;

            CallTestOptionsWindow (file->name, test->procName, test);
            break;
        }
    case BalsaProjectEntry_TestComponent: /* edit the whole test fixture */
        {
            //GtkCTreeNode *treeNode = BALSA_PROCEDURE(entry->data)->treeNode;
            GtkCTree *tree = GTK_CTREE (gtk_object_get_data (MainWindowObject,
                "TreeFilesView"));
            GtkCTreeNode *treeNode = GTK_CTREE_NODE (gtk_object_get_data (GTK_OBJECT (tree),
                "SelectedNode"));
            GtkCTreeNode *parentNode = GTK_CTREE_ROW (treeNode)->parent;

            entry = BALSA_PROJECT_ENTRY (gtk_ctree_node_get_row_data (tree, parentNode));
            {
                PtrBalsaTest test = BALSA_TEST (entry->data);
                PtrBalsaFile file = test->matchingBalsaFile;

                CallTestOptionsWindow (file->name, test->procName, test);
            }
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            PtrLardTestFile lardtestfile = LARDTEST_FILE (entry->data);

            CallLardTestFileSelectionWindow (lardtestfile->matchingBalsaFile, lardtestfile);
            break;
        }
    case BalsaProjectEntry_Implementation:
        {
            PtrImplementation implementation = IMPLEMENTATION (entry->data);

            CallImplementationSelectionWindow (implementation->matchingBalsaFile, implementation->matchingBalsaTest, implementation);
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            PtrBuiltinLib builtinLib = BUILTINLIB (entry->data);

            CallBuiltinLibSelectionWindow (builtinLib->matchingBalsaFile, builtinLib);
            break;
        }
    case BalsaProjectEntry_Directory:
    default:
        printfConsole ("No Edition instructions for this type.\n");
    }
}

void UponSelectionMenu_Make (GtkMenuItem * menuitem, gpointer user_data)
{
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

    switch (entry->nature)
    {                           /* different types of editing */
    case BalsaProjectEntry_File:
        {
            PtrBalsaFile file = BALSA_FILE (entry->data);

            char *makeName = g_strdup_printf ("%s.breeze", file->name);

            MakefileTable_MakeCallback (NULL, makeName);
            break;
        }
    case BalsaProjectEntry_Procedure:
        {
            PtrBalsaProcedure proc = BALSA_PROCEDURE (entry->data);

            char *makeName = g_strdup_printf ("%s.breeze", proc->associated_filename);

            MakefileTable_MakeCallback (NULL, makeName);
            break;
        }
    case BalsaProjectEntry_Test:
        {
            PtrBalsaTest test = BALSA_TEST (entry->data);
            char *makeName = g_strdup_printf ("sim-win-%s", test->name);

            MakefileTable_MakeCallback (NULL, makeName);
            break;
        }
    case BalsaProjectEntry_TestComponent: /* edit the whole test fixture */
        {
            //GtkCTreeNode *treeNode = BALSA_PROCEDURE(entry->data)->treeNode;
            GtkCTree *tree = GTK_CTREE (gtk_object_get_data (MainWindowObject,
                "TreeFilesView"));
            GtkCTreeNode *treeNode = GTK_CTREE_NODE (gtk_object_get_data (GTK_OBJECT (tree),
                "SelectedNode"));
            GtkCTreeNode *parentNode = GTK_CTREE_ROW (treeNode)->parent;

            entry = BALSA_PROJECT_ENTRY (gtk_ctree_node_get_row_data (tree, parentNode));
            {
                PtrBalsaTest test = BALSA_TEST (entry->data);

                char *makeName = g_strdup_printf ("sim-win-%s", test->name);

                MakefileTable_MakeCallback (NULL, makeName);
            }
            break;
        }
    case BalsaProjectEntry_LardTestFile:
        {
            PtrLardTestFile lardtestfile = LARDTEST_FILE (entry->data);

            char *makeName = g_strdup_printf ("sim-win-%s", lardtestfile->name);

            MakefileTable_MakeCallback (NULL, makeName);
            break;
        }
    case BalsaProjectEntry_Implementation:
        {
            PtrImplementation impl = IMPLEMENTATION (entry->data);

            char *makeName = g_strdup_printf ("impl-%s", impl->name);

            MakefileTable_MakeCallback (NULL, makeName);
            break;
        }
    case BalsaProjectEntry_BuiltinLib:
        {
            PtrBuiltinLib lib = BUILTINLIB (entry->data);

            char *makeName = g_strdup_printf ("lib-%s", lib->name);

            MakefileTable_MakeCallback (NULL, makeName);
            break;
        }
    case BalsaProjectEntry_Directory:
    default:
        printfConsole ("No Make instructions for this type.\n");
    }
}
