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

	`addfiledialog.c'

*/

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "widgets.h"
#include "project.h"
#include "utils.h"
#include "file.h"
#include "paths.h"
#include "main.h"
#include "menu_project.h"

extern GdkBitmap *BalsaBlockMask;
extern GdkPixmap *BalsaBlockPixmap;
extern GdkBitmap *DirectoryMask;
extern GdkPixmap *DirectoryPixmap;

GtkWidget *addFileDialog = NULL;
GList *selectionAddList = NULL;
GList *selectionRemoveList = NULL;
gboolean unactiveSelectionHandler = FALSE;

void AddFileDialog_UpdateTree ();
void UponAddFileDialog_OKButton ();

void showAddFileDialog (void)
{
    if (!addFileDialog)
        addFileDialog = create_AddFileDialog ();
    gtk_widget_show (addFileDialog);

    unactiveSelectionHandler = TRUE;
    selectionAddList = NULL;
    selectionRemoveList = NULL;
    AddFileDialog_UpdateTree ();
    unactiveSelectionHandler = FALSE;
}

void AddFileDialog_UpdateTree_AddFile (GtkCTree * TreeWidget, GtkCTreeNode * treeNode, char *filename, char *subdirprefix)
{
    gchar *text[2], *complete_filename;
    GtkCTreeNode *newChildNode;

    text[0] = g_strdup (filename);
    text[1] = "";
    newChildNode =
      gtk_ctree_insert_node (TreeWidget, treeNode, NULL, text, 4, BalsaBlockPixmap, BalsaBlockMask, BalsaBlockPixmap, BalsaBlockMask, TRUE, TRUE);

    complete_filename = g_strdup_printf ("%s%s", subdirprefix ? : "", filename);

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), newChildNode, (gpointer) complete_filename);

    if (IsFileInProject (complete_filename))
    {
        gtk_ctree_select (TreeWidget, newChildNode);
        gtk_ctree_node_set_text (TreeWidget, newChildNode, 1, "Yes");
    }
}

int
AddFileDialog_UpdateTree_AddImportSubDirectory (GtkCTree * TreeWidget, GtkCTreeNode * treeNode, char *directory, char *subdirprefix, char *entryname)
{
    gchar *text[2];
    GtkCTreeNode *newChildNode;
    int nbChilds = 0;

    //    subdirprefix = g_strdup_printf ("%s%s/",subdirprefix?:"",directory);
    if (isImportPath (directory))
        return 0;

    text[0] = g_strdup (entryname);
    text[1] = "";
    newChildNode = gtk_ctree_insert_node (TreeWidget, treeNode,
      NULL, text, 4, DirectoryPixmap, DirectoryMask, DirectoryPixmap, DirectoryMask, FALSE, TRUE);

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), newChildNode, (gpointer) NULL);

    {
        DIR *dir = opendir (directory);
        struct dirent *entry;

        while (dir && (entry = readdir (dir)))
        {
            if (strEndsWith (entry->d_name, ".balsa")) //||
                //      strEndsWith( entry->d_name, ".l" ) )
            {
                AddFileDialog_UpdateTree_AddFile (TreeWidget, newChildNode, entry->d_name, subdirprefix);
                nbChilds++;
            }

            if (entry->d_name[0] != '.')
            {
                struct stat st;
                char *filename = g_strdup_printf ("%s/%s", directory, entry->d_name);

                stat (filename, &st);
                if (S_ISDIR (st.st_mode))
                {
                    char *newsubdirprefix = g_strdup_printf ("%s%s/", subdirprefix ? : "",
                      entry->d_name);

                    nbChilds += AddFileDialog_UpdateTree_AddImportSubDirectory (TreeWidget, newChildNode, filename, newsubdirprefix, entry->d_name);
                }
                free (filename);
            }
        }
        if (dir)
            closedir (dir);
    }

    if (nbChilds == 0)
        gtk_ctree_remove_node (TreeWidget, newChildNode);

    return nbChilds;
}

void AddFileDialog_UpdateTree_AddImportDirectory (GtkCTree * TreeWidget, char *directory)
{
    gchar *text[2];
    GtkCTreeNode *newChildNode;

    text[0] = g_strdup (directory);
    text[1] = "";
    newChildNode = gtk_ctree_insert_node (TreeWidget, NULL, //treeNode,
      NULL, text, 4, NULL, NULL, NULL, NULL, FALSE, TRUE);

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), newChildNode, (gpointer) NULL);

    {
        DIR *dir = opendir (directory);
        struct dirent *entry;

        while (dir && (entry = readdir (dir)))
        {
            if (strEndsWith (entry->d_name, ".balsa")) //||
                //      strEndsWith( entry->d_name, ".l" ) )
                AddFileDialog_UpdateTree_AddFile (TreeWidget, newChildNode, entry->d_name, NULL);

            if (entry->d_name[0] != '.')
            {
                struct stat st;
                char *filename = g_strdup_printf ("%s/%s", directory, entry->d_name);

                stat (filename, &st);
                if (S_ISDIR (st.st_mode))
                {
                    char *subdirprefix = g_strdup_printf ("%s/", entry->d_name);

                    AddFileDialog_UpdateTree_AddImportSubDirectory (TreeWidget, newChildNode, filename, subdirprefix, entry->d_name);
                }
                free (filename);
            }
        }
        if (dir)
            closedir (dir);
    }
}

void AddFileDialog_UpdateTree (void)
{
    GtkCTree *TreeWidget = GTK_CTREE (gtk_object_get_data (GTK_OBJECT (addFileDialog),
        "AddFileDialog_Tree"));
    GtkCList *list = GTK_CLIST (&(TreeWidget->clist));

    gtk_clist_clear (list);

    {
        GList *pathsList = CurrentBalsaProject->importPath;

        for (; pathsList; pathsList = pathsList->next)
        {
            // printf("path=%s\n", pathsList->data);
            AddFileDialog_UpdateTree_AddImportDirectory (TreeWidget, pathsList->data);
        }
    }
}

/*
gboolean AddFileDialog_ImportNewPathCallback(char *directory, GtkWidget *entry)
{
  printf("directory=%s\n",directory);
  AddImportPathToCurrentBalsaProject(directory);
  showAddFileDialog();
  return TRUE; // destroy dialogue
}
*/
void UponAddFileDialog_ImportNewPathButton (GtkButton * button, gpointer user_data)
{
    UponProjectMenu_ProjectOptions (NULL, NULL);
    UponAddFileDialog_OKButton ();
    /*
       GtkWidget *dialogue = gtk_widget_get_toplevel( GTK_WIDGET(button));
       MakeDirectorySelectionDialogue( "Select A File Import Directory",
       NULL,
       (FileSelectionCallback) AddFileDialog_ImportNewPathCallback,
       NULL,
       GTK_WIDGET (dialogue));
     */
}

void UponAddFileDialog_OKButton_AfterRemoveConfirmation (void)
{
    GtkCTree *TreeWidget = GTK_CTREE (gtk_object_get_data (GTK_OBJECT (addFileDialog),
        "AddFileDialog_Tree"));
    GList *selection;

    for (selection = selectionRemoveList; selection; selection = selection->next)
    {
        GtkCTreeNode *node = GTK_CTREE_NODE (selection->data);
        char *filename = (char *) gtk_ctree_node_get_row_data (TreeWidget,
          node);

        //      printf("selection=%s : Remove\n",filename);
        {
            GList *filesList = CurrentBalsaProject->files;
            PtrBalsaFile file;

            for (; filesList; filesList = filesList->next)
            {
                file = BALSA_FILE (filesList->data);
                if (!strcmp (ConvertToBalsaFilename (file->name), filename))
                    break;
            }
            if (filesList != NULL)
            {
                CurrentBalsaProject->files = g_list_remove (CurrentBalsaProject->files, file);
                CurrentBalsaProject->dirty = TRUE;
            }
        }
    }

    for (selection = selectionAddList; selection; selection = selection->next)
    {
        GtkCTreeNode *node = GTK_CTREE_NODE (selection->data);
        char *filename = (char *) gtk_ctree_node_get_row_data (TreeWidget,
          node);

        //  printf("selection=%s : Add\n",filename);
        {
            PtrBalsaFile newFile = NewBalsaFileWithName (filename);

            if (newFile)
                AddFileToProject (CurrentBalsaProject, newFile, filename);
            else
                printfConsole ("Error in adding files: You can only add .balsa files (for the moment)\n");
        }
    }

    gtk_widget_hide (addFileDialog);
    UpdateProjectTrees ();
}

void UponAddFileDialog_OKButton ()
{
    if (selectionRemoveList)
    {
        GtkWidget *dialog;
        const gchar *buttons[] = { "Yes", "Cancel" };
        GtkSignalFunc handlers[] = { UponAddFileDialog_OKButton_AfterRemoveConfirmation,
            NULL
        };

        dialog =
          util_create_dialogue_with_buttons ("Are you sure you want to remove some files from the current project?", 2, buttons, 1, handlers, NULL);
        gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
        gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
        gtk_widget_show (dialog);
    } else
        UponAddFileDialog_OKButton_AfterRemoveConfirmation ();
}

gboolean UponAddFileDialog_CancelButton ()
{
    gtk_widget_hide (addFileDialog);
    return TRUE;
}

gboolean UponAddFileDialog_TreeSelectRow (GtkCTree * ctree, GtkCTreeNode * node, gint column, gpointer user_data)
{
    if (!unactiveSelectionHandler)
    {
        char *text;

        gboolean is_leaf;

        gtk_ctree_get_node_info (ctree, node, NULL, NULL, NULL, NULL, NULL, NULL, &is_leaf, NULL);
        if (!is_leaf)
        {
            gtk_ctree_unselect (ctree, node);
            return TRUE;
        }

        gtk_ctree_node_get_text (ctree, node, 1, &text);
        if (!strcmp (text, ""))
        {
            gtk_ctree_node_set_text (ctree, node, 1, "Add");
            selectionAddList = g_list_append (selectionAddList, node);
        } else
        {
            gtk_ctree_node_set_text (ctree, node, 1, "Yes");
            selectionRemoveList = g_list_remove (selectionRemoveList, node);
        }
    }
    return TRUE;
}

void UponAddFileDialog_TreeUnselectRow (GtkCTree * ctree, GtkCTreeNode * node, gint column, gpointer user_data)
{
    if (!unactiveSelectionHandler)
    {
        char *text;

        gboolean is_leaf;

        gtk_ctree_get_node_info (ctree, node, NULL, NULL, NULL, NULL, NULL, NULL, &is_leaf, NULL);
        if (!is_leaf)
            return;

        gtk_ctree_node_get_text (ctree, node, 1, &text);
        if (!strcmp (text, "Add"))
        {
            gtk_ctree_node_set_text (ctree, node, 1, "");
            selectionAddList = g_list_remove (selectionAddList, node);
        } else
        {
            gtk_ctree_node_set_text (ctree, node, 1, "Remove");
            selectionRemoveList = g_list_append (selectionRemoveList, node);
        }
    }
}

gboolean UponAddFileDialog_keyPressEvent (GdkEventKey * event)
{
    if (event->keyval == GDK_Return)
        UponAddFileDialog_OKButton ();
    else if (event->keyval == GDK_Escape)
        UponAddFileDialog_CancelButton ();
    return TRUE;
}
