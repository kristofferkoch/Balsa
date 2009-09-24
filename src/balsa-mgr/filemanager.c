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

	`filemanager.c'

*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <sys/stat.h>
#include <string.h>

#include "filemanager.h"

#include "editor.h"
#include "main.h"
#include "utils.h"
#include "paths.h"

struct fileStruct
{
    char *filename;
    char *displayedName;
    int notebookPage;
    GtkWidget *TextWidget, *LabelWidget;
    gboolean dirty;
    gboolean externallyModified;
    struct stat lastStat;
};

GtkWidget *FilesNotebook;
GSList *FileManager_OpenedFiles = NULL;
GdkFont *EditorFont;

void UpdateLabel (struct fileStruct *fileStruct)
{
    char *label, *newlabel;

    gtk_label_get (GTK_LABEL (fileStruct->LabelWidget), &label);

    newlabel = g_strdup_printf ("%s%s%s", fileStruct->displayedName, fileStruct->dirty ? "*" : "", fileStruct->externallyModified ? "!" : "");

    gtk_label_set_text (GTK_LABEL (fileStruct->LabelWidget), newlabel);
    //g_free(label);
}

struct fileStruct *GetFileStruct (char *filename)
{
    GSList *tmp;

    if (!filename || !filename[0])
        return NULL;
    else
        filename = ExpandFilename /*ImportPaths_GetCompletePathedFilename */ (filename,
          TRUE, NULL);
    // filename = ImportPaths_ConvertFileToRelative (filename);

    for (tmp = FileManager_OpenedFiles; tmp; tmp = tmp->next)
        if (((struct fileStruct *) tmp->data)->filename && !strcmp (((struct fileStruct *) tmp->data)->filename, filename))
            break;

    if (!tmp)
        return NULL;
    else
        return tmp->data;
}

struct fileStruct *GetFileStructFromPage (int numPage)
{
    GSList *tmp;

    for (tmp = FileManager_OpenedFiles; tmp; tmp = tmp->next)
        if (((struct fileStruct *) tmp->data)->notebookPage == numPage)
            break;

    if (!tmp)
        return NULL;
    else
        return tmp->data;
}

struct fileStruct *getCurrentFileStruct (void)
{
    int page = gtk_notebook_get_current_page (GTK_NOTEBOOK (FilesNotebook));

    return GetFileStructFromPage (page);
}

void SetDirtyFlag (struct fileStruct *fileStruct, gboolean dirty)
{
    if (dirty == FALSE)
    {
        stat (fileStruct->filename, &fileStruct->lastStat);
        if (fileStruct->externallyModified)
        {
            fileStruct->externallyModified = FALSE;
        }
    }

    if (fileStruct->dirty != dirty)
    {
        fileStruct->dirty = dirty;
        if (CurrentBalsaProject)
        {
            if (dirty)
                CurrentBalsaProject->nb_dirty_files++;
            else
                CurrentBalsaProject->nb_dirty_files--;

            UpdateProjectName ();
        }
    }

    UpdateLabel (fileStruct);
}

void FileManager_SetDirtyFlagToCurrentFile (gboolean dirty)
{
    SetDirtyFlag (getCurrentFileStruct (), dirty);
}

void BringToTop (struct fileStruct *fileStruct)
{
    gtk_notebook_set_page (GTK_NOTEBOOK (FilesNotebook), fileStruct->notebookPage);
}

void RefreshFile (struct fileStruct *fileStruct)
{
    if (!fileStruct->filename)
        return;

    if (Editor_LoadFile (fileStruct->TextWidget, fileStruct->filename))
        SetDirtyFlag (fileStruct, FALSE);
}

static void ConfirmReload_OnYes (GtkWidget * button, GtkSignalFunc fileStruct)
{
    RefreshFile ((struct fileStruct *) fileStruct);
}

void CheckIfFileWasExternallyModified (struct fileStruct *fileStruct)
{
    if (!fileStruct || !fileStruct->lastStat.st_mtime)
        return;
    else
    {
        struct stat newStat;

        if (stat (fileStruct->filename, &newStat) == 0)
            if (newStat.st_mtime != fileStruct->lastStat.st_mtime)
            {                   // file changed
                fileStruct->lastStat = newStat;
                fileStruct->externallyModified = TRUE;
                UpdateLabel (fileStruct);

                {
                    GtkWidget *dialog;
                    const gchar *buttons[] = { "Yes", "No" };
                    GtkSignalFunc handlers[] = { ConfirmReload_OnYes, NULL };

                    dialog =
                      util_create_dialogue_with_buttons ("File Externally modified\nDo you want to reload it?", 2, buttons, 2, handlers, fileStruct);
                    gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
                    //       gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
                    gtk_widget_show (dialog);
                }
            }

    }
}

void FileManager_SignalTextActivate (GtkWidget * widget, gpointer data)
{
    struct fileStruct *fileStruct = gtk_object_get_data (GTK_OBJECT (widget), "fileStruct");

    if (fileStruct)
        CheckIfFileWasExternallyModified (fileStruct);
    else
        printfConsole ("Problem in SignalTextActivate\n");
}

void AddNewFile (char *filename)
{
    struct fileStruct *newStruct;

    if (filename && filename[0])
        filename = ExpandFilename /*ImportPaths_GetCompletePathedFilename */ (filename,
          TRUE, NULL);
    //   filename = ImportPaths_ConvertFileToRelative (filename);

    /* Create the new structure */
    newStruct = (struct fileStruct *) g_malloc (sizeof (struct fileStruct));
    newStruct->filename = g_strdup (filename);
    stat (newStruct->filename, &newStruct->lastStat);
    newStruct->externallyModified = FALSE;

    if (filename)
        newStruct->displayedName = ImportPaths_ConvertFileToRelative (filename);
    else
        newStruct->displayedName = g_strdup ("<noname>");

    FileManager_OpenedFiles = g_slist_append (FileManager_OpenedFiles, newStruct);

    /* Create the new notebook pane */
    {
        GtkTooltips *tooltips = gtk_tooltips_new ();
        GtkWidget *newEditor, *newLabel;

        newEditor = create_new_editor ();
        newLabel = gtk_label_new (g_strdup (newStruct->displayedName));
        gtk_tooltips_set_tip (tooltips, GTK_WIDGET (newLabel), "toto", "titi");

        gtk_widget_show (newEditor);
        gtk_widget_show (newLabel);
        gtk_notebook_append_page (GTK_NOTEBOOK (FilesNotebook), newEditor, newLabel);
        gtk_notebook_set_page (GTK_NOTEBOOK (FilesNotebook), -1);

        newStruct->notebookPage = gtk_notebook_get_current_page (GTK_NOTEBOOK (FilesNotebook));
        newStruct->TextWidget = newEditor;
        newStruct->LabelWidget = newLabel;
        newStruct->dirty = FALSE;
        gtk_object_set_data (GTK_OBJECT (newEditor), "fileStruct", newStruct);

        RefreshFile (newStruct);
    }

    /* Show the notebook, in case where we create the first file in it */
    gtk_widget_show (FilesNotebook);
}

gboolean UponFilesNotebook_ButtonPressEvent (GtkWidget * widget, GdkEventButton * event, GtkWidget * user_data)
{
    if (event->button == 3)     //Right button pressed
    {
        GtkWidget *menu = (GtkWidget *) gtk_object_get_data (MainWindowObject,
          "FileMenu_menu");

        gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, 0);
    }
    return FALSE;
}

static void ConfirmSaveCurrentFile_OnYes (GtkWidget * button, GtkSignalFunc func)
{
    UponFileMenu_Save (NULL, NULL);
    func (NULL, NULL);
}
static void ConfirmSaveCurrentFile_OnNo (GtkWidget * button, GtkSignalFunc func)
{
    func (NULL, NULL);
}

void ConfirmSaveCurrentFileAndExecute (GtkSignalFunc func)
{
    /* check if the current file needs to be saved */
    struct fileStruct *fileStruct = getCurrentFileStruct ();

    if (!fileStruct)
        return;

    if (fileStruct->dirty)
    {
        GtkWidget *dialog;
        const gchar *buttons[] = { "Yes", "No", "Cancel" };
        GtkSignalFunc handlers[] = { ConfirmSaveCurrentFile_OnYes,
            ConfirmSaveCurrentFile_OnNo, NULL
        };

        dialog = util_create_dialogue_with_buttons ("Do you want to save the current file?", 3, buttons, 3, handlers, func);
        gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
        //      gtk_window_set_transient_for (GTK_WINDOW (dialog),
        //       GTK_WINDOW (project_window->window));
        gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
        gtk_widget_show (dialog);
    } else
        func (NULL, NULL);
}

void ChangeLineColorToRed (int position, int length)
{
    static GtkWidget *last_red_text_widget = NULL;
    static int last_red_position = 0;
    static int last_red_length = 0;

    if (length < 0)
        length = 0;

    if (last_red_length && GTK_IS_TEXT (last_red_text_widget))
    {
        GtkWidget *TextZoneWidget = last_red_text_widget;
        char *buf = gtk_editable_get_chars (GTK_EDITABLE (TextZoneWidget),
          last_red_position,
          last_red_position + last_red_length);

        gtk_text_freeze (GTK_TEXT (TextZoneWidget));
        gtk_text_set_point (GTK_TEXT (TextZoneWidget), last_red_position);
        gtk_text_forward_delete (GTK_TEXT (TextZoneWidget), last_red_length);
        gtk_text_insert (GTK_TEXT (TextZoneWidget), EditorFont, NULL, NULL, buf, last_red_length);
        gtk_text_thaw (GTK_TEXT (TextZoneWidget));
    }
    // Try and set the scrollbar to an approximate right value
    {
        return;                 //SCI_TODO
        GtkWidget *TextZoneWidget = getCurrentFileStruct ()->TextWidget;

        //  int length = gtk_text_get_length (GTK_TEXT (TextZoneWidget));
        GtkAdjustment *adjustment = GTK_TEXT (TextZoneWidget)->vadj;
        int nb_lines_before = 0;
        int nb_lines_after = 0;
        char *buf = gtk_editable_get_chars (GTK_EDITABLE (TextZoneWidget), 0, -1);
        char *ptr = buf;

        while (1)
        {
            ptr = strchr (ptr + 1, '\n');
            if (!ptr)
                break;
            if (ptr - buf < position)
                nb_lines_before++;
            else
                nb_lines_after++;
        };

        //  float value = adjustment->lower + (adjustment->upper - adjustment->lower) * position / length;
        float value =
          adjustment->lower + (adjustment->upper - adjustment->lower -
          adjustment->page_size) * nb_lines_before / (nb_lines_before + nb_lines_after) + adjustment->page_size;
        gtk_adjustment_set_value (adjustment, value);
        g_free (buf);
    }

    {
        return;                 //SCI_TODO
        GtkWidget *TextZoneWidget = getCurrentFileStruct ()->TextWidget;
        char *buf = gtk_editable_get_chars (GTK_EDITABLE (TextZoneWidget),
          position, position + length);

        gtk_text_set_point (GTK_TEXT (TextZoneWidget), position);
        gtk_text_forward_delete (GTK_TEXT (TextZoneWidget), length);
        gtk_text_freeze (GTK_TEXT (TextZoneWidget));
        gtk_text_insert (GTK_TEXT (TextZoneWidget), EditorFont, &Red, NULL, buf, length);
        gtk_text_thaw (GTK_TEXT (TextZoneWidget));

        last_red_text_widget = TextZoneWidget;
        last_red_position = position;
        last_red_length = length;
    }
}

void SaveFileCorrespondingToFileStruct (struct fileStruct *fileStruct)
{
    GtkWidget *editor = fileStruct->TextWidget;

    if (!fileStruct->filename)
    {
        FileManager_SaveFileAs ();
        return;
    }

    if (Editor_SaveBuffer (editor, fileStruct->filename))
        SetDirtyFlag (fileStruct, FALSE);
}

/********************/
/* Public Functions */
/********************/

void FileManager_Initialize (void)
{
    //    EditorFont = gdk_font_load ("-*-lucidatypewriter-medium-*-*-*-*-120-*-*-*-*-*-*");
    EditorFont = gdk_fontset_load ("-*-fixed-*-*-*-*-*-120-*-*-*-*-*-*");
    FilesNotebook = (GtkWidget *) gtk_object_get_data (MainWindowObject, "FilesNotebook");
    gtk_notebook_remove_page (GTK_NOTEBOOK (FilesNotebook), 0);
}

void FileManager_NewFile (void)
{
    AddNewFile (NULL);
    UpdateMainWindowGreying ();
}

gboolean FileManager_OpenFile_Callback (char *filename, gpointer user_data)
{
    FileManager_OpenFile (filename);
    return TRUE;
}

void FileManager_OpenFile (char *filename)
{
    FileManager_OpenFileAtLineAndColumn (filename, 0, 0);
}

void FileManager_OpenFileAtLine (char *filename, int line)
{
    FileManager_OpenFileAtLineAndColumn (filename, line, 0);
}

void FileManager_OpenFileAtLineAndColumn (char *filename, int line, int column)
{
    if (filename && filename[0])
    {
        filename = ExpandFilename /*ImportPaths_GetCompletePathedFilename */ (filename,
          TRUE, NULL);
        //        filename = ImportPaths_ConvertFileToRelative (filename);

        if (!doesFileExist (filename))
            return;

        struct fileStruct *fileStruct = GetFileStruct (filename);

        if (fileStruct)
            BringToTop (fileStruct);
        else
            AddNewFile (filename);

        if (line)
        {
            Editor_GotoAndHighlight_LineColumn (getCurrentFileStruct ()->TextWidget, line, column);
        }

        UpdateMainWindowGreying ();
    } else
    {
        MakeFileSelectionDialogue ("Open File... (Balsa:Try Up/Down arrows)", FileManager_OpenFile_Callback, NULL, NULL);
    }
}

void FileManager_ReOpenFile_afterSaveConfirmation (void)
{
    struct fileStruct *fileStruct = getCurrentFileStruct ();

    RefreshFile (fileStruct);
}

void FileManager_ReOpenFile (void)
{
    ConfirmSaveCurrentFileAndExecute (FileManager_ReOpenFile_afterSaveConfirmation);
}

void FileManager_SaveFile (void)
{
    SaveFileCorrespondingToFileStruct (getCurrentFileStruct ());
}

gboolean FileManager_SaveAs_Callback (char *filename, gpointer user_data)
{
    struct fileStruct *fileStruct = getCurrentFileStruct ();

    if (!filename || !filename[0])
        return TRUE;
    else
        filename = ExpandFilename /*ImportPaths_GetCompletePathedFilename */ (filename,
          TRUE, NULL);

    fileStruct->filename = g_strdup (filename);

    if (filename)
        fileStruct->displayedName = ImportPaths_ConvertFileToRelative (filename);
    else
        fileStruct->displayedName = g_strdup ("<noname>");

    UpdateMainWindowGreying ();
    FileManager_SaveFile ();

    return TRUE;
}

void FileManager_SaveFileAs (void)
{
    MakeFileSelectionDialogue ("Save As...", FileManager_SaveAs_Callback, NULL, NULL);
}

void FileManager_CloseFile_afterSaveConfirmation (void)
{
    int numPage = gtk_notebook_get_current_page (GTK_NOTEBOOK (FilesNotebook));

    //    ChangeLineColorToRed (0, 0);

    /* Remove the page from the notebook */
    gtk_notebook_remove_page (GTK_NOTEBOOK (FilesNotebook), numPage);

    /* Remove the structure correspoonding to the page in the GSList */
    {
        GSList *tmp, *toBeRemoved;

        for (tmp = FileManager_OpenedFiles; tmp; tmp = tmp->next)
            if (((struct fileStruct *) tmp->data)->notebookPage == numPage)
                toBeRemoved = tmp;
            else if (((struct fileStruct *) tmp->data)->notebookPage > numPage)
                ((struct fileStruct *) tmp->data)->notebookPage--;

        FileManager_OpenedFiles = g_slist_remove_link (FileManager_OpenedFiles, toBeRemoved);
    }
    UpdateMainWindowGreying ();
}

void FileManager_CloseFile (void)
{
    ConfirmSaveCurrentFileAndExecute (FileManager_CloseFile_afterSaveConfirmation);
}

void FileManager_CloseAllFiles_afterSaveConfirmation (void)
{
    FileManager_CloseFile_afterSaveConfirmation ();
    FileManager_CloseAllFiles ();
}

void FileManager_CloseAllFiles (void)
{
    ConfirmSaveCurrentFileAndExecute (FileManager_CloseAllFiles_afterSaveConfirmation);
}

char *FileManager_GetCurrentFileName (void)
{
    struct fileStruct *fileStruct = getCurrentFileStruct ();

    if (fileStruct)
        return g_strdup (fileStruct->filename);
    else
        return NULL;
}

char *FileManager_GetCurrentDisplayedName (void)
{
    struct fileStruct *fileStruct = getCurrentFileStruct ();

    if (fileStruct)
        return g_strdup (fileStruct->displayedName);
    else
        return NULL;
}

int FileManager_GetCurrentLineNumber (void)
{
    //TODO
    /*
       struct fileStruct *fileStruct = getCurrentFileStruct();
       if (fileStruct)
       {
       GtkWidget *TextZoneWidget = fileStruct->TextWidget;
       int position = gtk_text_get_point(GTK_TEXT(TextZoneWidget));
       }
     */
    return -1;
}

void FileManager_SaveFileIfOpenedAndIfNeeded (char *filename)
{
    if (!filename || !filename[0])
        return;
    else
        filename = ExpandFilename /*ImportPaths_GetCompletePathedFilename */ (filename,
          TRUE, NULL);

    {
        struct fileStruct *fileStruct = GetFileStruct (filename);

        if (fileStruct && fileStruct->dirty)
            SaveFileCorrespondingToFileStruct (fileStruct);
    }
}

void FileManager_SaveAllNamedFiles (void)
{
    int numPage;

    for (numPage = 0;; numPage++)
    {
        struct fileStruct *fileStruct = GetFileStructFromPage (numPage);

        if (!fileStruct)
            return;

        if (fileStruct && fileStruct->dirty && fileStruct->filename)
            SaveFileCorrespondingToFileStruct (fileStruct);
    }
}

void FileManager_RefreshAllDisplayedNames (void)
{
    static char *lastPath = NULL;
    int numPage;

    if (!CurrentBalsaProject || !CurrentBalsaProject->directory)
        return;

    if (!lastPath || strcmp (lastPath, CurrentBalsaProject->directory))
    {
        if (lastPath)
            g_free (lastPath);
        lastPath = g_strdup (CurrentBalsaProject->directory);
    }

    for (numPage = 0;; numPage++)
    {
        struct fileStruct *fileStruct = GetFileStructFromPage (numPage);

        if (!fileStruct)
            return;

        if (fileStruct && fileStruct->filename)
        {
            fileStruct->displayedName = ImportPaths_ConvertFileToRelative (fileStruct->filename);
            UpdateLabel (fileStruct);
        }
    }
}
