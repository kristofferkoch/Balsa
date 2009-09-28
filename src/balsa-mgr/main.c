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

	`main.c'
	Signal handlers and main function

*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include <breeze/libbreeze.h>

#include "workspace.h"
#include "widgets.h"
#include "main.h"
#include "project.h"
#include "miscgtk.h"
#include "options.h"
#include "commands.h"
#include "paths.h"
#include "file.h"
#include "testopts.h"
#include "projopts.h"
#include "utils.h"
#include "filemanager.h"
#include "executionmanager.h"
#include "makefiletable.h"
#include "toolseditor.h"
#include "addfiledialog.h"
#include "icons.h"
#include "menu_view.h"
#include "consolewindow.h"
#include "signals.h"
#include "technology.h"

struct environmentOptions EnvironmentOptions;

//GtkWidget *OptionsDialogue;
GtkWidget *ToolsMenuEditorDialog;

//GtkWidget *ProjectOptionsDialogue = NULL;
//GtkWidget *ProjectOptionsDialogue_NewProject = NULL;

GList *OtherWindows_CurrentProject = NULL;
GList *OtherWindows_ClosedProjects = NULL;

//GtkWidget *ConsoleWindow = NULL;

//GtkWidget *MessagesText = NULL;
GdkFont *MessagesFont = NULL;
GdkColor Red, Green, Blue;

int NewProjectCount = 1;

//PtrBalsaProject CurrentProject = NULL;
GList *Projects = NULL;
char *ProjectsFilename = NULL;

char *StartupPWD;
GtkWidget *RecentProjectsMenu = NULL;

PtrBalsaProjectEntry ProjectFileViewSelectedEntry = NULL;

static GList *FindFileProcedures (char *filename);
static gboolean AddFileCallback (char *filename, gpointer userData);
static int GetProcedureLine (char *procedureName, char *filename);
static gboolean OpenProjectCallback (char *directory, gpointer user_data);

void AddToOtherWindows_CurrentProject (GtkWidget * dialogue)
{
    OtherWindows_CurrentProject = g_list_append (OtherWindows_CurrentProject, dialogue);
}

void RemoveFromOtherWindows_CurrentProject (GtkWidget * dialogue)
{
    OtherWindows_CurrentProject = g_list_remove (OtherWindows_CurrentProject, dialogue);
}

void AddToOtherWindows_ClosedProjects (GtkWidget * dialogue)
{
    OtherWindows_ClosedProjects = g_list_append (OtherWindows_ClosedProjects, dialogue);
}

void RemoveFromOtherWindows_ClosedProjects (GtkWidget * dialogue)
{
    OtherWindows_ClosedProjects = g_list_remove (OtherWindows_ClosedProjects, dialogue);
}

void DestroyOtherWindow (GtkWidget * dialogue)
{
    RemoveFromOtherWindows_CurrentProject (dialogue);
    RemoveFromOtherWindows_ClosedProjects (dialogue);
    gtk_widget_destroy (dialogue);
}
static void RemoveProjectNameFromRecentProjectsHistory_OnYes (GtkWidget * button, char *projectName)
{
    //  printf("remove %s\n",projectName);
    GSList *history = EnvironmentOptions.projectsHistory;
    GSList *item;

    for (item = history; item; item = item->next)
        if (!strcmp (item->data, projectName))
            EnvironmentOptions.projectsHistory = g_slist_remove_link (history, item);

    WriteEnvironmentOptions ();
}

static char *NameProjectOpening;
void RecentProjectsHistory_MouseClick_afterSaveConfirmation (GtkWidget * button, gpointer user_data)
{
    if (OpenProjectCallback (NameProjectOpening, NULL) == FALSE)
    {
        // Ask to remove the project name from the RecentProjectsHistory
        GtkWidget *dialogue;
        const gchar *buttons[] = { "Yes", "No" };
        GtkSignalFunc handlers[] = { RemoveProjectNameFromRecentProjectsHistory_OnYes,
            NULL
        };

        dialogue =
          util_create_dialogue_with_buttons
          ("No Project file found in this directory.\nDo you want to remove it from the 'recent projects history'?",
          2, buttons, 1, handlers, NameProjectOpening);
        gtk_window_set_position (GTK_WINDOW (dialogue), GTK_WIN_POS_MOUSE);
        gtk_widget_show (dialogue);
    }
}
void RecentProjectsHistory_MouseClick (gchar * string)
{
    NameProjectOpening = string;
    ConfirmSaveCurrentBalsaProjectAndExecute (RecentProjectsHistory_MouseClick_afterSaveConfirmation);
}

static void RecentProjectsHistory_ClearMenu_OnYes (GtkWidget * button, char *projectName)
{
    EnvironmentOptions.projectsHistory = NULL;
    WriteEnvironmentOptions ();
}

void RecentProjectsHistory_ClearMenu (gchar * string)
{
    GtkWidget *dialogue;
    const gchar *buttons[] = { "Yes", "No" };
    GtkSignalFunc handlers[] = { RecentProjectsHistory_ClearMenu_OnYes,
        NULL
    };

    dialogue = util_create_dialogue_with_buttons ("Are you sure you want to clear the 'recent projects history'?", 2, buttons, 1, handlers, NULL);
    gtk_window_set_position (GTK_WINDOW (dialogue), GTK_WIN_POS_MOUSE);
    gtk_widget_show (dialogue);
}

void AddRecentProject (char *projectName)
{
    GSList *ptr = EnvironmentOptions.projectsHistory;
    char *tmp;

    while (ptr)
    {
        if (!strcmp ((char *) ptr->data, projectName))
        {
            EnvironmentOptions.projectsHistory = g_slist_remove (EnvironmentOptions.projectsHistory, ptr->data);
            break;
        }
        ptr = ptr->next;
    }

    tmp = (char *) g_strdup (projectName);
    EnvironmentOptions.projectsHistory = g_slist_append (EnvironmentOptions.projectsHistory, tmp);

    WriteEnvironmentOptions ();
}

void UpdateSelectionMenuGreying (void)
{
    if (!ProjectFileViewSelectedEntry)
        return;

    GtkWidget *SelectionMenu_AddTest = GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu_AddTest"));
    GtkWidget *SelectionMenu_AddLardTestFile = GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu_AddLardTestFile"));
    GtkWidget *SelectionMenu_AddImplementation = GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu_AddImplementation"));
    GtkWidget *SelectionMenu_AddBuiltinLib = GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu_AddBuiltinLib"));
    GtkWidget *SelectionMenu_Delete = GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu_Delete"));

    switch (ProjectFileViewSelectedEntry->nature)
    {
    case BalsaProjectEntry_File:
        gtk_widget_set_sensitive (SelectionMenu_AddTest, 0);
        gtk_widget_set_sensitive (SelectionMenu_AddLardTestFile, 0);
        gtk_widget_set_sensitive (SelectionMenu_AddImplementation, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddBuiltinLib, 1);
        gtk_widget_set_sensitive (SelectionMenu_Delete, 1);
        break;

    case BalsaProjectEntry_Procedure:
        gtk_widget_set_sensitive (SelectionMenu_AddTest, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddLardTestFile, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddImplementation, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddBuiltinLib, 1);
        gtk_widget_set_sensitive (SelectionMenu_Delete, 0);
        break;

    case BalsaProjectEntry_Directory:
        gtk_widget_set_sensitive (SelectionMenu_AddTest, 0);
        gtk_widget_set_sensitive (SelectionMenu_AddLardTestFile, 0);
        gtk_widget_set_sensitive (SelectionMenu_AddImplementation, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddBuiltinLib, 1);
        gtk_widget_set_sensitive (SelectionMenu_Delete, 0);
        break;

    case BalsaProjectEntry_TestComponent:
        gtk_widget_set_sensitive (SelectionMenu_AddTest, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddLardTestFile, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddImplementation, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddBuiltinLib, 1);
        gtk_widget_set_sensitive (SelectionMenu_Delete, 0);
        break;

    case BalsaProjectEntry_Test:
    case BalsaProjectEntry_LardTestFile:
    case BalsaProjectEntry_Implementation:
    case BalsaProjectEntry_BuiltinLib:
        gtk_widget_set_sensitive (SelectionMenu_AddTest, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddLardTestFile, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddImplementation, 1);
        gtk_widget_set_sensitive (SelectionMenu_AddBuiltinLib, 1);
        gtk_widget_set_sensitive (SelectionMenu_Delete, 1);
        break;
    }
}

/* UpdateMainWindowGreying : update the greying out of menus/menu items in the MainWindow */
void UpdateMainWindowGreying (void)
{
    GtkObject *toolbarProject = GTK_OBJECT (gtk_object_get_data (MainWindowObject, "ProjectToolbar"));
    GtkObject *toolbarFile = GTK_OBJECT (gtk_object_get_data (MainWindowObject, "FilesToolbar"));

    gboolean projectActive = CurrentBalsaProject ? TRUE : FALSE;
    gboolean fileActive = (FileManager_GetCurrentDisplayedName () != NULL);
    gboolean fileActiveNamed = (FileManager_GetCurrentFileName () != NULL);
    gboolean projectSelectionActive = (ProjectFileViewSelectedEntry != NULL);

    // Project Toolbar
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarProject, "Icon_SaveProject")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarProject, "Icon_AddFileToProject")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarProject, "Icon_LaunchEditor")), projectSelectionActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarProject, "Icon_ProjectOptions")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarProject, "Icon_Update")), projectActive);

    // Files Toolbar
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarFile, "Icon_SaveFile")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarFile, "Icon_CloseFile")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarFile, "Icon_LaunchEditor")), fileActiveNamed);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (toolbarFile, "Icon_Print")), fileActiveNamed);

    // Project Menu
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ProjectMenu_Save")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ProjectMenu_SaveAs")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ProjectMenu_Close")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ProjectMenu_ProjectOptions")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ProjectMenu_AddFilesIntoProject")), projectActive);

    // File Menu
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "FileMenu_ReOpen")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "FileMenu_Save")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "FileMenu_SaveAs")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "FileMenu_Close")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "FileMenu_CloseAll")), fileActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "FileMenu_AddCurrentFileToProject")), fileActive);

    // Selection Menu
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu")), projectSelectionActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu_GetInfo")), FALSE);

    // View Menu
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ViewMenu_Files")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ViewMenu_Procedures")), projectActive);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ViewMenu_Tests")), projectActive);

    // Build Menu
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "BuildMenu")), projectActive);

    // CVS Menu
    /* Always grey this out for the moment */
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "CVSMenu")), FALSE);
    /* And the Tools menu tool */
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "ToolsMenu")), FALSE);

    UpdateSelectionMenuGreying ();
    UpdateToolsMenuGreying ();
}

/* FindDirectoryRow : returns 0 if the pathFragment given matches the `name' of
	the given BalsaFile (if that BalsaFile is a BalsaDirectoryNode) */
gint FindDirectoryRow (PtrBalsaProjectEntry entry, char *pathFragment)
{
    return !(entry->nature == BalsaProjectEntry_Directory && strcmp (BALSA_FILE (entry->data)->name, pathFragment) == 0);
}

static void ConfirmAddImportPathForAddingFile_OnYes (GtkWidget * button, char *name_with_path)
{
    //  printf("confirmed %s\n", (char*)name_with_path);
    // Add path to the import path list
    char *lastSlash = strrchr (name_with_path, '/');

    if (lastSlash && (lastSlash > name_with_path))
    {
        int size = lastSlash - name_with_path;
        char *path = (char *) malloc (size + 1);

        strncpy (path, name_with_path, size);
        path[size] = 0;
        CurrentBalsaProject->importPath = g_list_append (CurrentBalsaProject->importPath, path);
    } else
    {
        // should never happen
        printfConsole ("Unexpected error in ConfirmAddImportPathForAddingFile_OnYes\n");
        return;
    }

    // Retry to add the file into the project
    AddFileCallback (name_with_path, NULL);
}

/* AddFileToProject : add a named file to the project `project' */
void AddFileToProject (PtrBalsaProject project, PtrBalsaFile file, char *CompletePathName)
{
    //todo: display this in the console:
    //  printf( "ajout de %s (%s)\n", file->name, CompletePathName);

    // Check the file doesn't already appear in the list
    {
        GList *files = project->files;

        for (; files; files = files->next)
            if (!strcmp (((PtrBalsaFile) (files->data))->name, file->name))
            {
                printfConsole ("File already in the project\n");
                return;
            }
    }

    // Check this is a valid file name
    // Check there is no name conflict between different paths
    {
        char *matchingPath = FindImportPathForFile (g_strdup_printf ("%s.balsa", file->name));

        if (!matchingPath)
        {
            char *lastSlash = NULL;

            if (CompletePathName)
                lastSlash = strrchr (CompletePathName, '/');
            if (lastSlash && (lastSlash > CompletePathName))
            {
                GtkWidget *dialogue;
                const gchar *buttons[] = { "Yes", "Cancel" };
                GtkSignalFunc handlers[] = {
                    ConfirmAddImportPathForAddingFile_OnYes,
                    NULL
                };

                dialogue =
                  util_create_dialogue_with_buttons
                  ("This filename doesn't appear in any import path.\nDo you want to add its path into the list of import paths?",
                  2, buttons, 1, handlers, CompletePathName);
                gtk_window_set_position (GTK_WINDOW (dialogue), GTK_WIN_POS_MOUSE);
                gtk_widget_show (dialogue);
            } else
            {
                printfConsole_s
                  ("Warning! This filename (%s.balsa) doesn't appear in any import path. Action 'AddFileToProject' cancelled.\n", file->name);
            }
            return;
        }
    }

    project->files = g_list_append (project->files, file);
    project->dirty = TRUE;
    //  project->needToRebuildMakefile = TRUE;
    UpdateProjectTrees ();
}

/* AddLardTestFileToFile : add a named LARD test file to the file section `balsaFile', in the project `project' */
void AddLardTestFileToFile (PtrBalsaProject project, PtrBalsaFile balsaFile, PtrLardTestFile lardFile)
{
    balsaFile->info.balsaFile.lardTestFiles = g_list_append (balsaFile->info.balsaFile.lardTestFiles, lardFile);
    project->dirty = TRUE;
    project->needToRebuildMakefile = TRUE;
    UpdateProjectTrees ();
}

/* AddImplementationToFile : add a named implementation to the file section `balsaFile', in the project `project' */
void AddImplementationToFile (PtrBalsaProject project, PtrBalsaFile balsaFile, PtrImplementation impl)
{
    balsaFile->info.balsaFile.implementations = g_list_append (balsaFile->info.balsaFile.implementations, impl);
    project->dirty = TRUE;
    project->needToRebuildMakefile = TRUE;
    UpdateProjectTrees ();
}

/* AddImplementationToTest : add a named implementation to the test section `balsaTest', in the project `project' */
void AddImplementationToTest (PtrBalsaProject project, PtrBalsaTest balsaTest, PtrImplementation impl)
{
    balsaTest->implementations = g_list_append (balsaTest->implementations, impl);
    project->dirty = TRUE;
    project->needToRebuildMakefile = TRUE;
    UpdateProjectTrees ();
}

/* AddBuiltinLibToFile : add a named builtinLib to the file section `balsaFile', in the project `project' */
void AddBuiltinLibToFile (PtrBalsaProject project, PtrBalsaFile balsaFile, PtrBuiltinLib lib)
{
    if (balsaFile->info.balsaFile.builtinLib)
    {
        fprintf (stderr, "Error AddBuiltinLibToFile: Trying to add more than 1 lib (Only 1 lib authorised)\n");
    }
    balsaFile->info.balsaFile.builtinLib = lib;
    project->dirty = TRUE;
    project->needToRebuildMakefile = TRUE;
    UpdateProjectTrees ();
}

/* SetEntryStringCallback : set the given entry to the string value passed */
gboolean SetEntryStringCallback (char *string, GtkWidget * entry)
{
    gtk_entry_set_text (GTK_ENTRY (entry), string);
    gtk_entry_set_position (GTK_ENTRY (entry), -1);

    return TRUE;                /* destroy dialogue */
}

/* MakeDirectorySelectionDialogue : make a directory selection dialogue
	which calls callback `callback' on pressing OK. Destroys itself
	on destruction of the parent window `parent' (if any) and when either
	of OK or Cancel are pressed. `initialDirectory' is the starting 
	directory selection and displayed directory. */
GtkWidget *MakeDirectorySelectionDialogue (char *title,
  char *initialDirectory, FileSelectionCallback callback, gpointer callbackData, GtkSignalFunc changeDirCallBack, GtkWidget * parent)
{
    GtkWidget *dialogue = create_DirectoryDialogue ();

    // GtkCList *file_list = GTK_CLIST (GTK_FILE_SELECTION (dialogue)->file_list);

    /* Gruby but working method of removing file selection */
    gtk_widget_hide (GTK_FILE_SELECTION (dialogue)->file_list->parent);

    if (initialDirectory && *initialDirectory)
    {
        char *directory = (char *) g_malloc (strlen (initialDirectory) + 2);

        sprintf (directory, "%s/", initialDirectory);
        gtk_file_selection_set_filename (GTK_FILE_SELECTION (dialogue), directory);
        g_free (directory);
    }

    gtk_window_set_title (GTK_WINDOW (dialogue), title);
    gtk_object_set_data (GTK_OBJECT (dialogue), "OKCallback", (gpointer) callback);
    gtk_object_set_data (GTK_OBJECT (dialogue), "CallbackData", callbackData);

    if (parent)
    {
        gtk_signal_connect_object_while_alive (GTK_OBJECT (parent), "destroy", GTK_SIGNAL_FUNC (OnDialogueCancel), GTK_OBJECT (dialogue));
    }

    if (changeDirCallBack)
    {
        gtk_signal_connect (GTK_OBJECT (dialogue), "draw", GTK_SIGNAL_FUNC (changeDirCallBack), NULL);
        (GTK_SIGNAL_FUNC (changeDirCallBack)) (dialogue);
    }

    gtk_widget_show (dialogue);
    return dialogue;
}

/* MakeFileSelectionDialogue : like MakeDirectorySelectionDialogue
	but for file selection instead of directories. */
GtkWidget *MakeFileSelectionDialogue (char *title, FileSelectionCallback callback, gpointer callbackData, GtkWidget * parent)
{
    GtkWidget *dialogue = create_FileDialogue ();

    gtk_window_set_title (GTK_WINDOW (dialogue), title);
    gtk_object_set_data (GTK_OBJECT (dialogue), "OKCallback", (gpointer) callback);
    gtk_object_set_data (GTK_OBJECT (dialogue), "CallbackData", callbackData);

    if (parent)
    {
        gtk_signal_connect_object_while_alive (GTK_OBJECT (parent), "destroy", GTK_SIGNAL_FUNC (OnDialogueCancel), GTK_OBJECT (dialogue));
    }

    gtk_widget_show (dialogue);
    return dialogue;
}

/* Upon{Directory,File,Confirm,}Dialogue{OK,Yes,Cancel} : raw callbacks for Directory/File/ConfirmDialogue */
void UponDirectoryDialogueOK (GtkObject * dialogue)
{
    FileSelectionCallback callback = (FileSelectionCallback) gtk_object_get_data (dialogue,
      "OKCallback");
    char *directory = g_dirname (gtk_file_selection_get_filename (GTK_FILE_SELECTION (dialogue)));
    GtkWidget *entry = GTK_WIDGET (GTK_FILE_SELECTION (dialogue)->selection_entry);
    char *text = gtk_entry_get_text (GTK_ENTRY (entry));

    if (!text || !*text || !strcmp (text, ".") || !strcmp (text, "./"))
    {
        GtkWidget *OKButton = GTK_WIDGET (GTK_FILE_SELECTION (dialogue)->ok_button);
        gboolean sensitive = GTK_WIDGET_SENSITIVE (OKButton);

        if (sensitive)
        {
            gboolean closeDialogue = callback (directory,
              (gpointer) gtk_object_get_data (dialogue,
                "CallbackData"));

            g_free (directory);
            if (closeDialogue)
                gtk_widget_destroy (GTK_WIDGET (dialogue));
        }
    } else
    {
        char *newdir, *newdir2;

        if (text[strlen (text) - 1] == '/')
            newdir = directory;
        else
            newdir = (text[0] == '/') ? text : g_strdup_printf ("%s/%s", directory, text);

        newdir2 = g_strdup_printf ("%s/", ConvertToAbsolutePath (newdir, 0));

        //              printf("%s - %s - %s - %s\n",directory,text,newdir,newdir2);
        gtk_file_selection_set_filename (GTK_FILE_SELECTION (dialogue), newdir2);
    }
}

void UponFileDialogueOK (GtkObject * dialogue)
{
    FileSelectionCallback callback = (FileSelectionCallback) gtk_object_get_data (dialogue,
      "OKCallback");
    char *file = gtk_file_selection_get_filename (GTK_FILE_SELECTION (dialogue));
    gboolean closeDialogue = callback (file,
      (gpointer) gtk_object_get_data (dialogue,
        "CallbackData"));

    if (closeDialogue)
        gtk_widget_destroy (GTK_WIDGET (dialogue));
}

void UponDialogueCancel (GtkObject * dialogue)
{
    gtk_widget_destroy (GTK_WIDGET (dialogue));
}

/* MakeTopLevelWindows : make persistent windows on startup */
void MakeTopLevelWindows (void)
{
    MainWindow = create_MainWindow ();
    MainWindowObject = GTK_OBJECT (MainWindow);
    //    gtk_window_set_title (GTK_WINDOW (MainWindow), TITLE_VERSION);

    //    OptionsDialogue = create_OptionsDialogue ();
    ToolsMenuEditorDialog = create_ToolsMenuEditorDialog ();

    //    ConsoleWindow = create_ConsoleWindow ();

    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu")), FALSE);
}

int OpenRecentButtonPressedCallback (GtkWidget * widget, GdkEventButton * event, GtkWidget * user_data)
{
    if (event->button == 3)     //Right button pressed
        gtk_menu_popup (GTK_MENU (RecentProjectsMenu), NULL, NULL, NULL, NULL, 0, 0);

    /* Tell calling code that we have not handled this event; pass it on. */
    return FALSE;
}

void AllocateFixedResources (void)
{
    GdkColormap *colours;

    // GtkStyle *style;

    // MessagesText = GTK_WIDGET (gtk_object_get_data (GTK_OBJECT (ConsoleWindow), "ConsoleTextBox"));
    //    MessagesFont = gdk_font_load ("-*-lucidatypewriter-medium-*-*-*-*-120-*-*-*-*-*-*");
    MessagesFont = gdk_fontset_load ("-*-fixed-*-*-*-*-*-120-*-*-*-*-*-*");

    colours = gdk_colormap_get_system (); //gtk_widget_get_colormap (MessagesText);
    Red.red = 65535;
    Red.blue = 0;
    Red.green = 0;
    if (!gdk_color_alloc (colours, &Red))
        g_error ("couldn't allocate colour");
    Green.red = 0;
    Green.blue = 0;
    Green.green = 35000;
    if (!gdk_color_alloc (colours, &Green))
        g_error ("couldn't allocate colour");
    Blue.red = 0;
    Blue.blue = 65535;
    Blue.green = 0;
    if (!gdk_color_alloc (colours, &Blue))
        g_error ("couldn't allocate colour");

    init_icons ();
}

char *program_name;
void signal_FAULT (int sig)
{
    printf
      ("\n*************************************\n Fatal Signal (%d) received !\nIf you want to help, run the following command: \"gdb %s %d\", and send us the result of the \"backtrace full\" command\nThis program is paused during 2 minutes to let you some time to run gdb, and dies afterwards.\n",
      sig, program_name, getpid ());
    sleep (120);
    exit (1);
}

/* *** MAIN *** */

int main (int argc, char *argv[])
{
    program_name = argv[0];

    //    gtk_set_locale ();
    gtk_init (&argc, &argv);

    if (argc > 2)
    {
        fprintf (stderr, "                                        \n");
        fprintf (stderr, " |_  _ |  _ _    ._  _ ._  _  _  _ ._   [ balsa-manager: Balsa Project Manager ]\n");
        fprintf (stderr, " |_)(_\\|_/ (_\\ - |||(_\\| |(_\\(_|(-'| `  (C) 1999, Amulet Group\n");
        fprintf (stderr, "                              -'        \n");
        fprintf (stderr, "usage: balsa-manager [<project-directory>]\n");
        exit (1);
    }
    //    signal (SIGSEGV, signal_FAULT);
    signal (SIGILL, signal_FAULT);
    //Mac pb:    signal (SIGBUS, signal_FAULT);
    signal (SIGFPE, signal_FAULT);

    StartupPWD = g_get_current_dir ();

    MakeTopLevelWindow ();
     /**/ InitOptions ();
     /**/ ReadEnvironmentOptions ();
    gtk_widget_show (MainWindow);
    AllocateFixedResources ();
    init_workspace ();

     /**/ ExecutionManager_RemoveTmpFilesOfOldSessions ();

    gboolean tech_dir_found = FALSE;
    char *balsahome = getenv ("BALSAHOME");

    if (balsahome && *balsahome)
    {
        char *TECH_DIR2 = g_strdup_printf ("%s/share/tech", balsahome);

        if (FindBalsaTechnologies (TECH_DIR2))
            tech_dir_found = TRUE;
        g_free (TECH_DIR2);
    }
#define TECH_DIR (BALSAHOME "/share/tech")
    if (!tech_dir_found && !FindBalsaTechnologies (TECH_DIR))
    {
        printfConsole ("No valid Balsa technologies found\n");
    }

    if (argc > 1)
        OpenProjectCallback (argv[1], 0);
    else
    {
        FILE *f = fopen ("Project", "r");

        if (f)
        {
            fclose (f);
            OpenProjectCallback (".", 0);
        }
    }

    gtk_main ();
    if (chdir (StartupPWD) == -1) {
      printfConsole("Could not enter working directory\n");
    }
    return 0;

    /*
       MakeTopLevelWindows(  );
       InitOptions(  );
       ReadEnvironmentOptions(  );
       gtk_widget_show( MainWindow );
       AllocateFixedResources(  );
       BuildToolbarIcons(  );
       FileManager_Initialize(  );
       ExecutionManager_Initialize(  );
       UpdateToolsMenu(  );
       UpdateMainWindowGreying(  );
       ExecutionManager_RemoveTmpFilesOfOldSessions(  );

       if ( argc > 1 )
       OpenProjectCallback( argv[1], 0 );

       gtk_main(  );
       chdir( StartupPWD );
       return 0;
     */
}

static gboolean AddFileCallback (char *filename, gpointer userData)
{
    PtrBalsaFile newFile = NewBalsaFileWithName (filename);

    if (newFile)
        AddFileToProject (CurrentBalsaProject, newFile, filename);
    else
        printfConsole ("You can only add .balsa files (for the moment)\n");
    return FALSE;
}

void UponProjectMenu_AddFile (GtkMenuItem * menuitem, gpointer user_data)
{
    showAddFileDialog ();
}

void UponProjectMenu_Update (GtkMenuItem * menuitem, gpointer user_data)
{
    // Check each file in the project:
    // - If the breeze exists: If balsa has been modified later than breeze, then recompile.
    GList *files = CurrentBalsaProject->files;
    GList *commandList = NULL;

    for (; files; files = files->next)
    {
        PtrBalsaFile file = files->data;
        char *filename = file->name;
        char *breezeFilename = ConvertToPathedBreezeFilename (filename);
        char *balsaFilename = ConvertToPathedBalsaFilename (filename);
        struct stat stat_breeze, stat_balsa;

        if (stat (breezeFilename, &stat_breeze) == 0)
        {                       // Breeze file exists
            stat (balsaFilename, &stat_balsa);
            if (stat_balsa.st_mtime > stat_breeze.st_mtime)
            {
                // balsa has been modified later than breeze => recompile.
                char *command = g_strdup_printf ("make -n %s", breezeFilename);

                commandList = g_list_append (commandList, command);
                //      printf ("Need to recompile %s\n", balsaFilename);
            }
        }
    }

    // Re-generate the project views (Files and Makefile)
    if (commandList)
    {
        char *command = g_strdup_printf ("~ExecuteFunction0arg %ld",
          (size_t) UpdateProjectTrees);

        commandList = g_list_append (commandList, command);
        ExecutionManager_CheckBuildMakefileAndRunCommandList (commandList);
    } else
    {
        UpdateProjectTrees ();
    }
}

gboolean LardTestFileSelectionCallback (char *filename, gpointer userData)
{
    GtkWidget *dialogue = GTK_WIDGET (userData);
    GtkEntry *filenameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "FileNameEntry"));

    gtk_entry_set_text (filenameEntry, g_strdup (filename));

    return TRUE;                /* destroy dialogue */
}

void UponLardTestFileOptionsBrowse (GtkObject * dialogue)
{
    MakeFileSelectionDialogue ("Select a LARD Test File",
      (FileSelectionCallback) LardTestFileSelectionCallback, (gpointer) GTK_WIDGET (dialogue), GTK_WIDGET (dialogue));
}

void UponLardTestFileOptionsDialogueOK (GtkObject * dialogue)
{
    GtkEntry *nameEntry = GTK_ENTRY (gtk_object_get_data (dialogue, "NameEntry"));
    GtkEntry *filenameEntry = GTK_ENTRY (gtk_object_get_data (dialogue, "FileNameEntry"));
    GtkEntry *simArgsEntry = GTK_ENTRY (gtk_object_get_data (dialogue, "SimArgsEntry"));
    GtkEntry *simOptsEntry = GTK_ENTRY (gtk_object_get_data (dialogue, "SimOptsEntry"));
    GtkEntry *buildOptsEntry = GTK_ENTRY (gtk_object_get_data (dialogue, "BuildOptsEntry"));

    char *nameValue = gtk_entry_get_text (nameEntry);
    char *filenameValue = gtk_entry_get_text (filenameEntry);
    char *simArgsValue = gtk_entry_get_text (simArgsEntry);
    char *simOptsValue = gtk_entry_get_text (simOptsEntry);
    char *buildOptsValue = gtk_entry_get_text (buildOptsEntry);

    PtrLardTestFile editedTest = (PtrLardTestFile) gtk_object_get_data (dialogue,
      "EditedTest");

    if (!nameValue || !*nameValue)
    {                           //empty test name
        printfConsole ("Error in AddLardTestFile : Please specify a test name\n");
        return;
    }
    if (!filenameValue || !*filenameValue)
    {                           //empty filetest name
        printfConsole ("Error in AddLardTestFile : Please specify a test file\n");
        return;
    }
    // now check the test name doesn't already exist (if it is a new test or if the name changed)
    if ((editedTest == NULL) || strcmp (editedTest->name, nameValue))
    {
        GList *files;

        for (files = CurrentBalsaProject->files; files; files = files->next)
        {
            PtrBalsaFile file = (PtrBalsaFile) files->data;
            GList *tests = file->info.balsaFile.testFixtures;

            for (; tests; tests = tests->next)
            {
                PtrBalsaTest test = (PtrBalsaTest) tests->data;

                if (!strcmp (nameValue, test->name))
                {
                    printfConsole ("Error in AddTest : The specified test name is already in use\n");
                    return;
                }
            }

            tests = file->info.balsaFile.lardTestFiles;
            for (; tests; tests = tests->next)
            {
                PtrLardTestFile test = (PtrLardTestFile) tests->data;

                if (!strcmp (nameValue, test->name))
                {
                    printfConsole ("Error in AddTest : The specified test name is already in use\n");
                    return;
                }
            }
        }
    }

    if (editedTest)
    {
        editedTest->name = g_strdup (nameValue);
        editedTest->filename = ImportPaths_ConvertFileToRelative (filenameValue);
        editedTest->simArgs = g_strdup (simArgsValue);
        editedTest->simOpts = g_strdup (simOptsValue);
        editedTest->buildOpts = g_strdup (buildOptsValue);
        CurrentBalsaProject->dirty = TRUE;
        CurrentBalsaProject->needToRebuildMakefile = TRUE;
        UpdateProjectTrees ();
    } else
    {
        PtrBalsaFile balsaFile = (PtrBalsaFile) gtk_object_get_data (GTK_OBJECT (dialogue),
          "BalsaFile");

        PtrLardTestFile lardFile = NewLardTestFileWithName (nameValue);

        lardFile->filename = ImportPaths_ConvertFileToRelative (filenameValue);
        lardFile->matchingBalsaFile = balsaFile;
        lardFile->simArgs = g_strdup (simArgsValue);
        lardFile->simOpts = g_strdup (simOptsValue);
        lardFile->buildOpts = g_strdup (buildOptsValue);
        AddLardTestFileToFile (CurrentBalsaProject, balsaFile, lardFile);
    }

    gtk_widget_hide GTK_WIDGET ((dialogue));
}

void UponLardTestFileOptionsDialogueCancel (GtkObject * dialogue)
{
    gtk_widget_hide (GTK_WIDGET (dialogue));
}

gboolean UponLardTestFileOptionsDialog_keyPressEvent (GtkObject * widget, GdkEventKey * event, gpointer user_data)
{
    if (event->keyval == GDK_Return)
        UponLardTestFileOptionsDialogueOK (widget);
    else if (event->keyval == GDK_Escape)
        UponLardTestFileOptionsDialogueCancel (widget);
    return FALSE;
}

void CallLardTestFileSelectionWindow (PtrBalsaFile file, PtrLardTestFile editedTest)
{
    GtkWidget *dialogue = create_LardTestFileOptionsDialogue ();
    GtkEntry *nameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "NameEntry"));
    GtkEntry *filenameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "FileNameEntry"));
    GtkEntry *simArgsEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "SimArgsEntry"));
    GtkEntry *simOptsEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "SimOptsEntry"));
    GtkEntry *buildOptsEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "BuildOptsEntry"));

    if (editedTest)
    {
        gtk_entry_set_text (nameEntry, g_strdup (editedTest->name));
        gtk_entry_set_text (filenameEntry, g_strdup (editedTest->filename));
        gtk_entry_set_text (simArgsEntry, g_strdup (editedTest->simArgs));
        gtk_entry_set_text (simOptsEntry, g_strdup (editedTest->simOpts));
        gtk_entry_set_text (buildOptsEntry, g_strdup (editedTest->buildOpts));
    } else
    {                           // find a unused name for the test
        char *test_name = NULL;
        int num = 1;

      retry:
        g_free (test_name);
        test_name = g_strdup_printf ("lardtest%d", num);
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

        gtk_entry_set_text (nameEntry, test_name);
    }

    /* Make name selected so we can directly overtype it */
    gtk_entry_select_region (nameEntry, 0, -1);
    gtk_widget_grab_focus (GTK_WIDGET (nameEntry));

    gtk_object_set_data (GTK_OBJECT (dialogue), "BalsaFile", file);
    gtk_object_set_data (GTK_OBJECT (dialogue), "EditedTest", editedTest);

    gtk_widget_show (dialogue);

    /*
       char *title = g_strdup_printf( "Add LARD Test File to %s", file->name );
       MakeFileSelectionDialogue( title, (FileSelectionCallback)LardTestFileSelectionCallback, (gpointer)file, 0 );
       free( title );
     */
}

gboolean UponMainWindowDestroy (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    gtk_main_quit ();
    return TRUE;
}

gboolean UponMainWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    UponProjectMenu_Quit (NULL, NULL);
    return TRUE;
}

void UponBalsaHomeDefault (GtkButton * button, gpointer user_data)
{
    GtkWidget *BalsaHomeEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
      "BalsaHomeEntry");

    gtk_entry_set_text (GTK_ENTRY (BalsaHomeEntry), Options_BalsaHome);
}

void UponTmpDirDefault (GtkButton * button, gpointer user_data)
{
    GtkWidget *TmpDirEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
      "TmpDirEntry");

    gtk_entry_set_text (GTK_ENTRY (TmpDirEntry), Options_TmpDir);
}

void UponEditorNameDefault (GtkButton * button, gpointer user_data)
{
    GtkWidget *EditorNameEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
      "EditorNameEntry");

    gtk_entry_set_text (GTK_ENTRY (EditorNameEntry), Options_Editor);
}

void UponPrintCommandDefault (GtkButton * button, gpointer user_data)
{
    GtkWidget *PrintCommandEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
      "PrintCommandEntry");

    gtk_entry_set_text (GTK_ENTRY (PrintCommandEntry), Options_PrintCommand);
}

void UponPSViewerNameDefault (GtkButton * button, gpointer user_data)
{
    GtkWidget *EditorNameEntry = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (workSpace.optionsDialog->dialog),
      "PSViewerNameEntry");

    gtk_entry_set_text (GTK_ENTRY (EditorNameEntry), Options_PSViewer);
}

void UponOptionsWindowShow (GtkWidget * widget, gpointer user_data)
{
}

/* *** OptionsDialogue END */

void UponProjectFileviewSelectRow (GtkWidget * tree, GtkCTreeNode * node, gint column, gpointer user_data)
{
    PtrBalsaProjectEntry entry = BALSA_PROJECT_ENTRY (gtk_ctree_node_get_row_data (GTK_CTREE (tree),
        GTK_CTREE_NODE (node)));

    ProjectFileViewSelectedEntry = entry;

    gtk_object_set_data (GTK_OBJECT (tree), "SelectedNode", (gpointer) node);
    gtk_widget_set_sensitive (GTK_WIDGET (gtk_object_get_data (MainWindowObject, "SelectionMenu")), TRUE);

    /* set the `Details' column title to something better */
    gtk_clist_set_column_title (GTK_CLIST (tree), 1, (!entry ? "Details" : BalsaProjectEntryDetailsNames[(int) entry->nature]));

    /* auto-edit the file */
    {
        char *filename = NULL;
        int line = 0;

        switch (entry->nature)
        {
        case BalsaProjectEntry_File:
            if (BALSA_FILE (entry->data)->nature == BalsaFile_File)
                filename = ConvertToBalsaFilename (BALSA_FILE (entry->data)->name);
            break;

        case BalsaProjectEntry_Procedure:
            {
                char *procedure_name = BALSA_PROCEDURE (entry->data)->name;

                filename = ConvertToBalsaFilename (BALSA_PROCEDURE (entry->data)->associated_filename);
                line = GetProcedureLine (procedure_name, filename);
            }
            break;

        case BalsaProjectEntry_TestComponent:
            switch (BALSA_TESTCOMPONENT (entry->data)->nature)
            {
            case BalsaTestComponent_InputFromFile:
            case BalsaTestComponent_OutputToFile:
            case BalsaTestComponent_Memory:
            case BalsaTestComponent_OutputToStdout:
                filename = ExtractCommaSeparatedParam (BALSA_TESTCOMPONENT (entry->data)->value, 0);
                break;
            case BalsaTestComponent_Sync:
            case BalsaTestComponent_InputFromValue:
            case BalsaTestComponent_Undefined:
                break;
            }
            break;

        case BalsaProjectEntry_LardTestFile:
            filename = LARDTEST_FILE (entry->data)->filename;
            break;

        case BalsaProjectEntry_Implementation:
            break;

        case BalsaProjectEntry_BuiltinLib:
        {
            PtrBuiltinLib lib = BUILTINLIB (entry->data);
            if (lib->sourceFilenames)
                filename = lib->sourceFilenames->data;
        }
           break;

        default:
            break;
        }

        if (filename && !BEGINS_WITH (filename, "Memory("))
        {
            if (line)
                FileManager_OpenFileAtLine (filename, line);
            else
                FileManager_OpenFile (filename);
        }
    }

    UpdateMainWindowGreying ();
}

void UponProjectFileviewUnselectRow (GtkWidget * tree, GtkCTreeNode * node, gint column, gpointer user_data)
{
    gtk_object_set_data (GTK_OBJECT (tree), "SelectedNode", NULL);

    /* set the `Details' column title back to "Details" */
    gtk_clist_set_column_title (GTK_CLIST (tree), 1, "Details");

    ProjectFileViewSelectedEntry = NULL;
    UpdateMainWindowGreying ();
}

gboolean UponProjectFileviewMousePressed (GtkWidget * tree, GdkEventButton * event, GtkWidget * user_data)
{
    if (event->button != 1)
    {
        int row, column;

        gtk_clist_get_selection_info (GTK_CLIST (tree), event->x, event->y, &row, &column);
        gtk_clist_select_row (GTK_CLIST (tree), row, 0);
    }

    if ((event->button == 3) && ProjectFileViewSelectedEntry) //Right button pressed
    {
        GtkWidget *menu = (GtkWidget *) gtk_object_get_data (MainWindowObject,
          "SelectionMenu_menu");

        gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, 0);
    } else if ((event->button == 2) && ProjectFileViewSelectedEntry) //Middle button pressed on test -> execute breeze-sim-ctrl
    {
        switch (ProjectFileViewSelectedEntry->nature)
        {
        case BalsaProjectEntry_Test:
            {
                PtrBalsaTest test = BALSA_TEST (ProjectFileViewSelectedEntry->data);
                char *makeName = g_strdup_printf ("sim-win-%s", test->name);

                MakefileTable_MakeCallback (NULL, makeName);
            }
            break;
        case BalsaProjectEntry_File:
            {
                char *makeName = ConvertToPathedBreezeFilename (BALSA_FILE (ProjectFileViewSelectedEntry->data)->name);

                MakefileTable_MakeCallback (NULL, makeName);
            }
            break;
        default:;
        }
    }
    return FALSE;
}

PtrBalsaProcedure NewBalsaProcedure (char *name, char *associated_filename)
{
    PtrBalsaProcedure proc = (PtrBalsaProcedure) g_malloc (sizeof (BalsaProcedure));

    proc->name = name;
    proc->associated_filename = associated_filename;
    proc->treeNode = NULL;
    return proc;
}

GList *FindProceduresInBalsaFile (char *filename)
{
    FILE *f = fopen (ConvertToPathedBalsaFilename (filename), "r");
    GList *procedures = NULL;

    if (!f)
        return NULL;

    while (!feof (f))
    {
        char buf[1000];
        int keyword_size;

        if (!fgets (buf, 1000, f))
            continue;

        if (!strncmp (buf, "procedure ", keyword_size = 10) || !strncmp (buf, "function ", keyword_size = 9))
        {
            char *procname = GetFirstWord (buf + keyword_size);

            procedures = g_list_append (procedures, NewBalsaProcedure (procname, g_strdup (filename)));
        }
    }

    fclose (f);
    return procedures;
}

GList *FindProceduresInBreezeFile (char *filename)
{
    GList *procedures = NULL;
    GList *tmp;
    struct BreezeFile *breezeFile = breezeInitParse (GetWithoutExtension (filename));

    if (breezeFile)
    {
        GList *parts = getBreezeParts (breezeFile);

        for (tmp = parts; tmp; tmp = g_list_next (tmp))
        {
            struct BreezePart *part = tmp->data;
            char *proc_name = g_strdup (getBreezePartName (part));

            procedures = g_list_append (procedures, NewBalsaProcedure (proc_name, g_strdup (filename)));
        }
    }

    return procedures;
}

static GList *FindFileProcedures (char *filename)
{
    GList *proceduresInBreezeFile = FindProceduresInBreezeFile (filename);

    if (proceduresInBreezeFile == NULL)
    {
        GList *proceduresInBalsaFile = FindProceduresInBalsaFile (filename);

        return proceduresInBalsaFile;
    } else
        return proceduresInBreezeFile;
}

GPtrArray *FindTestPortsForProcedureInBreezeFile (char *filename, char *procedureName)
{
    GPtrArray *testPorts = NULL;
    GList *tmp;
    struct BreezeFile *breezeFile = breezeInitParse (GetWithoutExtension (ConvertToPathedBalsaFilename (filename)));

    if (breezeFile)
    {
        struct BreezePart *part = getBreezePartByName (breezeFile, procedureName);

        if (part)
        {
            GList *ports = getBreezePartPortsList (part);

            testPorts = g_ptr_array_new ();

            for (tmp = ports; tmp; tmp = g_list_next (tmp))
            {
                struct BreezePartPortsPort *port = tmp->data;
                char *name = getBreezePartPortsPortName (port);
                enum BreezePartPortsPortType type = getBreezePartPortsPortType (port);
                BalsaPort2Nature portNature;
                PtrBalsaPort2 newPort;

                switch (type)
                {
                case PassiveSyncPort:
                case ActiveSyncPort:
                    portNature = BalsaPort2_Sync;
                    break;
                case PassiveOutputPort:
                case ActiveOutputPort:
                    portNature = BalsaPort2_Output;
                    break;
                case PassiveInputPort:
                case ActiveInputPort:
                    portNature = BalsaPort2_Input;
                    break;
                }

                if (strcmp (name, "activate"))
                {
                    if (!getBreezePartPortsPortIsArray (port))
                    {
                        newPort = NewBalsaPort2 (portNature, name);
                        g_ptr_array_add (testPorts, newPort);
                    } else
                    {
                        int low = getBreezePartPortsPortArrayIndexLow (port);
                        int size = getBreezePartPortsPortArraySize (port);
                        int i;

                        for (i = low; i < low+size; i++)
                        {
                            newPort = NewBalsaPort2 (portNature, g_strdup_printf ("%s[%d]", name, i));
                            g_ptr_array_add (testPorts, newPort);
                        }
                    }
                }
            }
        }
    }

    return testPorts;
}

GPtrArray *FindTestPortsForProcedure (char *filename, char *procedureName)
{
    GPtrArray *testPortsForProceduresInBreezeFile = FindTestPortsForProcedureInBreezeFile (filename,
      procedureName);

    /*
       GList *testPortsForProceduresInBalsaFile = FindTestPortsForProcedureInBalsaFile(filename, procedureName);

       if (testPortsForProceduresInSBreezeFile==NULL)
       return testPortsForProceduresInBalsaFile;

       if (g_list_length(testPortsForProceduresInBalsaFile) != g_list_length(testPortsForProceduresInSBreezeFile))
       {
       printf("Warning in the extraction of test ports names for procedure %s in %s.balsa and .breeze files\n", procedureName, filename);
       }
     */
    return testPortsForProceduresInBreezeFile;
}

static int GetProcedureLine (char *procedureName, char *filename)
{
    int line = 0;
    FILE *f = fopen (ConvertToPathedBalsaFilename (filename), "r");

    if (!f)
    {
        printfConsole_s ("ERROR! Impossible to open %s\n", filename);
        return 0;
    }

    while (!feof (f))
    {
        char buf[1000];
        int keyword_size;

        if (!fgets (buf, 1000, f))
            continue;
        line++;

        if (!strncmp (buf, "procedure ", keyword_size = 10) || !strncmp (buf, "function ", keyword_size = 9))
            if (!strncmp (buf + keyword_size, procedureName, strlen (procedureName)))
            {
                char nextChar = buf[keyword_size + strlen (procedureName)];

                if ((nextChar == '(') || (nextChar == ' ') || (nextChar == '\n'))
                {
                    fclose (f);
                    return line;
                }
            }
    }

    fclose (f);
    return 0;
}

gboolean IsProcedureContainingTest (PtrBalsaProcedure procedure, GList * tests)
{
    for (; tests; tests = tests->next)
        if (!strcmp (procedure->name, ((PtrBalsaTest) tests->data)->procName))
            return TRUE;
    return FALSE;
}

gboolean IsFileContainingTest (PtrBalsaFile file)
{
    if (file->info.balsaFile.testFixtures)
        return TRUE;
    else
        return FALSE;
}

void AddTestToProjectFilesViewTree (PtrBalsaTest test, GtkCTreeNode * matchingNode)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");
    GtkCTreeNode *testNode;
    gchar *text[2];

    text[0] = g_strdup (test->name);
    text[1] = g_strdup ("test");
    testNode = gtk_ctree_insert_node (GTK_CTREE (TreeWidget), matchingNode, NULL, text, 4, NULL, NULL, NULL, NULL, FALSE, TRUE);

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), testNode, (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_Test, (gpointer) test));

    UpdateTestFixtureCTreeNode (GTK_CTREE (TreeWidget), testNode);
}

void AddTestsToProjectFilesViewTree (GList * tests, GList * procedures)
{
    for (; tests; tests = tests->next)
    {
        PtrBalsaTest test = (PtrBalsaTest) tests->data;

        /* Find the procedure which is matching the test */
        GList *matchingProc;
        GtkCTreeNode *matchingNode;

        for (matchingProc = procedures; matchingProc; matchingProc = matchingProc->next)
            if (!strcmp (((PtrBalsaProcedure) matchingProc->data)->name, test->procName))
                break;

        if (matchingProc)
            matchingNode = ((PtrBalsaProcedure) matchingProc->data)->treeNode;
        else
            matchingNode = NULL;

        /* Add the test into the tree */
        AddTestToProjectFilesViewTree (test, matchingNode);
    }
}
void AddLardTestsToProjectFilesViewTree (GtkCTreeNode * nodeParent, GList * tests)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");
    gchar *text[2];
    GtkCTreeNode *matchingNode;

    for (; tests; tests = tests->next)
    {
        PtrLardTestFile test = (PtrLardTestFile) tests->data;

        text[0] = g_strdup (test->name);
        text[1] = GetWithoutPath (test->filename);

        matchingNode =
          gtk_ctree_insert_node (GTK_CTREE (TreeWidget),
          nodeParent, NULL, text, 4, LardTestFilePixmap, LardTestFileMask, LardTestFilePixmap, LardTestFileMask, FALSE, TRUE);

        gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), matchingNode, (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_LardTestFile, test));
    }
}
void AddImplementationsToProjectFilesViewTree (GtkCTreeNode * nodeParent, GList * implementations)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");
    gchar *text[2];
    GtkCTreeNode *matchingNode;

    for (; implementations; implementations = implementations->next)
    {
        PtrImplementation impl = (PtrImplementation) implementations->data;

        text[0] = g_strdup (impl->name);
        text[1] = g_strdup ("");

        matchingNode =
          gtk_ctree_insert_node (GTK_CTREE (TreeWidget),
          nodeParent, NULL, text, 4, ImplementationPixmap, ImplementationMask, ImplementationPixmap, ImplementationMask, FALSE, TRUE);

        gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), matchingNode, (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_Implementation, impl));
    }
}

void AddBuiltinLibToProjectFilesViewTree (GtkCTreeNode * nodeParent, PtrBuiltinLib lib)
{
    if (!lib)
        return;

    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");
    gchar *text[2];
    GtkCTreeNode *matchingNode;

    text[0] = g_strdup (lib->name);
    text[1] = g_strdup ("builtin lib");

    matchingNode =
      gtk_ctree_insert_node (GTK_CTREE (TreeWidget),
      nodeParent, NULL, text, 4, BuiltinLibPixmap, BuiltinLibMask, BuiltinLibPixmap, BuiltinLibMask, FALSE, TRUE);

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), matchingNode, (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_BuiltinLib, lib));

    GList *tmp = lib->sourceFilenames;

    for (; tmp; tmp = tmp->next)
    {
        char *sourceFilename = tmp->data;

        text[0] = sourceFilename;
        text[1] = "";
        GtkCTreeNode *filenameNode = gtk_ctree_insert_node (GTK_CTREE (TreeWidget), matchingNode, NULL,
          text, 4, NULL, NULL, NULL, NULL, TRUE,
          TRUE);

        gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), filenameNode,
          (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_BuiltinLib, (gpointer) lib));
    }

    gtk_ctree_expand (GTK_CTREE (TreeWidget), matchingNode);
}

void AddProcedureToProjectFilesViewTree (GtkCTreeNode * nodeParent, PtrBalsaProcedure procedure)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");
    gchar *text[2];
    GtkCTreeNode *matchingNode;

    //  char *filename = file->name;
    //  char *prefix = FindPrefixPath (filename, CurrentBalsaProject->importPath);

    text[0] = g_strdup (procedure->name);
    text[1] = g_strdup ("procedure");
    // todo: display the right icon
    matchingNode =
      gtk_ctree_insert_node (GTK_CTREE (TreeWidget), nodeParent,
      NULL, text, 4, ProcedurePixmap, ProcedureMask, ProcedurePixmap, ProcedureMask, FALSE, TRUE);
    procedure->treeNode = matchingNode;

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), matchingNode, (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_Procedure, procedure));
}

void AddFileToProjectFilesViewTree (PtrBalsaFile file)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");
    GtkCTreeNode *nodeParent = NULL;
    gchar *text[2];
    GtkCTreeNode *matchingNode;
    GList *procedures, *procedure;

    //    char *filename = GetWithoutExtension (GetWithoutPath (file->name));
    char *filename = GetWithoutExtension (file->name);
    char *filename_balsa = ConvertToBalsaFilename (file->name);
    char *filename_breeze = ConvertToPathedBreezeFilename (file->name);
    char *prefix = FindImportPathForFile (filename_balsa);

    //  printf("AddFileToProjectFilesViewTree of %s (%s,%s)\n", file->name, filename, prefix);
    if (!prefix)
        printfConsole_s
          ("Oops... The specified file (%s) doesn't appear in any imported path. We continue anyway, but you'll have to take care of that\n",
          file->name);

    text[0] = g_strdup (filename_balsa ? filename_balsa : "---"); //todo:change the ---
    text[1] = g_strdup (prefix ? prefix : "---"); //todo:change the ---
    // todo: display the right icon
    //   see   AddFileToProject (CurrentBalsaProject, BALSA_FILE (files->data));
    if (doesFileExist (filename_breeze))
        matchingNode =
          gtk_ctree_insert_node (GTK_CTREE (TreeWidget), nodeParent,
          NULL, text, 4, BalsaBlockPixmap, BalsaBlockMask, BalsaBlockPixmap, BalsaBlockMask, FALSE, TRUE);
    else
        matchingNode =
          gtk_ctree_insert_node (GTK_CTREE (TreeWidget), nodeParent,
          NULL, text, 4, BalsaBlockWithExclamationPixmap,
          BalsaBlockWithExclamationMask, BalsaBlockWithExclamationPixmap, BalsaBlockWithExclamationMask, FALSE, TRUE);

    gtk_ctree_node_set_row_data (GTK_CTREE (TreeWidget), matchingNode, (gpointer) NewBalsaProjectEntry (BalsaProjectEntry_File, file));

    /* Add procedures to the tree */
    procedures = FindFileProcedures (filename);
    for (procedure = procedures; procedure; procedure = procedure->next)
        if ((ViewProceduresOption == 1) || IsProcedureContainingTest (procedure->data, file->info.balsaFile.testFixtures))
            AddProcedureToProjectFilesViewTree (matchingNode, procedure->data);

    /* Insert the tests into the tree */
    if (file->nature == BalsaFile_File)
        if (ViewTestsOption == 1)
        {
            AddTestsToProjectFilesViewTree (file->info.balsaFile.testFixtures, procedures);
            // AddTestsToProjectFileTree (project, matchingNode, file->info.balsaFile.testFixtures);
            AddBuiltinLibToProjectFilesViewTree (matchingNode, file->info.balsaFile.builtinLib);
            AddLardTestsToProjectFilesViewTree (matchingNode, file->info.balsaFile.lardTestFiles);
            AddImplementationsToProjectFilesViewTree (matchingNode, file->info.balsaFile.implementations);
        }
}

void UpdateProjectName (void)
{
    // todo: free the old label
    GtkWidget *ProjectsFrame = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "ProjectsFrame");

    gtk_frame_set_label (GTK_FRAME (ProjectsFrame),
      g_strdup_printf ("Project %c %s", (CurrentBalsaProject->dirty || CurrentBalsaProject->nb_dirty_files) ? '*' : '-', CurrentBalsaProject->name));
}

int compare_ptrbalsafile_names (PtrBalsaFile file1, PtrBalsaFile file2)
{
    return strcmp (file1->name, file2->name);
}

/* Add files to file view */
void UpdateProjectTreeFilesView (void)
{
    GtkWidget *TreeWidget = (GtkWidget *) gtk_object_get_data (MainWindowObject,
      "TreeFilesView");

    gtk_clist_freeze (GTK_CLIST (TreeWidget));

    /* Remember the scrollbar's position */
    GtkWidget *scrolledWindow = (GtkWidget *) gtk_object_get_data (MainWindowObject, "TreeFilesScrolledWindow");
    GtkAdjustment *vadjustment = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (scrolledWindow));
    gfloat vposition = vadjustment->value;

    /* Clear the tree */
    GtkCList *list = GTK_CLIST (TreeWidget);
    int rowCount = list->rows;
    int i;

    for (i = 0; i < rowCount; i++)
    {
        PtrBalsaProjectEntry entry = BALSA_PROJECT_ENTRY (gtk_clist_get_row_data (list, i));

        if (entry)
        {
            /* make sure we don't kill the contents */
            entry->data = NULL;
            g_free (entry);
        }
    }
    gtk_clist_clear (list);

    /* Sort the file list */
    {
        CurrentBalsaProject->files = g_list_sort (CurrentBalsaProject->files, (GCompareFunc) compare_ptrbalsafile_names);
    }

    /* Add files to the tree */
    {
        GList *files;

        for (files = CurrentBalsaProject->files; files; files = files->next)
            if ((ViewFilesOption == 1) || IsFileContainingTest (files->data))
                AddFileToProjectFilesViewTree (files->data);
    }

    gtk_clist_thaw (GTK_CLIST (TreeWidget));

    /* Restore the scrollbar's position */
    gtk_adjustment_set_value (vadjustment, vposition);

    /* TODO: re-select the right element in the tree */
    ProjectFileViewSelectedEntry = NULL;
    UpdateMainWindowGreying ();
}

void UpdateProjectTrees (void)
{
    UpdateProjectName ();
    UpdateProjectTreeFilesView ();

    {
        GtkWidget *ProjectNotebook = (GtkWidget *) gtk_object_get_data (MainWindowObject,
          "ProjectNotebook");
        int page = gtk_notebook_get_current_page (GTK_NOTEBOOK (ProjectNotebook));

        if (page == 2)
            UpdateMakefileTable ();
    }
}

void SaveAllFilesOfCurrentProject (void)
{
    GList *files = CurrentBalsaProject->files;

    for (; files; files = files->next)
    {
        PtrBalsaFile file = files->data;

        FileManager_SaveFileIfOpenedAndIfNeeded (ConvertToPathedBalsaFilename (file->name));
    }
}

/*******************************************************************************/

/*******************************************************************************/

/*******************************************************************************/

static gboolean OpenProjectCallback (char *directory, gpointer user_data)
{
    PtrBalsaProject project = ReadBalsaProjectFile (directory);

    if (project)
    {
        SetAsCurrentBalsaProject (project);

        UpdateProjectTrees ();
        //      UpdateMakefileList();
        AddRecentProject (CurrentBalsaProject->directory);
        UpdateMainWindowGreying ();
        return TRUE;
    } else
    {
        printfConsole_s ("Error: Can't find the Project file %s/Project\n", directory);
        return FALSE;
    }
}
static void OpenProjectWindow_Directorychanged (GtkWidget * dialogue)
{
    char *directory = g_dirname (gtk_file_selection_get_filename (GTK_FILE_SELECTION (dialogue)));

    //printf("%s\n",directory);
    char *filename = g_strdup_printf ("%s/Project", directory);
    GtkWidget *OKButton = GTK_WIDGET (GTK_FILE_SELECTION (dialogue)->ok_button);

    FILE *f = fopen (filename, "r");

    if (f)
    {
        gtk_widget_set_sensitive (OKButton, TRUE);
        fclose (f);
    } else
    {
        gtk_widget_set_sensitive (OKButton, FALSE);
    }
}
void UponProjectMenu_Open_afterSaveConfirmation (GtkWidget * button, gpointer user_data)
{
    MakeDirectorySelectionDialogue
      ("Select the Project Directory you want to open", NULL, OpenProjectCallback, NULL, GTK_SIGNAL_FUNC (OpenProjectWindow_Directorychanged), NULL);
}

void UponProjectMenu_Open (GtkMenuItem * button, gpointer user_data)
{
    ConfirmSaveCurrentBalsaProjectAndExecute (UponProjectMenu_Open_afterSaveConfirmation);
}

void UponProjectMenu_Save (GtkMenuItem * button, gpointer user_data)
{
    if (CurrentBalsaProject->directory && *(CurrentBalsaProject->directory))
    {
        // todo:      PushStatusBarMessage (StatusBar, "Saved project file: %s", CurrentBalsaProject->directory);
        WriteBalsaProjectFile (CurrentBalsaProject->directory, CurrentBalsaProject);
        FileManager_SaveAllNamedFiles (); //      SaveAllFilesOfCurrentProject();
        AddRecentProject (CurrentBalsaProject->directory);
        UpdateProjectTrees ();
    } else
        UponProjectMenu_SaveAs (button, user_data);
}

gboolean OpenProjectMenu_SaveAs_Callback (char *directory, gpointer user_data)
{
    CurrentBalsaProject->directory = g_strdup (directory);
    UponProjectMenu_Save (NULL, NULL);
    FileManager_RefreshAllDisplayedNames ();
    return TRUE;
}

void UponProjectMenu_SaveAs (GtkMenuItem * button, gpointer user_data)
{
    MakeDirectorySelectionDialogue
      ("Select the directory where you want to save the Project", NULL, OpenProjectMenu_SaveAs_Callback, NULL, NULL, NULL);
}

void UponProjectMenu_Close_afterSaveConfirmation (GtkWidget * button, gpointer user_data)
{
    CloseCurrentBalsaProject ();
}

void UponProjectMenu_Close (GtkMenuItem * button, gpointer user_data)
{
    ConfirmSaveCurrentBalsaProjectAndExecute (UponProjectMenu_Close_afterSaveConfirmation);
}

char *generate_balsamd_command (void)
{
    char *cmd = g_strdup_printf ("%s -b -p \"%s\"",
      (CurrentBalsaProject->simulationSystem == breezesim) ? "balsa-make-makefile" : "balsa-md",
      CurrentBalsaProject->directory);

/*
    if (CurrentBalsaProject->B2LOPTS && *CurrentBalsaProject->B2LOPTS)
        cmd = g_strdup_printf ("%s -D B2LOPTS \"%s\"", cmd, CurrentBalsaProject->B2LOPTS);

    if (CurrentBalsaProject->BALSACOPTS && *CurrentBalsaProject->BALSACOPTS)
        cmd = g_strdup_printf ("%s -D BALSACOPTS \"%s\"", cmd, CurrentBalsaProject->BALSACOPTS);

    if (CurrentBalsaProject->LCDOPTS && *CurrentBalsaProject->LCDOPTS)
        cmd = g_strdup_printf ("%s -D LCDOPTS \"%s\"", cmd, CurrentBalsaProject->LCDOPTS);
*/

    return cmd;
}

void UponBuildMenu_BuildMakefile (GtkMenuItem * button, gpointer user_data)
{
    GList *commandsList;
    char *command = generate_balsamd_command ();

    commandsList = g_list_append (NULL, command);
    ExecutionManager_RunCommandList (commandsList);
}

void UponProjectMenu_AddFilesIntoProject (GtkMenuItem * button, gpointer user_data)
{
    UponProjectMenu_AddFile (NULL, NULL);
}

void UponProjectMenu_Quit_afterSaveConfirmation (GtkWidget * button, gpointer user_data)
{
    UponExecutionWindowDelete (NULL, NULL, (gpointer) 1);
    //    gtk_main_quit ();
}

void UponProjectMenu_Quit (GtkMenuItem * button, gpointer user_data)
{
    ConfirmSaveCurrentBalsaProjectAndExecute (UponProjectMenu_Quit_afterSaveConfirmation);
}

void UponButton_BuildMakefile_clicked (GtkButton * button, gpointer user_data)
{
    UponBuildMenu_BuildMakefile (NULL, NULL);
}

void UponMakefileList_SelectChild (GtkList * list, GtkWidget * widget, gpointer user_data)
{
    //  printf("%x %x %x\n",list, widget, user_data);
}

void UponProjectToolbar_LaunchEditor (GtkMenuItem * button, gpointer user_data)
{
    /* Run the editor */
    char *args[5] = { NULL, NULL, NULL, NULL };

    if (EnvironmentOptions.editor && EnvironmentOptions.editor[0])
        args[0] = EnvironmentOptions.editor;
    else
    {
        char *editor = getenv ("EDITOR");

        if (editor)
            args[0] = g_strdup_printf ("xterm -e %s", editor);
        else
            args[0] = g_strdup ("emacs");
    }

    {
        switch (ProjectFileViewSelectedEntry->nature)
        {
        case BalsaProjectEntry_File:
            if (BALSA_FILE (ProjectFileViewSelectedEntry->data)->nature == BalsaFile_File)
                args[1] = ConvertToPathedBalsaFilename (BALSA_FILE (ProjectFileViewSelectedEntry->data)->name);
            break;

        case BalsaProjectEntry_Procedure:
            {
                char *procedure_name = BALSA_PROCEDURE (ProjectFileViewSelectedEntry->data)->name;
                char *filename = ConvertToPathedBalsaFilename (BALSA_PROCEDURE (ProjectFileViewSelectedEntry->data)->associated_filename);
                int line = GetProcedureLine (procedure_name,
                  filename);

                args[1] = g_strdup_printf ("+%d", line);
                args[2] = filename;
            }
            break;

        case BalsaProjectEntry_Test:
        case BalsaProjectEntry_TestComponent:
            UponSelectionMenu_Edit (NULL, NULL);
            break;

        case BalsaProjectEntry_LardTestFile:
            args[1] = LARDTEST_FILE (ProjectFileViewSelectedEntry->data)->filename;
            break;

        default:
            break;
        }
    }

    if (args[1])
    {
        /*
           args[3] = args[1];
           args[2] = args[0];
           args[1] = "-e";
           args[0] = "xterm";
         */

      if (chdir (CurrentBalsaProject->directory) == -1) {
	// TODO: report error
      }
        RunCommandWithoutOutput (args[0], args);
    }
}

void UponSelectionMenu_GetInfo (GtkMenuItem * button, gpointer user_data)
{
    //TODO
    PtrBalsaProjectEntry entry = ProjectFileViewSelectedEntry;

    switch (entry->nature)
    {
    case BalsaProjectEntry_File:
        break;
    case BalsaProjectEntry_Directory:
        break;
    case BalsaProjectEntry_Procedure:
        break;
    case BalsaProjectEntry_Test:
        break;
    case BalsaProjectEntry_TestComponent:
        break;
    default:
        break;
    }
}

gboolean UponGeneralKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    /*
       if ( ProjectOptionsDialogue == NULL )
       return FALSE;

       if (event->keyval == GDK_Return)
       {
       GtkWidget *nextWidget;

       switch ((int) user_data)
       {
       case 0:
       nextWidget = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (ProjectOptionsDialogue), "ProjectDirectoryEntry");
       break;
       default:
       nextWidget = (GtkWidget *) gtk_object_get_data (GTK_OBJECT (ProjectOptionsDialogue), "OKbutton");
       }

       gtk_window_set_focus (GTK_WINDOW (ProjectOptionsDialogue || ProjectOptionsDialogue_NewProject), nextWidget);
       } else
       if ( event->keyval == GDK_Escape ) {
       UponProjectOptionsDialogueCancel( GTK_OBJECT
       ( ProjectOptionsDialogue ) );
       }
     */

    return FALSE;
}

gboolean disable_filenameEntry = FALSE;
void UponTestFilenameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    if (disable_filenameEntry)
        return;

    //    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
    //    GtkCombo *procNameCombo = GTK_COMBO (gtk_object_get_data (GTK_OBJECT (dialogue), "TestProcNameCombo"));

    /* Fill the ProcNames combo box */
    {
        GList *procNames = NULL;
        char *selectedFilename = gtk_entry_get_text (GTK_ENTRY (editable));
        GList *procedures = FindFileProcedures (selectedFilename);

        for (; procedures; procedures = procedures->next)
            procNames = g_list_append (procNames, g_strdup (BALSA_PROCEDURE (procedures->data)->name));

        if (!procNames)
            procNames = g_list_append (procNames, g_strdup ("<no procedures>"));

        //        gtk_combo_set_popdown_strings (procNameCombo, procNames);
    }
    /* Select the right procName in the combo box => it will call UponTestProcNameEntryChanged(...) */
    {
    }
}

void UponTestProcNameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (editable));
    GtkCList *testPortList = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue), "TestPortList"));
    GtkEntry *filenameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestFilenameEntry"));

    /* Clear the TestPort list */
//    gtk_clist_clear (testPortList);

    /* Fill the TestPort list */
    char *selectedFilename = gtk_entry_get_text (filenameEntry);
    char *selectedProcName = gtk_entry_get_text (GTK_ENTRY (editable));

    /*
       GPtrArray *testPorts = FindTestPortsForProcedure (selectedFilename,
       selectedProcName);
     */
    GtkButton *breezeWarningButton = GTK_BUTTON (gtk_object_get_data (GTK_OBJECT (dialogue),
        "CompileSBreezeFileButton"));

    if (doesFileExist (ConvertToPathedBreezeFilename (selectedFilename)))
        gtk_widget_hide (GTK_WIDGET (breezeWarningButton));
    else                        // no .breeze file
        gtk_widget_show (GTK_WIDGET (breezeWarningButton));

    // Update the ports
    CreateNewComponents (testPortList, selectedFilename, selectedProcName);

    /*
       {
       PtrBalsaTest editedTest = (PtrBalsaTest) gtk_object_get_data (GTK_OBJECT (dialogue), "EditedTest");

       if (editedTest)
       {
       GList *testPort = editedTest->testComponents;
       GtkCList *list = GTK_CLIST (gtk_object_get_data (GTK_OBJECT (dialogue),
       "TestPortList"));

       for (; testPort; testPort = testPort->next)
       ChangePortInTestPortList (BALSA_PORT (testPort->data), list);
       //AddTestComponentToTestComponentList (list, testPort->data, 0, FALSE);
       }
       }
     */
}

void UponSBreezeWarningButtonClicked (GtkWidget * widget, gpointer data)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (widget));
    GtkEntry *filenameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestFilenameEntry"));
    GtkEntry *procNameEntry = GTK_ENTRY (gtk_object_get_data (GTK_OBJECT (dialogue), "TestProcNameEntry"));
    char *filename = gtk_entry_get_text (filenameEntry);
    char *breezeFilename = ConvertToPathedBreezeFilename (filename);

    GList *commandList = NULL;

    /* Check if the Makefile exists */
    /*
       {
       FILE *f = fopen ("Makefile", "r");
       if (f)
       fclose (f);
       else
       {
       char *command = generate_balsamd_command ();
       commandList = g_list_append (NULL, command);
       }
       }
     */

    /* Execute the appropriate make function */
    char *firstCommand = g_strdup_printf ("make -n %s", breezeFilename);
    char *nextCommand = g_strdup_printf ("~ExecuteFunction2args %p %p %p",
      UponTestProcNameEntryChanged,
      GTK_EDITABLE (procNameEntry), NULL);

    commandList = g_list_append (commandList, firstCommand);
    commandList = g_list_append (commandList, nextCommand);

    ExecutionManager_CheckBuildMakefileAndRunCommandList (commandList);

    /*
       char *args[3];

       args[0] = "make";
       args[1] = ConvertToBreezeFilename(filename);
       args[2] = NULL;

       chdir (CurrentBalsaProject->directory);
       RunCommand ("make", args);

       // update the arguments list
       UponTestProcNameEntryChanged( GTK_EDITABLE(procNameEntry), NULL);
     */
}

int FileDialogFindNextBalsaFilenameRow (GList * row_list, char *filename, int direction)
{
    // Find the row corresponding to the specified filename (if exists)
    GList *row_elt, *row_elt_found;

    for (row_elt = row_list; row_elt; row_elt = row_elt->next)
    {
        GtkCListRow *row = GTK_CLIST_ROW (row_elt);
        GtkCell *cell = row->cell;
        char *textCell = cell->u.text;

        if (!strcmp (textCell, filename))
            break;
    }
    if (!row_elt)
        row_elt = row_list;
    row_elt_found = row_elt;

    // Find the next .balsa filename
    for (;;)
    {
        char *text;

        if (direction == 1)
        {
            row_elt = row_elt->next;
            if (row_elt == NULL)
                row_elt = row_list;
        } else
        {
            row_elt = row_elt->prev;
            if (row_elt == NULL)
                row_elt = g_list_last (row_list);
        }

        if (row_elt == row_elt_found)
            break;
        text = GTK_CLIST_ROW (row_elt)->cell->u.text;
        if (!strcmp (text + strlen (text) - 6, ".balsa"))
            break;
    }

    return g_list_index (row_list, row_elt->data);
}

gboolean UponDirectoryDialogueKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    if (event->keyval == GDK_Escape)
        UponDialogueCancel (GTK_OBJECT (widget));
    return FALSE;
}

gboolean UponFileDialogueKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    if (event->keyval == GDK_Escape)
        UponDialogueCancel (GTK_OBJECT (widget));
    else if ((event->keyval == GDK_Down) || (event->keyval == GDK_Up))
    {
        GtkWidget *dialogue = gtk_widget_get_toplevel (widget);
        GtkCList *file_list = GTK_CLIST (GTK_FILE_SELECTION (dialogue)->file_list);
        GtkEntry *selection_entry = GTK_ENTRY (GTK_FILE_SELECTION (dialogue)->selection_entry);
        char *selectedFilename = gtk_entry_get_text (selection_entry);

        //      gtk_file_selection_complete( GTK_FILE_SELECTION(dialogue), "*.balsa");

        if (file_list->row_list)
        {
            int row = FileDialogFindNextBalsaFilenameRow (file_list->row_list,
              selectedFilename,
              (event->keyval == GDK_Down) ? 1 : -1);

            gtk_clist_moveto (file_list, row, 0, 0.5, -1);
            gtk_clist_select_row (file_list, row, 0);
        }

        gtk_widget_grab_focus (GTK_WIDGET (selection_entry));
    }
    return FALSE;
}

gboolean UponConsoleWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    gtk_widget_hide (workSpace.consoleWindow->window);
    return TRUE;
}

gboolean UponAuxillaryWindow_KeyPressed (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    if (event->state & 4)       // CTRL key
    {
        if ((event->keyval == GDK_Q) || (event->keyval == GDK_q))
            UponProjectMenu_Quit (NULL, NULL);

        if ((event->keyval == GDK_C) || (event->keyval == GDK_c))
            UponViewMenu_Console_DisplayHide (NULL, NULL);

        if ((event->keyval == GDK_E) || (event->keyval == GDK_e))
            UponViewMenu_ExecutionWindow_DisplayHide (NULL, NULL);
    }
    return TRUE;
}
