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

#include <stdlib.h>
#include <unistd.h>

#include "menu_file.h"
#include "filemanager.h"
#include "file.h"
#include "paths.h"
#include "main.h"
#include "commands.h"

void UponFileMenu_New (GtkMenuItem * button, gpointer user_data)
{
    FileManager_NewFile ();
}

void UponFileMenu_Open (GtkMenuItem * button, gpointer user_data)
{
    FileManager_OpenFile (NULL);
}

void UponFileMenu_ReOpen (GtkMenuItem * button, gpointer user_data)
{
    FileManager_ReOpenFile ();
}

void UponFileMenu_Save (GtkMenuItem * button, gpointer user_data)
{
    FileManager_SaveFile ();
}

void UponFileMenu_SaveAs (GtkMenuItem * button, gpointer user_data)
{
    FileManager_SaveFileAs ();
}

void UponFileMenu_Close (GtkMenuItem * button, gpointer user_data)
{
    FileManager_CloseFile ();
}

void UponFileMenu_CloseAll (GtkMenuItem * button, gpointer user_data)
{
    FileManager_CloseAllFiles ();
}

void UponFileMenu_AddCurrentFileToProject (GtkMenuItem * button, gpointer user_data)
{
    char *filename = FileManager_GetCurrentFileName ();

    if (filename)
    {
        PtrBalsaFile newFile = NewBalsaFile ();

        newFile->name = GetWithoutPath (GetWithoutExtension (filename));
        AddFileToProject (CurrentBalsaProject, newFile, NULL); //todo: maybe change the NULL into the complete path+name, if the file CAN be absent from the imported paths.
    }
}

void UponFileMenu_LaunchEditor (GtkMenuItem * button, gpointer user_data)
{
    char *filename = FileManager_GetCurrentFileName ();

    if (filename)
    {
        int line = FileManager_GetCurrentLineNumber ();
        char *args[4] = { NULL, NULL, NULL, NULL };

        if (EnvironmentOptions.editor && EnvironmentOptions.editor[0])
            args[0] = EnvironmentOptions.editor;
        else
        {
            char *editor;

            if ((editor = (char *) getenv ("EDITOR")) != NULL)
                args[0] = editor;
            else
                args[0] = g_strdup ("emacs");
        }
        if (line > 0)
        {
            args[1] = g_strdup_printf ("+%d", line);
            args[2] = filename;
        } else
            args[1] = filename;

        if (CurrentBalsaProject)
            chdir (CurrentBalsaProject->directory);
        RunCommandWithoutOutput (args[0], args);
    }
}

void UponFileMenu_Print (GtkMenuItem * button, gpointer user_data)
{
    char *filename = FileManager_GetCurrentFileName ();

    if (filename)
    {
//        int line = FileManager_GetCurrentLineNumber ();
        char *args[4] = { NULL, NULL, NULL, NULL };

        if (EnvironmentOptions.PrintCommand && EnvironmentOptions.PrintCommand[0])
            args[0] = EnvironmentOptions.PrintCommand;
        else
            args[0] = g_strdup ("lpr");

        args[1] = filename;

        if (CurrentBalsaProject)
            chdir (CurrentBalsaProject->directory);
        RunCommandWithoutOutput (args[0], args);
    }
}
