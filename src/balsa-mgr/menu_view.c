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

#include "menu_view.h"
#include "main.h"
#include "workspace.h"
#include "executionmanager.h"

int ViewFilesOption = 1;        // 0=when tests ; 1=always
int ViewProceduresOption = 1;   // 0=when tests ; 1=always
int ViewTestsOption = 1;        // 0=no ; 1=yes

int ConsoleWindowViewOptions = 2; //0=Always 1=Never 2=Automatic

void UponViewMenu_ViewFilesAlways (GtkMenuItem * button, gpointer user_data)
{
    ViewFilesOption = 1;
    UpdateProjectTrees ();
}

void UponViewMenu_ViewFilesNotAlways (GtkMenuItem * button, gpointer user_data)
{
    ViewFilesOption = 0;
    UpdateProjectTrees ();
}

void UponViewMenu_ViewProceduresAlways (GtkMenuItem * button, gpointer user_data)
{
    ViewProceduresOption = 1;
    UpdateProjectTrees ();
}

void UponViewMenu_ViewProceduresNotAlways (GtkMenuItem * button, gpointer user_data)
{
    ViewProceduresOption = 0;
    UpdateProjectTrees ();
}

void UponViewMenu_ViewTestsYes (GtkMenuItem * button, gpointer user_data)
{
    ViewTestsOption = 1;
    UpdateProjectTrees ();
}

void UponViewMenu_ViewTestsNo (GtkMenuItem * button, gpointer user_data)
{
    ViewTestsOption = 0;
    UpdateProjectTrees ();
}

void UponViewMenu_Console_Always (GtkMenuItem * button, gpointer user_data)
{
    ConsoleWindowViewOptions = 0; //0=Always 1=Never 2=Automatic
    gtk_widget_show (workSpace.consoleWindow->window);
}

void UponViewMenu_Console_Never (GtkMenuItem * button, gpointer user_data)
{
    ConsoleWindowViewOptions = 1; //0=Always 1=Never 2=Automatic
    gtk_widget_hide (workSpace.consoleWindow->window);
}

void UponViewMenu_Console_Automatic (GtkMenuItem * button, gpointer user_data)
{
    ConsoleWindowViewOptions = 2; //0=Always 1=Never 2=Automatic
}

void UponViewMenu_Console_DisplayHide (GtkMenuItem * button, gpointer user_data)
{
    if (GTK_WIDGET_VISIBLE (workSpace.consoleWindow->window))
        gtk_widget_hide (workSpace.consoleWindow->window);
    else
    {
        gtk_widget_hide (workSpace.consoleWindow->window);
        gtk_widget_show (workSpace.consoleWindow->window);
    }
}
void UponViewMenu_ExecutionWindow_Always (GtkMenuItem * button, gpointer user_data)
{
    ExecutionWindowViewOptions = 0; //0=Always 1=Never 2=Automatic
    gtk_widget_show (ExecutionWindow);
}

void UponViewMenu_ExecutionWindow_Never (GtkMenuItem * button, gpointer user_data)
{
    ExecutionWindowViewOptions = 1; //0=Always 1=Never 2=Automatic
    gtk_widget_hide (ExecutionWindow);
}

void UponViewMenu_ExecutionWindow_Automatic (GtkMenuItem * button, gpointer user_data)
{
    ExecutionWindowViewOptions = 2; //0=Always 1=Never 2=Automatic
}

void UponViewMenu_ExecutionWindow_DisplayHide (GtkMenuItem * button, gpointer user_data)
{
    if (GTK_WIDGET_VISIBLE (ExecutionWindow))
        gtk_widget_hide (ExecutionWindow);
    else
    {
        gtk_widget_hide (ExecutionWindow);
        gtk_widget_show (ExecutionWindow);
    }
}
