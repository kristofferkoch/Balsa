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

	`commands.c'
	Wrapper for calling external progs/logging std{out,err}

*/

#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>
#include <glib.h>

#include "buffer.h"
#include "main.h"
#include "widgets.h"
#include "utils.h"

char *NullCommandArguments[] = { NULL };

void LogCommandInTextBox (char *command, char **args, GtkText * textBox)
{
    gtk_text_insert (textBox, MessagesFont, &Green, NULL, command, -1);

    while (*args)
    {
        gtk_text_insert (textBox, MessagesFont, &Green, NULL, " ", -1);
        gtk_text_insert (textBox, MessagesFont, &Green, NULL, *args, -1);
        args++;
    }
    gtk_text_insert (textBox, MessagesFont, &Green, NULL, "\n", -1);
}

void LogCommand (char *command, char **args)
{
    //  LogCommandInTextBox( command, args, GTK_TEXT (MessagesText));
    printfConsole (command);
    while (*args)
    {
        printfConsole_nobringtofront (" ");
        printfConsole_nobringtofront (*args);
        args++;
    }
    printfConsole ("\n");
}

void LogLine (char *line, gboolean red, gboolean inBuffer, GtkText * gtkText)
{
    gtk_text_insert (gtkText, MessagesFont, (red ? &Red : NULL), NULL, line, -1);
    gtk_text_insert (gtkText, MessagesFont, (red ? &Red : NULL), NULL, "\n", -1);
}

/* LogStreams : log line messages on the given stdout/stderr streams
	to the messages window */
void LogStreams (int stdoutFD, int stderrFD, GtkText * gtkText)
{
    ReadBuffer stdoutBuff, stderrBuff;
    char *stdoutLine = NULL;
    char *stderrLine = NULL;
    int somethingWritten = 0;

    InitReadBuffer (&stdoutBuff);
    InitReadBuffer (&stderrBuff);

    do
    {
        /* Try to read from STDERR */
        stderrLine = ReadLineUsingReadBuffer (stderrFD, &stderrBuff, NULL);
        if (stderrLine)
        {
            char *nextLine;

            somethingWritten = 1;
            LogLine (stderrLine, TRUE, FALSE, gtkText);
            do
            {
                nextLine = ReturnLineFromReadBuffer (&stderrBuff);
                if (nextLine)
                    LogLine (nextLine, TRUE, TRUE, gtkText);
            }
            while (nextLine);
        }

        /* Try to read from STDOUT */
        stdoutLine = ReadLineUsingReadBuffer (stdoutFD, &stdoutBuff, NULL);
        if (stdoutLine)
        {
            char *nextLine;

            somethingWritten = 1;
            LogLine (stdoutLine, FALSE, FALSE, gtkText);
            do
            {
                nextLine = ReturnLineFromReadBuffer (&stdoutBuff);
                if (nextLine)
                    LogLine (nextLine, FALSE, TRUE, gtkText);
            }
            while (nextLine);
        }
    }
    while (stdoutLine || stderrLine);

    if (!somethingWritten)
        gtk_text_insert (gtkText, MessagesFont, &Red, NULL,
          "Error! (execution completed without output. Maybe the executable is not in the PATH)", -1);
}

void RunCommandWithoutOutput (char *command, char **args)
{
    /* FIXME check command */
    int childPid;               // outPipe[2], errPipe[2];

    LogCommand (command, args + 1);

    childPid = fork ();

    if (childPid == 0)
    {                           /* in the child process */
        /*
           execvp (command, args);
         */
        char *comm = g_strjoinv (" ", args);

        system (comm);
        free (comm);
        _exit (0);
    }
}
