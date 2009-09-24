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

#include "aboutdialog.h"
#include "workspace.h"
#include "widgets.h"

struct _AboutDialog *new_aboutDialog (void)
{
    struct _AboutDialog *obj = (struct _AboutDialog *) malloc (sizeof (struct _AboutDialog));

    obj->dialog = NULL;

    return obj;
}

GtkWidget *get_workspace_aboutDialog_dialog (void)
{
    if (workSpace.aboutDialog->dialog == NULL)
    {
        GtkWidget *dialog;
        GtkLabel *VersionLabel;
        GtkLabel *DateLabel;

        dialog = create_About_dialog ();
        VersionLabel = GTK_LABEL (gtk_object_get_data (GTK_OBJECT (dialog), "VersionLabel"));
        DateLabel = GTK_LABEL (gtk_object_get_data (GTK_OBJECT (dialog), "DateLabel"));
        gtk_label_set_text (VersionLabel, BALSA_MGR_VERSION_NUMBER);
        gtk_label_set_text (DateLabel, BALSA_MGR_VERSION_DATE);

        workSpace.aboutDialog->dialog = dialog;
    }

    return workSpace.aboutDialog->dialog;
}
