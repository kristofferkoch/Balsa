/*
  The Balsa Asynchronous Hardware Synthesis System
  Copyright (C) 2002 Department of Computer Science
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

#ifndef THREADGROUPS_H
#define THREADGROUPS_H

#include <glib.h>
#include "libbreeze.h"

struct ThreadGroupsItem
{
    char *name;
    GPtrArray *comps;           // list of struct Comp
    //    GList *chans; // list of struct Chan

    void *data;
    gboolean processed;
};

struct BreezePart;
void InitBreezePartThreadGroups (struct BreezePart *part);

void ThreadGroups_Foreach (struct BreezePart *part, GHFunc func, gpointer user_data);

#endif
