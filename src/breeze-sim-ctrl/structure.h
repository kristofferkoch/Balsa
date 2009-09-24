/*
  The Balsa Asynchronous Hardware Synthesis System
  Copyright (C) 2002 Amulet Group, Department of Computer Science
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

#ifndef STRUCTURE_H
#define STRUCTURE_H

#include <gtk/gtk.h>
#include <breeze/libbreeze.h>

typedef struct EdgeInfo
{
    struct Chan *chan;
    GPtrArray *trace;           // array of ChanTraceItem (declared in simtrace.h)
    int posInTrace;
    GPtrArray *valueTrace;      // array of ChanValueTraceItem (declared in simtrace.h)
    int posInValueTrace;
}
EdgeInfo, *PtrEdgeInfo;

struct CallcontextTreeData
{
    struct Vertex *vertex;
};

extern GHashTable *callcontexts_hashtable;
extern struct BreezePart *projectBreezePart;

void InitStructure (void);

void Structure_Init (void);
struct BreezePart *Structure_GetFlattenedBreezePart (void);
struct Vertex *Structure_GetRootVertex (void);

#endif
