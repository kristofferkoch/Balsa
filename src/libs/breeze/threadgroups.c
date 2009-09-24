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

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "threadgroups.h"
#include "libbreeze.h"
#include "breezestructs.h"
#include "callcontexttree.h"

//#define DEBUG
#ifdef DEBUG
#include <signal.h>
#define ASSERT(x) {if (!(x)) {fprintf(stderr,"ASSERT failed\n"); raise(SIGUSR1);}}
#else
#define ASSERT(x)
#endif

 /*
    static void ReadChans (struct BreezePart *breezePart)
    {
    GList *channelList = getBreezePartChannelsList (breezePart);
    GList *tmp;
    allChans = g_ptr_array_new ();
    g_ptr_array_add (allChans, NULL); // no channel 0

    int num = 1;
    for (tmp=g_list_last(channelList); tmp; tmp=tmp->prev, num++) {
    struct BreezePartChannelsChannel *channel = tmp->data;
    struct Chan *chan = g_new0 (struct Chan, 1);
    chan->num = num;
    chan->channel = channel;
    struct Position *position = getBreezePartChannelsChannelPosition (channel);
    int numcontext = 0;
    if (position)
    numcontext = position->context;

    gpointer val = g_hash_table_lookup (breezePart->threadGroups, (gconstpointer)&numcontext);
    struct ThreadGroupsItem *cctitem = val;
    if (cctitem) {
    cctitem->chans = g_list_prepend (cctitem->chans, chan);
    chan->callcontext = cctitem;
    }

    g_ptr_array_add (allChans, chan);
    }
    }
  */

/*
static void ReadComps (struct BreezePart *breezePart)
{
    struct ThreadGroupsItem *threadGroup = g_new0 (struct ThreadGroupsItem, 1);
    threadGroup->name = g_strdup ("thread 1");
    threadGroup->comps = g_ptr_array_new ();
    g_ptr_array_add (breezePart->threadGroups, threadGroup);

    GList *componentList = getBreezePartComponentsList (breezePart);
    //    GList *channelList = getBreezePartChannelsList (breezePart);
    GList *tmp;
    int num = 1;
    for (tmp=g_list_last(componentList); tmp; tmp=tmp->prev, num++) {
	struct BreezePartComponentsComponent *component = tmp->data;

	struct Comp *comp = g_new0 (struct Comp, 1);
	comp->num = num;
	comp->component = component;

	PtrTMPNode compNode = getBreezePartComponentsComponentTMPNode (component);
	PtrTMPNode compChansNode = compNode->body.list->next->next->next->data;
	GList *argList = compChansNode->body.list;

	PtrTMPNode tmp = compChansNode;
	while (tmp->type == TMPList)
	    tmp = tmp->body.list->data;
	//	ASSERT (tmp->type == TMPNumberStr);
	int chanNum = atoi (tmp->body.string);
	comp->mainChannel = g_ptr_array_index (allChans, chanNum);

	// Fill comp->channels
	void RecTruc (PtrTMPNode node) {
		switch (node->type) {
		case TMPList:
			{
				GList *elt = node->body.list;
				for (; elt; elt=elt->next) {
					PtrTMPNode node2 = elt->data;
					RecTruc (node2);
				}
			}
			break;
		case TMPNumberStr:
			{
				int arg = atoi(node->body.string);
				struct Chan *chan = g_ptr_array_index (allChans, arg);
				if (!chan) {
					char *compname = getBreezePartComponentsComponentName (comp->component);
					fprintf (stderr, "error with component %s\n", compname);
					return;
				}
				comp->channels = g_list_prepend (comp->channels, chan);
			}
			break;

		default:
			{
				char *compname = getBreezePartComponentsComponentName (comp->component);
				fprintf (stderr, "oh... error with component %s\n", compname);
			}
		}
	}

	RecTruc (compChansNode);

	char *compname = getBreezePartComponentsComponentName (component);
	int mainChannelNum;
#define ELT1 atoi(((PtrTMPNode)argList->data)->body.string)
#define ELT2 atoi(((PtrTMPNode)argList->next->data)->body.string)
#define ELT3 atoi(((PtrTMPNode)argList->next->next->data)->body.string)
#define ELT4 atoi(((PtrTMPNode)argList->next->next->next->data)->body.string)
#define PASSIVE_PORT(x) struct Chan *chan=g_ptr_array_index(allChans,x); ASSERT(!chan->dest); chan->dest=comp;
#define PASSIVE_SYNC_PORT(x) { PASSIVE_PORT(x); chan->direction=ChanDirection_Sync; }
#define PASSIVE_PUSH_PORT(x) { PASSIVE_PORT(x); chan->direction=ChanDirection_Push; }
#define PASSIVE_PULL_PORT(x) { PASSIVE_PORT(x); chan->direction=ChanDirection_Pull; }
#define ACTIVE_PORT(x) struct Chan *chan=g_ptr_array_index(allChans,x); ASSERT(!chan->source); chan->source=comp;
#define ACTIVE_SYNC_PORT(x) { ACTIVE_PORT(x); }
#define ACTIVE_PUSH_PORT(x) { ACTIVE_PORT(x); }
#define ACTIVE_PULL_PORT(x) { ACTIVE_PORT(x); }

#define ELT1_1 atoi(((PtrTMPNode)((PtrTMPNode)argList->data)->body.list->data)->body.string)

#define LIST1 ((PtrTMPNode)argList->data)->body.list
#define LIST2 ((PtrTMPNode)argList->next->data)->body.list
#define LIST3 ((PtrTMPNode)argList->next->next->data)->body.list
#define LIST4 ((PtrTMPNode)argList->next->next->next->data)->body.list
#define ACTIVE_SYNC_PORT_LIST(l) { GList *tmp = l; for (; tmp; tmp=tmp->next) ((struct Chan *)g_ptr_array_index (allChans, atoi(((PtrTMPNode)tmp->data)->body.string)))->source = comp; }
#define ACTIVE_PUSH_PORT_LIST(x) { ACTIVE_SYNC_PORT_LIST(x); }
#define ACTIVE_PULL_PORT_LIST(x) { ACTIVE_SYNC_PORT_LIST(x); }
#define PASSIVE_SYNC_PORT_LIST(l) { GList *tmp = l; for (; tmp; tmp=tmp->next) { struct Chan *chan = g_ptr_array_index (allChans, atoi(((PtrTMPNode)tmp->data)->body.string)); chan->dest = comp; chan->direction=ChanDirection_Sync; } }
#define PASSIVE_PUSH_PORT_LIST(l) { GList *tmp = l; for (; tmp; tmp=tmp->next) { struct Chan *chan = g_ptr_array_index (allChans, atoi(((PtrTMPNode)tmp->data)->body.string)); chan->dest = comp; chan->direction=ChanDirection_Push; } }
#define PASSIVE_PULL_PORT_LIST(l) { GList *tmp = l; for (; tmp; tmp=tmp->next) { struct Chan *chan = g_ptr_array_index (allChans, atoi(((PtrTMPNode)tmp->data)->body.string)); chan->dest = comp; chan->direction=ChanDirection_Pull; } }

	if (!strcmp (compname, "$BrzAdapt")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzBinaryFunc")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	    ACTIVE_PULL_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzBinaryFuncPush")) {
	    mainChannelNum = ELT1;
	    ACTIVE_PUSH_PORT (ELT1);
	    PASSIVE_PUSH_PORT (ELT2);
	    PASSIVE_PUSH_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzBinaryFuncConstR")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzBinaryFuncConstRPush")) {
	    mainChannelNum = ELT1;
	    ACTIVE_PUSH_PORT (ELT1);
	    PASSIVE_PUSH_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzCall")) {
	    mainChannelNum = ELT2;
	    PASSIVE_SYNC_PORT_LIST (LIST1);
	    ACTIVE_SYNC_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzCallMux")) {
	    mainChannelNum = ELT2;
	    PASSIVE_PUSH_PORT_LIST (LIST1);
	    ACTIVE_PUSH_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzCallDemux")) {
	    mainChannelNum = ELT2;
	    PASSIVE_PULL_PORT_LIST (LIST1);
	    ACTIVE_PULL_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzCase")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    ACTIVE_SYNC_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzCaseFetch")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	    ACTIVE_PULL_PORT_LIST (LIST3);
	}
	else if (!strcmp (compname, "$BrzCombine")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	    ACTIVE_PULL_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzCombineEqual")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzConcur")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_SYNC_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzConstant")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	}
	else if (!strcmp (compname, "$BrzDecisionWait")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    PASSIVE_SYNC_PORT_LIST (LIST2);
	    ACTIVE_SYNC_PORT_LIST (LIST3);
	}
	else if (!strcmp (compname, "$BrzEncode")) {
	    mainChannelNum = ELT2;
	    PASSIVE_SYNC_PORT_LIST (LIST1);
	    ACTIVE_PUSH_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzFalseVariable")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    ACTIVE_SYNC_PORT (ELT2);
	    PASSIVE_PULL_PORT_LIST (LIST3);
	}
	else if (!strcmp (compname, "$BrzEagerFalseVariable")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    ACTIVE_SYNC_PORT (ELT2);
	    PASSIVE_PULL_PORT_LIST (LIST3);
	}
	else if (!strcmp (compname, "$BrzFetch")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	    ACTIVE_PUSH_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzLoop")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_SYNC_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzPassivatorPush")) {
	    mainChannelNum = ELT2;
	    PASSIVE_PULL_PORT_LIST (LIST1);
	    PASSIVE_PUSH_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzSequence")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_SYNC_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzSequenceOptimised")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_SYNC_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzSlice")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzSynch")) {
	    mainChannelNum = ELT2;
	    PASSIVE_SYNC_PORT_LIST (LIST1);
	    ACTIVE_SYNC_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzUnaryFunc")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PULL_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzUnaryFuncPush")) {
	    mainChannelNum = ELT1;
	    ACTIVE_PUSH_PORT (ELT1);
	    PASSIVE_PUSH_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzVariable")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    PASSIVE_PULL_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzBuiltinVariable")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    PASSIVE_PULL_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzInitVariable")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    PASSIVE_SYNC_PORT (ELT2);
	    PASSIVE_PULL_PORT_LIST (LIST3);
	}
	else if (!strcmp (compname, "$BrzContinuePush")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	}
	else if (!strcmp (compname, "$BrzForkPush")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    ACTIVE_PUSH_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzCallDemuxPush")) {
	    printf("Warning: $BrzCallDemuxPush in libbreeze/callcontexttree.c\n");
	}
	else if (!strcmp (compname, "$BrzSynchPush")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    PASSIVE_PULL_PORT_LIST (LIST2);
	    ACTIVE_PUSH_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzArbiter")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    PASSIVE_SYNC_PORT (ELT2);
	    ACTIVE_SYNC_PORT (ELT3);
	    ACTIVE_SYNC_PORT (ELT4);
	}
	else if (!strcmp (compname, "$BrzBar")) {
	    mainChannelNum = ELT2;
	    PASSIVE_PULL_PORT (ELT1);
	    PASSIVE_SYNC_PORT (ELT2);
	    ACTIVE_PULL_PORT_LIST (LIST3);
	    ACTIVE_PULL_PORT_LIST (LIST4);
	}
	else if (!strcmp (compname, "$BrzContinue")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	}
	else if (!strcmp (compname, "$BrzFork")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_SYNC_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzNullAdapt")) {
	    mainChannelNum = ELT1;
	    ACTIVE_PULL_PORT (ELT1);
	    PASSIVE_SYNC_PORT (ELT2);
	}
	else if (!strcmp (compname, "$BrzSplit")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    ACTIVE_PUSH_PORT (ELT2);
	    ACTIVE_PUSH_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzSplitEqual")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	    ACTIVE_PUSH_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzWhile")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_PULL_PORT (ELT2);
	    ACTIVE_SYNC_PORT (ELT3);
	}
	else if (!strcmp (compname, "$BrzWireFork")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	    ACTIVE_SYNC_PORT_LIST (LIST2);
	}
	else if (!strcmp (compname, "$BrzHalt")) {
	    mainChannelNum = ELT1;
	    PASSIVE_SYNC_PORT (ELT1);
	}
	else if (!strcmp (compname, "$BrzHaltPush")) {
	    mainChannelNum = ELT1;
	    PASSIVE_PUSH_PORT (ELT1);
	}
	else if (!strcmp (compname, "$BrzPassivator")) {
	    mainChannelNum = ELT1_1;
	    PASSIVE_SYNC_PORT_LIST (LIST1);
	}
	else if (!strcmp (compname, "$BrzSynchPull")) {
	    mainChannelNum = ELT2;
	    PASSIVE_PULL_PORT_LIST (LIST1);
	    ACTIVE_PULL_PORT (ELT2);
	}
        else {
            GList *elt = argList;
            for (; elt; elt=elt->next) {
                int arg = atoi(((PtrTMPNode)elt->data)->body.string);
                struct Chan *chan = g_ptr_array_index (allChans, arg);
				if (!chan) {
					fprintf (stderr, "error with component %s\n", compname);
					continue;
				}
                if (!chan->source)
                    chan->source = comp;
                else if (!chan->dest)
                    chan->dest = comp;
                else // Emphasize the fact that src and dest are occupied by unoriented components
                    chan->dest = comp;
                chan->direction=ChanDirection_Unknown;
            }
        }

        g_ptr_array_add (threadGroup->comps, comp);
    }
}
*/

void InitBreezePartThreadGroups (struct BreezePart *breezePart)
{
    breezePart->threadGroups = g_ptr_array_new ();

    //    ReadChans (breezePart);
    //    ReadComps (breezePart);
}

void ThreadGroups_Foreach (struct BreezePart *breezePart, GHFunc func, gpointer user_data)
{
    unsigned int i;

    for (i = 0; i < breezePart->threadGroups->len; i++)
    {
        struct ThreadGroupsItem *threadGroup = g_ptr_array_index (breezePart->threadGroups, i);

        func (threadGroup, user_data, 0);
    }
}
