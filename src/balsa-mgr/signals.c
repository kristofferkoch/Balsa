#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>

#include "signals.h"
#include "widgets.h"
#include "support.h"

#include "main.h"
#include "project.h"
#include "file.h"
#include "miscgtk.h"
#include "options.h"
#include "paths.h"
#include "buffer.h"
#include "commands.h"
#include "projopts.h"
#include "testopts.h"
#include "filemanager.h"
#include "utils.h"
#include "makefiletable.h"
#include "executionmanager.h"
#include "toolseditor.h"
#include "addfiledialog.h"
#include "technology.h"
#include "workspace.h"
#include "mainwindow.h"
#include "executionwindow.h"
#include "consolewindow.h"
#include "aboutdialog.h"
#include "optionsdialog.h"
#include "toolsmenueditordialog.h"
#include "menu_project.h"
#include "menu_file.h"
#include "menu_selecteditem.h"
#include "menu_view.h"
#include "menu_build.h"
#include "menu_cvs.h"
#include "menu_tools.h"
#include "menu_help.h"
#include "mainwindow_projecteditor.h"
#include "mainwindow_fileseditor.h"
#include "mainwindow_menus.h"
#include "icons.h"
#include "projectoptionsdialog.h"
#include "imploptionsdialog.h"
#include "builtinliboptionsdialog.h"

gboolean OnMainWindowDestroy (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponMainWindowDestroy (widget, event, user_data);
    return FALSE;
}

gboolean OnMainWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponMainWindowDelete (widget, event, user_data);
    return FALSE;
}

void OnProjectMenu_New (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_New (menuitem, user_data);
}

void OnProjectMenu_Open (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_Open (menuitem, user_data);
}

void OnProjectMenu_Save (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_Save (menuitem, user_data);
}

void OnProjectMenu_SaveAs (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_SaveAs (menuitem, user_data);
}

void OnProjectMenu_Close (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_Close (menuitem, user_data);
}

void OnProjectMenu_ProjectOptions (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_ProjectOptions (menuitem, user_data);
}

void OnProjectMenu_EnvironmentOptions (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_EnvironmentOptions (menuitem, user_data);
}

void OnProjectMenu_Quit (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_Quit (menuitem, user_data);
}

void OnFileMenu_New (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_New (menuitem, user_data);
}

void OnFileMenu_Open (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_Open (menuitem, user_data);
}

void OnFileMenu_ReOpen (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_ReOpen (menuitem, user_data);
}

void OnFileMenu_Save (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_Save (menuitem, user_data);
}

void OnFileMenu_SaveAs (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_SaveAs (menuitem, user_data);
}

void OnFileMenu_Close (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_Close (menuitem, user_data);
}

void OnFileMenu_CloseAll (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_CloseAll (menuitem, user_data);
}

void OnProjectMenu_AddFilesIntoProject (GtkMenuItem * menuitem, gpointer user_data)
{
    UponProjectMenu_AddFilesIntoProject (menuitem, user_data);
}

void OnFileMenu_AddCurrentFileToProject (GtkMenuItem * menuitem, gpointer user_data)
{
    UponFileMenu_AddCurrentFileToProject (menuitem, user_data);
}

void OnSelectionMenu_Edit (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_Edit (menuitem, user_data);
}

void OnSelectionMenu_Make (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_Make (menuitem, user_data);
}

void OnSelectionMenu_GetInfo (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_GetInfo (menuitem, user_data);
}

void OnSelectionMenu_AddTest (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_AddTest (menuitem, user_data);
}

void OnSelectionMenu_AddLardTestFile (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_AddLardTestFile (menuitem, user_data);
}

void OnSelectionMenu_AddImplementation (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_AddImplementation (menuitem, user_data);
}

void OnSelectionMenu_Delete (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_Delete (menuitem, user_data);
}

void OnViewMenu_ViewFilesNotAlways (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ViewFilesNotAlways (menuitem, user_data);
}

void OnViewMenu_ViewFilesAlways (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ViewFilesAlways (menuitem, user_data);
}

void OnViewMenu_ViewProceduresNotAlways (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ViewProceduresNotAlways (menuitem, user_data);
}

void OnViewMenu_ViewProceduresAlways (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ViewProceduresAlways (menuitem, user_data);
}

void OnViewMenu_ViewTestsNo (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ViewTestsNo (menuitem, user_data);
}

void OnViewMenu_ViewTestsYes (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ViewTestsYes (menuitem, user_data);
}

void OnViewMenu_Console_Always (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_Console_Always (menuitem, user_data);
}

void OnViewMenu_Console_Never (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_Console_Never (menuitem, user_data);
}

void OnViewMenu_Console_Automatic (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_Console_Automatic (menuitem, user_data);
}

void OnViewMenu_Console_DisplayHide (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_Console_DisplayHide (menuitem, user_data);
}

void OnViewMenu_ExecutionWindow_Always (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ExecutionWindow_Always (menuitem, user_data);
}

void OnViewMenu_ExecutionWindow_Never (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ExecutionWindow_Never (menuitem, user_data);
}

void OnViewMenu_ExecutionWindow_Automatic (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ExecutionWindow_Automatic (menuitem, user_data);
}

void OnViewMenu_ExecutionWindow_DisplayHide (GtkMenuItem * menuitem, gpointer user_data)
{
    UponViewMenu_ExecutionWindow_DisplayHide (menuitem, user_data);
}

void OnBuildMenu_BuildMakefile (GtkMenuItem * menuitem, gpointer user_data)
{
    UponBuildMenu_BuildMakefile (menuitem, user_data);
}

void OnHelpMenu_About (GtkMenuItem * menuitem, gpointer user_data)
{
    UponHelpMenu_About (menuitem, user_data);
}

void OnProjectNotebook_SwitchPage (GtkNotebook * notebook, GtkNotebookPage * page, gint page_num, gpointer user_data)
{
    UponProjectNotebook_SwitchPage (notebook, page, page_num, user_data);
}

void OnProjectFileviewSelectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data)
{
    UponProjectFileviewSelectRow (GTK_WIDGET (ctree), GTK_CTREE_NODE (node), column, user_data);
}

void OnProjectFileviewUnselectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data)
{
    UponProjectFileviewUnselectRow (GTK_WIDGET (ctree), GTK_CTREE_NODE (node), column, user_data);
}

gboolean OnProjectFileviewMousePressed (GtkWidget * widget, GdkEventButton * event, gpointer user_data)
{
    return UponProjectFileviewMousePressed (widget, event, user_data);
    return FALSE;
}

gboolean OnFilesNotebook_ButtonPressEvent (GtkWidget * widget, GdkEventButton * event, gpointer user_data)
{
    return UponFilesNotebook_ButtonPressEvent (widget, event, user_data);
    return FALSE;
}

void OnOptionsWindowShow (GtkWidget * widget, gpointer user_data)
{
    UponOptionsWindowShow (widget, user_data);
}

gboolean OnOptionsDialogueCancel (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponOptionsDialogueCancel ();
}

gboolean OnOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponOptionsKeyPressEvent (widget, event, user_data);
    return FALSE;
}

void OnPSViewerNameDefault (GtkButton * button, gpointer user_data)
{
    UponPSViewerNameDefault (button, user_data);
}

void OnTmpDirDefault (GtkButton * button, gpointer user_data)
{
    UponTmpDirDefault (button, user_data);
}

void OnPrintCommandDefault (GtkButton * button, gpointer user_data)
{
    UponPrintCommandDefault (button, user_data);
}

void OnBalsaHomeDefault (GtkButton * button, gpointer user_data)
{
    UponBalsaHomeDefault (button, user_data);
}

void OnEditorNameDefault (GtkButton * button, gpointer user_data)
{
    UponEditorNameDefault (button, user_data);
}

void OnOptionsDialogueOK (GtkButton * button, gpointer user_data)
{
    UponOptionsDialogueOK (button, user_data);
}

gboolean OnProjectOptionsDialogueCancel (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (widget));

    UponProjectOptionsDialogueCancel (dialogue);
    return FALSE;
}

gboolean OnProjectOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponProjectOptionsKeyPressEvent (widget, event, user_data);
    return FALSE;
}

void OnProjectFileImportPathListSelectChild (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponProjectFileImportPathListSelectChild (clist, row, column, event);
}

void OnProjectFileImportPathListUnselectChild (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponProjectFileImportPathListUnselectChild (clist, row, column, event);
}

void OnProjectFileImportPathEntryChange (GtkEditable * editable, gpointer user_data)
{
    UponProjectFileImportPathEntryChange (editable);
}

void OnProjectFileImportPathUpButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectFileImportPathUpButton (dialogue);
}

void OnProjectFileImportPathNewButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectFileImportPathNewButton (dialogue);
}

void OnProjectFileImportPathDeleteButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectFileImportPathDeleteButton (dialogue);
}

void OnProjectFileImportPathDownButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectFileImportPathDownButton (dialogue);
}

void OnProjectFileImportPathBrowse (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectFileImportPathBrowse (dialogue);
}

void OnProjectFileImport_ConvertRelAbs (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectFileImport_ConvertRelAbs (dialogue);
}

void OnProjectDirectoryBrowse (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectDirectoryBrowse (dialogue);
}

void OnProjectOptions_LardInterpretedSimulation (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_LardInterpretedSimulation (button, user_data);
}

void OnProjectOptions_Lard2C (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_Lard2C (button, user_data);
}

void OnProjectOptions_BreezeDirectSimulation (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_BreezeDirectSimulation (button, user_data);
}

void OnProjectOptions_Breeze2Lard (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_Breeze2Lard (button, user_data);
}

void OnProjectOptions_FlattenedCompilation (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_FlattenedCompilation (button, user_data);
}

void OnProjectOptions_HierarchicalCompilation (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_HierarchicalCompilation (button, user_data);
}

void OnProjectOptions_StructuralSimulation (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_StructuralSimulation (button, user_data);
}

void OnProjectOptions_BehavioralSimulation (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_BehavioralSimulation (button, user_data);
}

void OnProjectOptions_SaveChannelNumbers (GtkToggleButton * togglebutton, gpointer user_data)
{
    UponProjectOptions_SaveChannelNumbers (togglebutton, user_data);
}

void OnProjectOptionsLCDOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsLCDOPTSentryChanged (editable, user_data);
}

void OnProjectOptionsB2LOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsB2LOPTSentryChanged (editable, user_data);
}

void OnProjectOptionsBALSACOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsBALSACOPTSentryChanged (editable, user_data);
}

void OnProjectOptionsBREEZELINKOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsBREEZELINKOPTSentryChanged (editable, user_data);
}

void OnProjectOptionsBREEZESIMOPTSentryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsBREEZESIMOPTSentryChanged (editable, user_data);
}

void OnProjectOptions_TraceAllChannels (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_TraceAllChannels (button, user_data);
}

void OnProjectOptions_TraceOnlyInterfacePorts (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_TraceOnlyInterfacePorts (button, user_data);
}

void OnProjectOptions_NoTraceFlushing (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_NoTraceFlushing (button, user_data);
}

void OnProjectOptions_TraceFlushingDelay (GtkButton * button, gpointer user_data)
{
    UponProjectOptions_TraceFlushingDelay (button, user_data);
}

void OnProjectOptions_TraceFlushingDelaySpin (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptions_TraceFlushingDelaySpin (editable, user_data);
}

gboolean OnProjectOptions_TraceFlushingDelaySpin_key (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponProjectOptions_TraceFlushingDelaySpin_key (widget, event, user_data);
    return FALSE;
}

void OnProjectOptionsDefinitionsUpButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectOptionsDefinitionsUpButton (dialogue);
}

void OnProjectOptionsDefinitionsNewButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectOptionsDefinitionsNewButton (dialogue);
}

void OnProjectOptionsDefinitionsDeleteButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectOptionsDefinitionsDeleteButton (dialogue);
}

void OnProjectOptionsDefinitionsDownButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectOptionsDefinitionsDownButton (dialogue);
}

void OnProjectOptionsDefinitionsSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponProjectOptionsDefinitionsSelectRow (clist, row, column, event);
}

void OnProjectOptionsDefinitionsUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponProjectOptionsDefinitionsUnselectRow (clist, row, column, event);
}

void OnProjectOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsDialogueDefinitionsNameEntryChanged (editable);
}

void OnProjectOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponProjectOptionsDialogueDefinitionsValueEntryChanged (editable);
}

void OnProjectOptionsDialogueSaveAsDefaultTemplate (GtkButton * button, gpointer user_data)
{
    UponProjectOptionsDialogueSaveAsDefaultTemplate (button, user_data);
}

void OnProjectOptionsDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponProjectOptionsDialogueOK (dialogue);
}

gboolean OnDirectoryDialogueKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponDirectoryDialogueKeyPressEvent (widget, event, user_data);
    return FALSE;
}

void OnDirectoryDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponDirectoryDialogueOK (dialogue);
}

void OnDialogueCancel (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponDialogueCancel (dialogue);
}

gboolean OnFileDialogueKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponFileDialogueKeyPressEvent (widget, event, user_data);
    return FALSE;
}

void OnFileDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponFileDialogueOK (dialogue);
}

gboolean OnTestOptionsDialogue_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (widget));

    return UponTestOptionsDialogue_keyPressEvent (dialogue, event, user_data);
}

void OnTestProcNameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponTestProcNameEntryChanged (editable, user_data);
}

void OnTestFilenameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponTestFilenameEntryChanged (editable, user_data);
}

void OnSBreezeWarningButtonClicked (GtkButton * button, gpointer user_data)
{
    UponSBreezeWarningButtonClicked (GTK_WIDGET (button), user_data);
}

void OnTestPortRadiobuttonToggle (GtkToggleButton * togglebutton, gpointer user_data)
{
    UponTestPortRadiobuttonToggle (togglebutton);
}

void OnTestPortEntryChange (GtkEditable * editable, gpointer user_data)
{
    UponTestPortEntryChange (editable);
}

void OnTestPortValueEntryChange (GtkEditable * editable, gpointer user_data)
{
    UponTestPortValueEntryChange (editable);
}

void OnTestPortFileBrowseButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestPortFileBrowseButton (dialogue);
}

void OnTestFformatEntryChange (GtkEditable * editable, gpointer user_data)
{
    UponTestFformatEntryChange (editable);
}

void OnMemoryPortEntryChange (GtkEditable * editable, gpointer user_data)
{
    UponMemoryPortEntryChange (editable);
}

void OnTestPortListSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponTestPortListSelectRow (clist, row, column, event);
}

void OnTestPortListUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponTestPortListUnselectRow (clist, row, column, event);
}

void OnTestPortsNewButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestPortsNewButton (dialogue);
}

void OnTestPortsDelete (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestPortsDelete (dialogue);
}

void OnTestPortsRefill (GtkButton * button, gpointer user_data)
{
    UponTestPortsRefill (button, user_data);
}

void OnTestOptionsDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestOptionsDialogueOK (dialogue);
}

void OnTestOptionsDialogueCancel (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestOptionsDialogueCancel (dialogue);
}

gboolean OnExecutionWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponExecutionWindowDelete (widget, event, user_data);
    return FALSE;
}

gboolean OnAuxillaryWindow_KeyPressed (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponAuxillaryWindow_KeyPressed (widget, event, user_data);
    return FALSE;
}

void OnExecutionCTree_SelectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data)
{
    UponExecutionCTree_SelectRow (ctree, GTK_CTREE_NODE (node), column, user_data);
}

void OnExecutionCTree_UnselectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data)
{
    UponExecutionCTree_UnselectRow (ctree, GTK_CTREE_NODE (node), column, user_data);
}

void OnExecutionWindowStopProcessButton (GtkButton * button, gpointer user_data)
{
    UponExecutionWindowStopProcessButton (button, user_data);
}

gboolean OnTextBox_ButtonPressed (GtkWidget * widget, GdkEventButton * event, gpointer user_data)
{
    return UponTextBox_ButtonPressed (widget, event, user_data);
    return FALSE;
}

gboolean OnConsoleWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponConsoleWindowDelete (widget, event, user_data);
    return FALSE;
}

gboolean OnToolsMenuEditorDialog_delete (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponToolsMenuEditorDialog_delete (widget, event, user_data);
    return FALSE;
}

void OnToolsList_selectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponToolsList_selectRow (clist, row, column, event, user_data);
}

void OnToolsList_unselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponToolsList_unselectRow (clist, row, column, event, user_data);
}

void OnToolsDialog_buttonNew (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_buttonNew (button, user_data);
}

void OnToolsDialog_buttonCopy (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_buttonCopy (button, user_data);
}

void OnToolsDialog_buttonRemove (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_buttonRemove (button, user_data);
}

void OnToolsDialog_buttonUp (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_buttonUp (button, user_data);
}

void OnToolsDialog_buttonDown (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_buttonDown (button, user_data);
}

void OnToolNameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponToolNameEntryChanged (editable, user_data);
}

void OnToolCommandEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponToolCommandEntryChanged (editable, user_data);
}

void OnToolsDialog_ButtonOK (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_ButtonOK (button, user_data);
}

void OnToolsDialog_ButtonCancel (GtkButton * button, gpointer user_data)
{
    UponToolsDialog_ButtonCancel (button, user_data);
}

gboolean OnAddFileDialog_CancelButton (GtkWidget * widget, GdkEvent * event, gpointer user_data)
{
    return UponAddFileDialog_CancelButton (widget, event, user_data);
    return FALSE;
}

gboolean OnAddFileDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponAddFileDialog_keyPressEvent (event);
}

void OnAddFileDialog_TreeSelectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data)
{
    UponAddFileDialog_TreeSelectRow (ctree, GTK_CTREE_NODE (node), column, user_data);
}

void OnAddFileDialog_TreeUnselectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data)
{
    UponAddFileDialog_TreeUnselectRow (ctree, GTK_CTREE_NODE (node), column, user_data);
}

void OnAddFileDialog_ImportNewPathButton (GtkButton * button, gpointer user_data)
{
    UponAddFileDialog_ImportNewPathButton (button, user_data);
}

void OnAddFileDialog_OKButton (GtkButton * button, gpointer user_data)
{
    UponAddFileDialog_OKButton (button, user_data);
}

gboolean OnLardTestFileOptionsDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (widget));

    return UponLardTestFileOptionsDialog_keyPressEvent (dialogue, event, user_data);
}

void OnLardTestFileOptionsBrowse (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponLardTestFileOptionsBrowse (dialogue);
}

void OnLardTestFileOptionsDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponLardTestFileOptionsDialogueOK (dialogue);
}

void OnLardTestFileOptionsDialogueCancel (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponLardTestFileOptionsDialogueCancel (dialogue);
}

gboolean OnImplementationOptionsDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponImplementationOptionsDialog_keyPressEvent (widget, event, user_data);
    return FALSE;
}

void OnBalsaNetlistOptionsDefault (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = GTK_WIDGET (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponBalsaNetlistOptionsDefault (dialogue);
}

void OnImplementationOptionsDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = GTK_WIDGET (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponImplementationOptionsDialogueOK (dialogue);
}

void OnImplementationOptionsDialogueCancel (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = GTK_WIDGET (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponImplementationOptionsDialogueCancel (dialogue);
}

void OnKillAllConfirm (GtkButton * button, gpointer user_data)
{
    UponKillAllConfirm (button, user_data);
}

void OnKillAllCancel (GtkButton * button, gpointer user_data)
{
    UponKillAllCancel (button, user_data);
}

void OnBuiltinLibOptionsDialogueOK (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));

    UponBuiltinLibOptionsDialogueOK (dialogue);
}

void OnBuiltinLibOptionsDialogueCancel (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));

    UponBuiltinLibOptionsDialogueCancel (dialogue);
}

void OnSelectionMenu_AddBuiltinLib (GtkMenuItem * menuitem, gpointer user_data)
{
    UponSelectionMenu_AddBuiltinLib (menuitem, user_data);
}

gboolean OnBuiltinLibOptionsDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data)
{
    return UponBuiltinLibOptionsDialog_keyPressEvent (widget, event, user_data);
    return FALSE;
}

void OnStyleOptionValueChanged (GtkEditable * editable, gpointer user_data)
{
    UponStyleOptionValueChanged (editable, user_data);
}

void OnSelectStyleOption (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponSelectStyleOption (clist, row, column, event, user_data);
}

void on_BuiltinLibOptionsNewButton_clicked (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));

    UponBuiltinLibOptionsNewButtonClicked (dialogue);
}

void on_BuiltinLibOptionsRemoveButton_clicked (GtkButton * button, gpointer user_data)
{
    GtkWidget *dialogue = gtk_widget_get_toplevel (GTK_WIDGET (button));

    UponBuiltinLibOptionsRemoveButtonClicked (dialogue);
}

void OnTestOptionsDefinitionsUpButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestOptionsDefinitionsUpButton (dialogue);
}

void OnTestOptionsDefinitionsNewButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestOptionsDefinitionsNewButton (dialogue);
}

void OnTestOptionsDefinitionsDeleteButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestOptionsDefinitionsDeleteButton (dialogue);
}

void OnTestOptionsDefinitionsDownButton (GtkButton * button, gpointer user_data)
{
    GtkObject *dialogue = GTK_OBJECT (gtk_widget_get_toplevel (GTK_WIDGET (button)));

    UponTestOptionsDefinitionsDownButton (dialogue);
}

void OnTestOptionsDefinitionsSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponTestOptionsDefinitionsSelectRow (clist, row, column, event);
}

void OnTestOptionsDefinitionsUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data)
{
    UponTestOptionsDefinitionsUnselectRow (clist, row, column, event);
}

void OnTestOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponTestOptionsDialogueDefinitionsNameEntryChanged (editable);
}

void OnTestOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable, gpointer user_data)
{
    UponTestOptionsDialogueDefinitionsValueEntryChanged (editable);
}

void on_DumpFileCheckbutton_toggled (GtkToggleButton * togglebutton, gpointer user_data)
{
    UponDumpFileCheckbuttonToggled (togglebutton);
}
