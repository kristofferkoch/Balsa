#include <gtk/gtk.h>

gboolean OnMainWindowDestroy (GtkWidget * widget, GdkEvent * event, gpointer user_data);

gboolean OnMainWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data);

void OnProjectMenu_New (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_Open (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_Save (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_SaveAs (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_Close (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_ProjectOptions (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_EnvironmentOptions (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_Quit (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_New (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_Open (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_ReOpen (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_Save (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_SaveAs (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_Close (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_CloseAll (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectMenu_AddFilesIntoProject (GtkMenuItem * menuitem, gpointer user_data);

void OnFileMenu_AddCurrentFileToProject (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_Edit (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_Make (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_GetInfo (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_AddTest (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_AddLardTestFile (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_AddImplementation (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_Delete (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ViewFilesNotAlways (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ViewFilesAlways (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ViewProceduresNotAlways (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ViewProceduresAlways (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ViewTestsNo (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ViewTestsYes (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_Console_Always (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_Console_Never (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_Console_Automatic (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_Console_DisplayHide (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ExecutionWindow_Always (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ExecutionWindow_Never (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ExecutionWindow_Automatic (GtkMenuItem * menuitem, gpointer user_data);

void OnViewMenu_ExecutionWindow_DisplayHide (GtkMenuItem * menuitem, gpointer user_data);

void OnBuildMenu_BuildMakefile (GtkMenuItem * menuitem, gpointer user_data);

void OnHelpMenu_About (GtkMenuItem * menuitem, gpointer user_data);

void OnProjectNotebook_SwitchPage (GtkNotebook * notebook, GtkNotebookPage * page, gint page_num, gpointer user_data);

void OnProjectFileviewSelectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data);

void OnProjectFileviewUnselectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data);

gboolean OnProjectFileviewMousePressed (GtkWidget * widget, GdkEventButton * event, gpointer user_data);

gboolean OnFilesNotebook_ButtonPressEvent (GtkWidget * widget, GdkEventButton * event, gpointer user_data);

void OnOptionsWindowShow (GtkWidget * widget, gpointer user_data);

gboolean OnOptionsDialogueCancel (GtkWidget * widget, GdkEvent * event, gpointer user_data);

gboolean OnOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnPSViewerNameDefault (GtkButton * button, gpointer user_data);

void OnTmpDirDefault (GtkButton * button, gpointer user_data);

void OnPrintCommandDefault (GtkButton * button, gpointer user_data);

void OnBalsaHomeDefault (GtkButton * button, gpointer user_data);

void OnEditorNameDefault (GtkButton * button, gpointer user_data);

void OnOptionsDialogueOK (GtkButton * button, gpointer user_data);

gboolean OnProjectOptionsDialogueCancel (GtkWidget * widget, GdkEvent * event, gpointer user_data);

gboolean OnProjectOptionsKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnProjectFileImportPathListSelectChild (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnProjectFileImportPathListUnselectChild (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnProjectFileImportPathEntryChange (GtkEditable * editable, gpointer user_data);

void OnProjectFileImportPathUpButton (GtkButton * button, gpointer user_data);

void OnProjectFileImportPathNewButton (GtkButton * button, gpointer user_data);

void OnProjectFileImportPathDeleteButton (GtkButton * button, gpointer user_data);

void OnProjectFileImportPathDownButton (GtkButton * button, gpointer user_data);

void OnProjectFileImportPathBrowse (GtkButton * button, gpointer user_data);

void OnProjectFileImport_ConvertRelAbs (GtkButton * button, gpointer user_data);

void OnProjectDirectoryBrowse (GtkButton * button, gpointer user_data);

void OnProjectOptions_LardInterpretedSimulation (GtkButton * button, gpointer user_data);

void OnProjectOptions_Lard2C (GtkButton * button, gpointer user_data);

void OnProjectOptions_BreezeDirectSimulation (GtkButton * button, gpointer user_data);

void OnProjectOptions_Breeze2Lard (GtkButton * button, gpointer user_data);

void OnProjectOptions_FlattenedCompilation (GtkButton * button, gpointer user_data);

void OnProjectOptions_HierarchicalCompilation (GtkButton * button, gpointer user_data);

void OnProjectOptions_StructuralSimulation (GtkButton * button, gpointer user_data);

void OnProjectOptions_BehavioralSimulation (GtkButton * button, gpointer user_data);

void OnProjectOptions_SaveChannelNumbers (GtkToggleButton * togglebutton, gpointer user_data);

void OnProjectOptionsLCDOPTSentryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptionsB2LOPTSentryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptionsBALSACOPTSentryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptionsBREEZELINKOPTSentryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptionsBREEZESIMOPTSentryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptions_TraceAllChannels (GtkButton * button, gpointer user_data);

void OnProjectOptions_TraceOnlyInterfacePorts (GtkButton * button, gpointer user_data);

void OnProjectOptions_NoTraceFlushing (GtkButton * button, gpointer user_data);

void OnProjectOptions_TraceFlushingDelay (GtkButton * button, gpointer user_data);

void OnProjectOptions_TraceFlushingDelaySpin (GtkEditable * editable, gpointer user_data);

gboolean OnProjectOptions_TraceFlushingDelaySpin_key (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnProjectOptionsDefinitionsUpButton (GtkButton * button, gpointer user_data);

void OnProjectOptionsDefinitionsNewButton (GtkButton * button, gpointer user_data);

void OnProjectOptionsDefinitionsDeleteButton (GtkButton * button, gpointer user_data);

void OnProjectOptionsDefinitionsDownButton (GtkButton * button, gpointer user_data);

void OnProjectOptionsDefinitionsSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnProjectOptionsDefinitionsUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnProjectOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable, gpointer user_data);

void OnProjectOptionsDialogueSaveAsDefaultTemplate (GtkButton * button, gpointer user_data);

void OnProjectOptionsDialogueOK (GtkButton * button, gpointer user_data);

gboolean OnDirectoryDialogueKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnDirectoryDialogueOK (GtkButton * button, gpointer user_data);

void OnDialogueCancel (GtkButton * button, gpointer user_data);

gboolean OnFileDialogueKeyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnFileDialogueOK (GtkButton * button, gpointer user_data);

gboolean OnTestOptionsDialogue_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnTestProcNameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnTestFilenameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnSBreezeWarningButtonClicked (GtkButton * button, gpointer user_data);

void OnTestPortRadiobuttonToggle (GtkToggleButton * togglebutton, gpointer user_data);

void OnTestPortEntryChange (GtkEditable * editable, gpointer user_data);

void OnTestPortValueEntryChange (GtkEditable * editable, gpointer user_data);

void OnTestPortFileBrowseButton (GtkButton * button, gpointer user_data);

void OnTestFformatEntryChange (GtkEditable * editable, gpointer user_data);

void OnTestPformatEntryChange (GtkEditable * editable, gpointer user_data);

void OnMemoryPortEntryChange (GtkEditable * editable, gpointer user_data);

void OnTestPortListSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnTestPortListUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnTestPortsNewButton (GtkButton * button, gpointer user_data);

void OnTestPortsDelete (GtkButton * button, gpointer user_data);

void OnTestPortsRefill (GtkButton * button, gpointer user_data);

void OnTestOptionsDialogueOK (GtkButton * button, gpointer user_data);

void OnTestOptionsDialogueCancel (GtkButton * button, gpointer user_data);

gboolean OnExecutionWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data);

gboolean OnAuxillaryWindow_KeyPressed (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnExecutionCTree_SelectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data);

void OnExecutionCTree_UnselectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data);

void OnExecutionWindowStopProcessButton (GtkButton * button, gpointer user_data);

gboolean OnTextBox_ButtonPressed (GtkWidget * widget, GdkEventButton * event, gpointer user_data);

gboolean OnConsoleWindowDelete (GtkWidget * widget, GdkEvent * event, gpointer user_data);

gboolean OnToolsMenuEditorDialog_delete (GtkWidget * widget, GdkEvent * event, gpointer user_data);

void OnToolsList_selectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnToolsList_unselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnToolsDialog_buttonNew (GtkButton * button, gpointer user_data);

void OnToolsDialog_buttonCopy (GtkButton * button, gpointer user_data);

void OnToolsDialog_buttonRemove (GtkButton * button, gpointer user_data);

void OnToolsDialog_buttonUp (GtkButton * button, gpointer user_data);

void OnToolsDialog_buttonDown (GtkButton * button, gpointer user_data);

void OnToolNameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnToolCommandEntryChanged (GtkEditable * editable, gpointer user_data);

void OnToolsDialog_ButtonOK (GtkButton * button, gpointer user_data);

void OnToolsDialog_ButtonCancel (GtkButton * button, gpointer user_data);

gboolean OnAddFileDialog_CancelButton (GtkWidget * widget, GdkEvent * event, gpointer user_data);

gboolean OnAddFileDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnAddFileDialog_TreeSelectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data);

void OnAddFileDialog_TreeUnselectRow (GtkCTree * ctree, GList * node, gint column, gpointer user_data);

void OnAddFileDialog_ImportNewPathButton (GtkButton * button, gpointer user_data);

void OnAddFileDialog_OKButton (GtkButton * button, gpointer user_data);

gboolean OnLardTestFileOptionsDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnLardTestFileOptionsBrowse (GtkButton * button, gpointer user_data);

void OnLardTestFileOptionsDialogueOK (GtkButton * button, gpointer user_data);

void OnLardTestFileOptionsDialogueCancel (GtkButton * button, gpointer user_data);

gboolean OnImplementationOptionsDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnBalsaNetlistOptionsDefault (GtkButton * button, gpointer user_data);

void OnStyleOptionValueChanged (GtkEditable * editable, gpointer user_data);

void OnSelectStyleOption (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnImplementationOptionsDialogueOK (GtkButton * button, gpointer user_data);

void OnImplementationOptionsDialogueCancel (GtkButton * button, gpointer user_data);

void OnKillAllConfirm (GtkButton * button, gpointer user_data);

void OnKillAllCancel (GtkButton * button, gpointer user_data);

void OnTestFilenameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnTestProcNameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnTestPortFileFormatBrowseButton (GtkButton * button, gpointer user_data);

void OnTestPortPrintFormatBrowseButton (GtkButton * button, gpointer user_data);

void OnSelectionMenu_AddBuiltinLin_activate (GtkMenuItem * menuitem, gpointer user_data);

void OnBuiltinLibOptionsDialogueOK (GtkButton * button, gpointer user_data);

void OnBuiltinLibOptionsDialogueCancel (GtkButton * button, gpointer user_data);

void OnSelectionMenu_AddBuiltinLib_activate (GtkMenuItem * menuitem, gpointer user_data);

void OnSelectionMenu_AddBuiltinLib (GtkMenuItem * menuitem, gpointer user_data);

gboolean OnBuiltinLibOptionsDialog_keyPressEvent (GtkWidget * widget, GdkEventKey * event, gpointer user_data);

void OnStyleOptionValueChanged (GtkEditable * editable, gpointer user_data);

void OnSelectStyleOption (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void on_BuiltinLibOptionsRemoveButton_clicked (GtkButton * button, gpointer user_data);

void on_BuiltinLibOptionsNewButton_clicked (GtkButton * button, gpointer user_data);

void OnTestOptionsDefinitionsUpButton (GtkButton * button, gpointer user_data);

void OnTestOptionsDefinitionsNewButton (GtkButton * button, gpointer user_data);

void OnTestOptionsDefinitionsDeleteButton (GtkButton * button, gpointer user_data);

void OnTestOptionsDefinitionsDownButton (GtkButton * button, gpointer user_data);

void OnTestOptionsDefinitionsSelectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnTestOptionsDefinitionsUnselectRow (GtkCList * clist, gint row, gint column, GdkEvent * event, gpointer user_data);

void OnTestOptionsDialogueDefinitionsNameEntryChanged (GtkEditable * editable, gpointer user_data);

void OnTestOptionsDialogueDefinitionsValueEntryChanged (GtkEditable * editable, gpointer user_data);

void on_DumpFileCheckbutton_toggled (GtkToggleButton * togglebutton, gpointer user_data);

void OnTestFformatEntryChange (GtkEditable * editable, gpointer user_data);
