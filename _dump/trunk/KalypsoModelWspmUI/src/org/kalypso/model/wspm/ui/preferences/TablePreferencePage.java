package org.kalypso.model.wspm.ui.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.view.table.TableView;

/**
 * @author gernot
 * 
 */
public class TablePreferencePage extends FieldEditorPreferencePage implements
    IWorkbenchPreferencePage
{

  public TablePreferencePage( )
  {
    super( GRID );
    setPreferenceStore( KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore() );
    
    setDescription( "Einstellungen der tabellarischen Profilansicht" );
  }

  @Override
  public void createFieldEditors( )
  {
//    addField( new DirectoryFieldEditor( PreferenceConstants.P_PATH,
//        "&Directory preference:", getFieldEditorParent() ) );

    addField( new BooleanFieldEditor( PreferenceConstants.P_ALLWAYSOPENTABLE,
        "&Tabellenansicht mit Profileditor öffnen", getFieldEditorParent() ) );

    final String[][] modes = TableView.getAdvanceModes();
    addField( new RadioGroupFieldEditor( PreferenceConstants.P_TABLE_ADVANCE_MODE,
        "Cursor nach dem Editieren", modes.length, modes,
        getFieldEditorParent() ) );
//    addField( new StringFieldEditor( PreferenceConstants.P_STRING,
//        "A &text preference:", getFieldEditorParent() ) );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( IWorkbench workbench )
  {
  }
}