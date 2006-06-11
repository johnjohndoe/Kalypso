package org.kalypso.model.wspm.ui.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;


/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage </samp>,
 * we can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */
public class ProfileditorPreferencePage extends FieldEditorPreferencePage
    implements IWorkbenchPreferencePage
{

  public ProfileditorPreferencePage( )
  {
    super( GRID );
    setPreferenceStore( KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore() );
    
    setDescription( "Allgmeines Einstellungen des Profileditor Plugins" );
  }

  @Override
  public void createFieldEditors( )
  {
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( IWorkbench workbench )
  {
    // nix zu tun
  }
}