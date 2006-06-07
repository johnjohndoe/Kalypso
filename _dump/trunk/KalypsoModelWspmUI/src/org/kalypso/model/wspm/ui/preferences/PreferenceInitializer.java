package org.kalypso.model.wspm.ui.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.view.table.TableView;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer
{
  /**
   * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
   */
  @Override
  public void initializeDefaultPreferences( )
  {
    final IPreferenceStore store = KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore();
    store.setDefault( PreferenceConstants.P_ALLWAYSOPENTABLE, true );

    store.setDefault( PreferenceConstants.P_TABLE_ADVANCE_MODE, TableView.getAdvanceModes()[0][1] );
    
    store.setDefault( PreferenceConstants.P_VALIDATE_PROFILE, true );

    store.setDefault( PreferenceConstants.P_VALIDATE_RULES_TO_EXCLUDE, "" );
  }
}
