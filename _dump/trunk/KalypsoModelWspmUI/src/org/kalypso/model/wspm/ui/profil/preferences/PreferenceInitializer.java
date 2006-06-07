package org.kalypso.model.wspm.ui.profil.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;


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

  }
}
