package org.kalypso.ui.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * TODO: Andreas, Gernot, Marc: set your default preferences here.
 * 
 * @author schlienger
 */
public class KalypsoPreferenceInitializer extends AbstractPreferenceInitializer
{
  /**
   * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
   */
  public void initializeDefaultPreferences()
  {
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.NUMBER_OF_DAYS, "30" );
    
    // TODO change this default property before installation!
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.CLIENT_CONF_URLS, "http://pc242:8080/KalypsoConf/kalypso-client.ini");
    
    //KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault(key, value);
  }
}
