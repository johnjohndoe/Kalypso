package org.kalypso.ui.preferences;

import java.text.DateFormat;
import java.util.Date;

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
    // location of the server configuration for the clients
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.CLIENT_CONF_URLS, "http://SERVER_NAME:8080/KalypsoConf/kalypso-client.ini");
    
    // do not use range, use number of days
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.USE_RANGE, false );
    // number of days
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.NUMBER_OF_DAYS, 30 );
    // default dates
    final String defDate = DateFormat.getDateTimeInstance().format( new Date() );
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.DATE_FROM, defDate );
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.DATE_TO, defDate );
  }
}
