package org.kalypso.ogc.sensor.view;

import org.kalypso.repository.IRepositoryItem;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * ObservationViewHelper
 * 
 * @author schlienger
 */
public final class ObservationViewHelper
{
  private ObservationViewHelper( )
  {
    // not to be instanciated
  }

  /**
   * Makes a DateRangeArgument using the properties of the repository into which
   * the item belongs. If the property is not defined, it uses the Kalypso Preferences.
   * 
   * @param item
   * @return new instance of DateRangeArgument
   */
  public static DateRangeArgument makeDateRange( final IRepositoryItem item )
  {
    String strDays = item.getRepository().getProperty( IKalypsoPreferences.NUMBER_OF_DAYS );
    
    if( strDays == null || strDays.length() == 0 )
      strDays = KalypsoGisPlugin.getDefault().getPluginPreferences().getString(  IKalypsoPreferences.NUMBER_OF_DAYS );
    
    return DateRangeArgument.createFromPastDays( Integer.valueOf( strDays ).intValue() );
  }
}
