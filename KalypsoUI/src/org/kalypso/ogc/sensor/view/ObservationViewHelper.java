package org.kalypso.ogc.sensor.view;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

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
   * the item belongs. If the property is not defined, it uses the Kalypso
   * Preferences.
   * 
   * @param item
   * @return new instance of DateRangeArgument
   */
  public static DateRangeArgument makeDateRange( final IRepositoryItem item )
  {
    final DateFormat df = DateFormat.getDateTimeInstance();
    
    if( item.getRepository().getProperty( IKalypsoPreferences.USE_RANGE ) != null )
    {
      final boolean useRange = Boolean.valueOf( item.getRepository()
          .getProperty( IKalypsoPreferences.USE_RANGE ) ).booleanValue();

      if( useRange )
      {
        try
        {
          final Date dateFrom = df.parse( item.getRepository().getProperty(
              IKalypsoPreferences.DATE_FROM ) );

          final Date dateTo = df.parse( item.getRepository().getProperty(
              IKalypsoPreferences.DATE_TO ) );
          
          return new DateRangeArgument( dateFrom, dateTo );
        }
        catch( ParseException e )
        {
          return new DateRangeArgument();
        }
      }
      else
      {
        final String strDays = item.getRepository().getProperty(
            IKalypsoPreferences.NUMBER_OF_DAYS );

        return DateRangeArgument.createFromPastDays( Integer.valueOf( strDays )
            .intValue() );
      }
    }
    else
    {
      final boolean useRange = KalypsoGisPlugin.getDefault()
          .getPluginPreferences().getBoolean( IKalypsoPreferences.USE_RANGE );

      if( useRange )
      {
        try
        {
          final Date dateFrom = df.parse( KalypsoGisPlugin.getDefault()
              .getPluginPreferences().getString(
              IKalypsoPreferences.DATE_FROM ) );

          final Date dateTo = df.parse( KalypsoGisPlugin.getDefault()
              .getPluginPreferences().getString(
              IKalypsoPreferences.DATE_TO ) );
          
          return new DateRangeArgument( dateFrom, dateTo );
        }
        catch( ParseException e )
        {
          return new DateRangeArgument();
        }
      }
      else
      {
        final int days = KalypsoGisPlugin.getDefault()
            .getPluginPreferences().getInt(
                IKalypsoPreferences.NUMBER_OF_DAYS );

        return DateRangeArgument.createFromPastDays( days );
      }
    }
  }
}