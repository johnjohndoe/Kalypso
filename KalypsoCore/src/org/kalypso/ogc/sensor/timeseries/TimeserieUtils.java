package org.kalypso.ogc.sensor.timeseries;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * Utilities when dealing with Observations which are Kalypso Timeseries.
 * 
 * @author schlienger
 */
public class TimeserieUtils
{
  private TimeserieUtils( )
  {
    // no instanciation
  }

  /**
   * Finds out which metadata of the given observation begin with the
   * given prefix.
   * <p>
   * This is for instance usefull for the Alarmstufen
   * 
   * @param obs
   * @param mdPrefix
   * @return list of metadata keys or empty array if nothing found
   */
  public final static String[] findOutMDBeginningWith( final IObservation obs, final String mdPrefix )
  {
    final MetadataList mdl = obs.getMetadataList();
    
    final ArrayList mds = new ArrayList();
    
    final Iterator it = mdl.keySet().iterator();
    while( it.hasNext() )
    {
      final String md = it.next().toString();
      
      if( md.startsWith( mdPrefix ) )
        mds.add( md );
    }
    
    return (String[]) mds.toArray( new String[mds.size()]);
  }

  /**
   * Finds out the list of alarmstufen metadata keys.
   * 
   * @param obs
   * @return list of metadata keys
   */
  public final static String[] findOutMDAlarmstufen( final IObservation obs )
  {
    return findOutMDBeginningWith( obs, "Alarmstufe" );
  }
  
  /**
   * Returns the color to use when displaying the value of the
   * given Alarmstufe.
   * 
   * @param mdAlarm
   * @return color
   */
  public final static Color getColorFor( final String mdAlarm )
  {
    if( TimeserieConstants.MD_ALARM_1.equals( mdAlarm ) )
      return Color.RED;
    if( TimeserieConstants.MD_ALARM_2.equals( mdAlarm ) )
      return Color.ORANGE;
    if( TimeserieConstants.MD_ALARM_3.equals( mdAlarm ) )
      return Color.CYAN;
    if( TimeserieConstants.MD_ALARM_4.equals( mdAlarm ) )
      return Color.MAGENTA;
    
    return null;
  }
  
  /**
   * Sets the 'forecast' metadata of the given observation using the given date
   * range.
   * 
   * @param obs
   * @param from
   * @param to
   */
  public final static void setForecast( final IObservation obs,
      final Date from, final Date to )
  {
    obs.getMetadataList().setProperty(
        TimeserieConstants.MD_VORHERSAGE,
        TimeserieConstants.DEFAULT_DF.format( from ) + ";"
            + TimeserieConstants.DEFAULT_DF.format( to ) );
  }

  /**
   * Returns a new instance of DateRangeArgument containing the beginning and
   * the end of the forecast, given the observation is a forecast.
   * <p>
   * An observation is a forecast when it has the MD_VORHERSAGE Metadata.
   * 
   * @param obs
   * @return date range of the forecast or null if obs isn't a forecast.
   */
  public final static DateRangeArgument isForecast( final IObservation obs )
  {
    final MetadataList mdl = obs.getMetadataList();
    final String range = mdl.getProperty( TimeserieConstants.MD_VORHERSAGE );
    if( range != null )
    {
      final String[] splits = range.split( ";" );
      if( splits.length == 2 )
      {
        try
        {
          final Date from = TimeserieConstants.DEFAULT_DF.parse( splits[0] );
          final Date to = TimeserieConstants.DEFAULT_DF.parse( splits[1] );

          return new DateRangeArgument( from, to );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    return null;
  }
}