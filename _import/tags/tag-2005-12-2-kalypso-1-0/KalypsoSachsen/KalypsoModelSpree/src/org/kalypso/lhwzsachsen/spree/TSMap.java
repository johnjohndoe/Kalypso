package org.kalypso.lhwzsachsen.spree;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * @author belger
 */
public class TSMap
{
  public static String getTypeForName( final String tsName )
  {
    if( tsName.startsWith( "W_" ) )
      return TimeserieConstants.TYPE_WATERLEVEL;

    if( tsName.startsWith( "Q_" ) )
      return TimeserieConstants.TYPE_RUNOFF;
    if( tsName.startsWith( "QX" ) )
      return TimeserieConstants.TYPE_RUNOFF;
    if( tsName.startsWith( "WV" ) )
      return TimeserieConstants.TYPE_WATERLEVEL;
    if( tsName.startsWith( "QV" ) )
      return TimeserieConstants.TYPE_RUNOFF;
    if( tsName.startsWith( "QP" ) )
      return TimeserieConstants.TYPE_RUNOFF;
    if( tsName.startsWith( "PG" ) )
      return TimeserieConstants.TYPE_RAINFALL;
    if( tsName.startsWith( "PA" ) )
      return TimeserieConstants.TYPE_RAINFALL;
    if( tsName.startsWith( "V_" ) )
      return TimeserieConstants.TYPE_VOLUME;

    return "value";
  }

  /** Zeitreihenname (z.B. W_SCHIRG) -> [date->value] */
  final Map m_map = new HashMap();

  /** sortiert die Daten nach der Zeit */
  final Set m_dateSet = new TreeSet();

  /** name (String) -> IObservation */
  private Map m_obsMap = new HashMap();
  
  /** name (String) -> accuracy (Double) */
  private Map m_accuracyMap = new HashMap();

  public void addObservation( final IObservation obs, final String name ) throws SensorException,
      NoSuchElementException
  {
    final IAxis[] axisList = obs.getAxisList();

    final IAxis dateAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_DATE );

    final IAxis valueAxis = ObservationUtilities.findAxisByType( axisList, getTypeForName( name ) );

    final ITuppleModel model = obs.getValues( null );

    for( int j = 0; j < model.getCount(); j++ )
    {
      final Date date = (Date)model.getElement( j, dateAxis );
      final Number val = (Number)model.getElement( j, valueAxis );
      final Double value = val == null ? null : new Double( val.doubleValue() );

      putValue( name, date, value );
    }

    m_obsMap.put( name, obs );
  }

  public void putValue( final String name, final Date date, final Double value )
  {
    final Map dateToValueMap = getMap( name );

    m_dateSet.add( date );
    dateToValueMap.put( date, value );
  }

  private Map getMap( final String name )
  {
    final Map map = (Map)m_map.get( name );
    if( map != null )
      return map;

    final HashMap newMap = new HashMap();
    m_map.put( name, newMap );
    return newMap;
  }

  public Date[] getDates()
  {
    return (Date[])m_dateSet.toArray( new Date[m_dateSet.size()] );
  }

  public Map getTimeserie( final String id )
  {
    return (Map)m_map.get( id );
  }

  public MetadataList getMetadataFor( final String name )
  {
    final IObservation oldObs = (IObservation)m_obsMap.get( name );
    return oldObs == null ? null : oldObs.getMetadataList();
  }

  public void setAccuracy( final String name, final Double accuracy )
  {
    m_accuracyMap.put( name, accuracy );
  }
  
  public double getAccuracy( final String name )
  {
    final Double accuracy = (Double)m_accuracyMap.get(name);
    if( accuracy == null )
      return 5d;
    
    return accuracy.doubleValue();
  }
}