package org.kalypso.wiskiadapter;

import java.util.Date;
import java.util.Map;
import java.util.Properties;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * This item is adaptable into a wiski timeserie. It represens a station.
 * 
 * @author schlienger
 */
public class TsInfoItem implements IRepositoryItem
{
  /**
   * Optional, group can be null. In that case calling getParent() will obvisouly return null
   */
  private final GroupItem m_group;

  private final WiskiRepository m_rep;

  /** maps the column-names to their values for this TsInfo item */
  private final Properties m_map;

  private WiskiTimeserie m_ts = null;

  /**
   * Constructor with group and map. The repository to which this item belongs is delivered by the group.
   */
  public TsInfoItem( final GroupItem item, final Map map )
  {
    m_group = item;
    m_map = new Properties();
    m_map.putAll( map );

    m_rep = (WiskiRepository)m_group.getRepository();
  }

  /**
   * Constructor without group. Be aware that the group is null here. This constructor is provided for simplifying the
   * process of retrieving items using WiskiRepository.findItem(). The group in that case is not relevant.
   */
  public TsInfoItem( final WiskiRepository rep, final Map map )
  {
    m_group = null;

    m_map = new Properties();
    m_map.putAll( map );

    m_rep = rep;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return m_map.getProperty( "tsinfo_name" );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getName();
  }

  /**
   * Return the Kalypso-Wiski-ID which is built according to the following specification:
   * <p>
   * wiski://GRUPPENART.PARAMETERNAME.MESSSTELLENNUMMER
   * 
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    //return m_rep.getIdentifier() + getName();

    return m_rep.getIdentifier() + getWiskiSuperGroupName() + "." + getWiskiGroupName() + "." + getWiskiStationNo();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent() throws RepositoryException
  {
    return m_group;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren() throws RepositoryException
  {
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren() throws RepositoryException
  {
    return IRepositoryItem.EMPTY_ARRAY;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
    return m_rep;
  }

  public Object getAdapter( final Class anotherClass )
  {
    if( anotherClass == IObservation.class )
    {
      if( m_ts == null )
        m_ts = new WiskiTimeserie( this );

      return m_ts;
    }

    if( anotherClass == Properties.class )
      return new WiskiTimeserie( this ).getMetadataList();

    return null;
  }

  String getWiskiUnit()
  {
    return m_map.getProperty( "tsinfo_unitname", "<?>" );
  }

  String getWiskiParameterType()
  {
    return m_map.getProperty( "parametertype_name", "<?>" );
  }

  /**
   * tsinfo_timelevel: time series time step type <br>
   * 0: high resolution <br>
   * 1: daily <br>
   * 2: monthly <br>
   * 3: year <br>
   * 4: week <br>
   * 5: half year <br>
   * 255: other
   */
  int getWiskiTimeLevel()
  {
    return Integer.valueOf( m_map.getProperty( "tsinfo_timelevel", "255" ) ).intValue();
  }

  /**
   * tsinfo_valuetype: type of data values: <br>
   * 0: instantaneous <br>
   * 1: mean <br>
   * 2: sum <br>
   * 3: min <br>
   * 4: max <br>
   * 255: other
   */
  int getWiskiValueType()
  {
    return Integer.valueOf( m_map.getProperty( "tsinfo_valuetype", "255" ) ).intValue();
  }

  /**
   * @return tsinfo_begin_of als Date, dessen Time-Anteil den Beginn der Integrationszeit des Tageswertes Beschreibt
   *         (z.B. 07:30 oder ähnlich). Can be null.
   */
  Date getWiskiBegin()
  {
    final String strDate = m_map.getProperty( "tsinfo_begin_of" );
    if( strDate == null )
      return null;

    return new Date( Long.valueOf( strDate ).longValue() );
  }

  /**
   * @return tsinfo_offset_of als Long, welcher beschreibt, ob die Quellwerte eines Tageswertes zum Datum x vom Tag x
   *         bis x+1 einfliessen (offset 0) oder z.B. vom Tag x-1 bis zum Tag x (offset -1). Can be null.
   */
  Long getWiskiOffset()
  {
   // TODO: bei offsetverschiebung auch defaultwert berücksichtigen: auf -1 setzen 
    final String strOffset = m_map.getProperty( "tsinfo_offset_of", "0" );

    return Long.valueOf( strOffset );
  }

  /**
   * @return the internal id which is used within wiski. This id should not be used "outside of the program code"
   */
  Long getWiskiId()
  {
    return Long.valueOf( m_map.getProperty( "tsinfo_id", "-1" ) );
  }

  /**
   * @return the internal id of the parameter
   */
  Long getWiskiParameterId()
  {
    return Long.valueOf( m_map.getProperty( "stationparameter_id", "-1" ) );
  }

  String getWiskiIdAsString()
  {
    return m_map.getProperty( "tsinfo_id", "<?>" );
  }

  String getWiskiDescription()
  {
    final String noValue = "<kein Eintrag:";

    final StringBuffer bf = new StringBuffer();
    bf.append( m_map.getProperty( "parametertype_longname", noValue + "parametertype_longname>" ) ).append( " - " );
    bf.append( m_map.getProperty( "stationparameter_name", noValue + "stationparameter_name>" ) ).append( " - " );
    bf.append( m_map.getProperty( "stationparameter_longname", noValue + "stationparameter_longname>" ) )
        .append( " - " );
    bf.append( m_map.getProperty( "station_name", noValue + "station_name>" ) );

    return bf.toString();
  }

  /**
   * @return wiski internal station id
   */
  String getWiskiStationId()
  {
    return m_map.getProperty( "station_id", "<?>" );
  }

  /**
   * Return the station number (in german: Messstellennummer) in the Wiski sense.
   * <p>
   * This represents the third part of the Kalypso-Wiski-ID (GRUPPENART.PARAMETERNAME.MESSSTELLENNUMMER)
   */
  String getWiskiStationNo()
  {
    return m_map.getProperty( "station_no", "<?>" );
  }

  String getStationParameterName()
  {
    return m_map.getProperty( "stationparameter_name", "" );
  }
  
  /**
   * Return the name of the group/parameter. The group of a TsInfoItem is actually the Parameter in the Wiski sense.
   * <p>
   * This represents the second part of the Kalypso-Wiski-ID (GRUPPENART.PARAMETERNAME.MESSSTELLENNUMMER)
   */
  String getWiskiGroupName()
  {
    return m_group.getName();
  }

  String getWiskiStationName()
  {
    return m_map.getProperty( "station_name", "<?>" );
  }

  /**
   * Return the name of the supergroup/gruppenart. The supergroup is the top structuring element.
   * <p>
   * This represents the first part of the Kalypso-Wiski-ID (GRUPPENART.PARAMETERNAME.MESSSTELLENNUMMER)
   */
  String getWiskiSuperGroupName()
  {
    try
    {
      return m_group.getParent().getName();
    }
    catch( final RepositoryException e )
    {
      // can occur in extreme situations, so just print stack trace
      e.printStackTrace();

      return "<FEHLER IN SCHNITTSTELLE>";
    }
  }

  int getWiskiDistUnit()
  {
    final String strWiskiUnit = m_map.getProperty( "tsinfo_distunit" );
    if( strWiskiUnit == null )
      throw new IllegalStateException( "Wiski does not deliver which time-unit to use (Property: tsinfo_distunit)" );

   return Integer.valueOf( strWiskiUnit ).intValue();
  }

  int getWiskiDistValue()
  {
    final String strWiskiValue = m_map.getProperty( "tsinfo_distcount" );
    if( strWiskiValue == null )
      throw new IllegalStateException(
          "Wiski does not deliver which amount of the given time-unit to use (Property: tsinfo_distvalue)" );

    return Integer.valueOf( strWiskiValue ).intValue();
  }

  /**
   * Helper: finds a sibling timeserie (under same station) of the given parameter
   */
  TsInfoItem findSibling( final String parameterName ) throws RepositoryException
  {
    final SuperGroupItem supergroup = (SuperGroupItem)m_group.getParent();
    final GroupItem group = supergroup.findGroup( parameterName );

    if( group == null )
      throw new RepositoryException( "Could not find a sibling, parameter name is: " + parameterName
          + ", supergroup is: " + supergroup.getName() );

    return group.findTsInfo( "station_no", getWiskiStationNo() );
  }

  /**
   * @return true if the container is designed for forecasts
   */
  boolean isForecast()
  {
    return getWiskiSuperGroupName().indexOf( WiskiUtils.getProperty( "FORECAST_SUPERGROUP" ) ) != -1;
  }

  /**
   * @return the underlying map that holds the wiski properties
   */
  protected Map getWiskiPropertyMap()
  {
    return m_map;
  }
}
