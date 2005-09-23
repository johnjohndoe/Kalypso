package org.kalypso.wiskiadapter;

import java.util.Calendar;
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

  String getWiskiType()
  {
    return m_map.getProperty( "parametertype_name", "<?>" );
  }

  /**
   * @return the internal id which is used within wiski. This id should not be used "outside of the program code"
   */
  Long getWiskiId()
  {
    return Long.valueOf( m_map.getProperty( "tsinfo_id", "<?>" ) );
  }

  String getWiskiIdAsString()
  {
    return m_map.getProperty( "tsinfo_id", "<?>" );
  }

  String getWiskiDescription()
  {
    final StringBuffer bf = new StringBuffer();
    bf.append( m_map.getProperty( "parametertype_longname" ) ).append( " - " );
    bf.append( m_map.getProperty( "stationparameter_name" ) ).append( " - " );
    bf.append( m_map.getProperty( "stationparameter_longname" ) ).append( " - " );
    bf.append( m_map.getProperty( "station_name" ) );

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

  /**
   * Return the name of the group/parameter. The group of a TsInfoItem is actually the Parameter in the Wiski sense.
   * <p>
   * This represents the second part of the Kalypso-Wiski-ID (GRUPPENART.PARAMETERNAME.MESSSTELLENNUMMER)
   */
  String getWiskiGroupName()
  {
    return m_group.getName();
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

  int getWiskiDistUnitAsCalendarField()
  {
    final String strWiskiUnit = m_map.getProperty( "tsinfo_distunit" );
    if( strWiskiUnit == null )
      throw new IllegalStateException( "Wiski does not deliver which time-unit to use (Property: tsinfo_distunit)" );

    final int wiskiUnit = Integer.valueOf( strWiskiUnit ).intValue();
    switch( wiskiUnit )
    {
      case 1:
        return Calendar.SECOND;
      case 2:
        return Calendar.MINUTE;
      case 3:
        return Calendar.HOUR_OF_DAY;
      case 4:
        return Calendar.DAY_OF_YEAR;
      default:
        throw new IllegalStateException( "Cannot translate Wiski property tsinfo_distunit into Calendar-Field" );
    }
  }

  int getWiskiDistValue()
  {
    final String strWiskiValue = m_map.getProperty( "tsinfo_distvalue" );
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
    
    return group.findTsInfo( "station_no", getWiskiStationNo() );
  }
  
  /**
   * @return true if the container is designed for forecasts
   */
  boolean isForecast()
  {
    return getWiskiSuperGroupName().indexOf( WiskiUtils.getProperty( "FORECAST_SUPERGROUP" ) ) != -1;
  }
}
