package org.kalypso.wiskiadapter;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.lang.StringUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.wiskicall.GetTsInfoList;

/**
 * This item is adaptable into a wiski timeserie.
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
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    // return Wiski external name (not wiski intern id)
    // this is the discussed solution with Kisters
    // that is viable over time
    return m_rep.getIdentifier() + getName();
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

  String getWiskiName()
  {
    return m_map.getProperty( "stationparameter_longname", "<?>" );
  }

  /**
   * @return the id (string) which can be used outside of wiski in a persistent and viable manner. This id is actually
   *         defined by the administrator and is not likely to change for this timeserie.
   */
  String getWiskiCustomId()
  {
    return m_map.getProperty( "tsinfo_name", "<?>" );
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

  String getWiskiStationId()
  {
    return m_map.getProperty( "station_id", "<?>" );
  }

  /**
   * Helper: finds a sibling timeserie (under same station) of the given otherType. For instance if 'this' is a
   * timeserie named MyStation.Type.XX, then the timeserie named MyStation.otherType.XX is looked for.
   */
  TsInfoItem findSibling( final String otherType )
  {
    final String name = getWiskiCustomId();
    final String[] splits = name.split( "\\." );

    if( splits.length < 2 )
      return null;

    splits[1] = otherType;
    final String otherName = StringUtils.join( splits, '.' );
    final GetTsInfoList call = new GetTsInfoList( null, otherName );
    try
    {
      m_rep.executeWiskiCall( call );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }

    if( call.getResultList().size() > 0 )
    {
      final HashMap map = (HashMap)call.getResultList().get( 0 );
      return new TsInfoItem( m_group, map );
    }

    return null;
  }
}
