package org.kalypso.wiskiadapter;

import java.util.Map;
import java.util.Properties;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * TsItem
 * 
 * @author schlienger
 */
public class TsInfoItem implements IRepositoryItem
{
  private final GroupItem m_group;

  private final WiskiRepository m_rep;

  /** maps the column-names to their values for this TsInfo item */
  private final Properties m_map;

  private WiskiTimeserie m_ts = null;

  public TsInfoItem( final GroupItem item, final Map map )
  {
    m_group = item;
    m_map = new Properties();
    m_map.putAll( map );

    m_rep = (WiskiRepository) m_group.getRepository();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_map.getProperty( "tsinfo_name" );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    // return Wiski external name (not wiski intern id)
    // this is the discussed solution with Kisters
    // that is viable over time
    return m_rep.getIdentifier() + getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( ) throws RepositoryException
  {
    return m_group;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository( )
  {
    return m_rep;
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class anotherClass )
  {
    if( anotherClass == IObservation.class )
    {
      if( m_ts == null )
        m_ts = new WiskiTimeserie( this );

      return m_ts;
    }

    return null;
  }

  String getWiskiUnit( )
  {
    return m_map.getProperty( "tsinfo_unitname" );
  }

  String getWiskiType( )
  {
    return m_map.getProperty( "parametertype_name" );
  }

  Long getWiskiId( )
  {
    return Long.valueOf( m_map.getProperty( "tsinfo_id" ) );
  }

  String getWiskiIdAsString( )
  {
    return m_map.getProperty( "tsinfo_id" );
  }

  String getWiskiName( )
  {
    return m_map.getProperty( "tsinfo_name" );
  }

  String getWiskiDescription( )
  {
    final StringBuffer bf = new StringBuffer();
    bf.append( m_map.getProperty( "parametertype_longname" ) ).append( " - " );
    bf.append( m_map.getProperty( "stationparameter_name" ) ).append( " - " );
    bf.append( m_map.getProperty( "stationparameter_longname" ) )
        .append( " - " );
    bf.append( m_map.getProperty( "station_name" ) );

    return bf.toString();
  }
  
  String getWiskiStationId()
  {
    return m_map.getProperty( "station_id" );
  }
}
