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
  /** Columns of TSINFO */
  public final static String[] COLUMNS = { "tsinfo_id", "tsinfo_name",
      "tsinfo_group_ident", "tsinfo_unitname", "tsinfo_distunit",
      "tsinfo_distcount", "tsinfo_precision", "tsinfo_timelevel",
      "tsinfo_valuetype", "parametertype_name", "parametertype_longname",
      "stationparameter_id", "stationparameter_name",
      "stationparameter_longname", "station_id", "station_no", "station_name" };

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

  /**
   * @return the wiski parameter unit
   */
  String getWiskiUnit( )
  {
    return m_map.getProperty( "tsinfo_unitname" );
  }

  /**
   * @return the wiski parameter type
   */
  String getWiskiType( )
  {
    return m_map.getProperty( "parametertype_name" );
  }

  /**
   * @return wiski id
   */
  String getWiskiId( )
  {
    return m_map.getProperty( "tsinfo_id" );
  }
}
