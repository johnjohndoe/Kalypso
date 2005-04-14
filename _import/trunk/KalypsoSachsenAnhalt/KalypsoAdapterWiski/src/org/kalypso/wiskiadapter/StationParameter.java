package org.kalypso.wiskiadapter;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

/**
 * StationParameter
 * 
 * @author schlienger
 */
public class StationParameter implements IRepositoryItem
{
  private final WiskiRepository m_rep;

  private final StationItem m_station;

  private final String m_id;

  private final String m_name;

  private final String m_unit;

  private final String m_type;

  private MetadataList m_metadata = null;

  public StationParameter( final WiskiRepository rep,
      final StationItem station, final String id, final String name,
      final String unit, final String type )
  {
    m_rep = rep;
    m_station = station;
    m_id = id;
    m_name = name;
    m_unit = unit;
    m_type = type;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_rep.getIdentifier() + m_id;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( ) throws RepositoryException
  {
    return m_station;
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
      return new WiskiTimeserie( this );
    }

    return null;
  }

  /**
   * @return wiski id
   */
  public String getWiskiId( )
  {
    return m_id;
  }

  /**
   * @return metadalist for that parameter
   */
  MetadataList getMetadataList( )
  {
    if( m_metadata == null )
    {
      m_metadata = new MetadataList();

      fetchAlarmLevels( m_metadata );

      //      fetchWQTable( m_metadata );
    }

    return m_metadata;
  }

  //  private void fetchWQTable( MetadataList metadata )
  //  {
  //    // TODO WQ-Table holen, Frage: welche Datum muss man mitgeben?
  //          final HashMap wqt = m_rep.getWiski().getRatingTables(
  //              m_rep.getUserData(), KiWWDataProviderInterface.OBJECT_STATION,
  //              new Long[] { new Long( id ) }, curvets );
  //  }

  /**
   * Helper for translating wiski alarm levels into kalypso metadata
   */
  private void fetchAlarmLevels( final MetadataList md )
  {
    try
    {
      final HashMap alarmlevel = m_rep.getWiski().getAlarmLevelList(
          m_rep.getUserData(),
          new Long[] { new Long( Long.parseLong( m_id ) ) }, null );

      final List al = (List) alarmlevel.get( m_id );
      for( final Iterator it = al.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();

        final String level = (String) map.get( "epv_name" );
        final String value = (String) map.get( "epv_value" );

        md.put( WiskiUtils.wiskiMetadataName2Kalypso( level ), value );
      }
    }
    catch( Exception e ) // RemoteException, KiWWException
    {
      e.printStackTrace();
    }
  }

  /**
   * @return the wiski parameter unit
   */
  String getWiskiUnit( )
  {
    return m_unit;
  }

  /**
   * @return the wiski parameter type
   */
  String getWiskiType( )
  {
    return m_type;
  }
}
