package org.kalypso.wiskiadapter;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;

/**
 * GroupItem
 * 
 * @author schlienger
 */
public class GroupItem implements IRepositoryItem
{
  /** Interesting columns for querying stations */
  private final static String[] COLUMNS_STATION = { "station_no",
      "station_name", "station_id", "station_longname", "river_name",
      "station_group_ident" };

  private final WiskiRepository m_rep;

  private final String m_id;

  private final String m_name;

  public GroupItem( final WiskiRepository rep, final String id,
      final String name )
  {
    m_rep = rep;
    m_id = id;
    m_name = name;
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
    return m_rep;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    final SimpleRequestFilterTerm filter = new SimpleRequestFilterTerm();
    filter.addColumnReference( "station_group_ident" );
    filter.addOperator( "like" );
    filter.addValue( m_id );

    final SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "station_name" );

    try
    {
      final HashMap stationlist = m_rep.getWiski().getStationList(
          m_rep.getUserData(), COLUMNS_STATION, sort, filter, 15, 0, false,
          null );

      //    String[] getdl = new String[] { "station_id", "station_name",
      //        "station_longname", "station_shortname", "station_carteasting",
      //        "station_cartnorthing", "station_valid_from", "station_no",
      //        "river_name" };
      //
      //    HashMap detaillist = myServerObject.getStationDetailList( ud, getdl,
      //        new Long[] { new Long( 53949 ) }, null );

      final List resultList = (List) stationlist
          .get( KiWWDataProviderInterface.KEY_RESULT_LIST );
      final StationItem[] stations = new StationItem[resultList.size()];

      int i = 0;
      for( final Iterator it = resultList.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();
        stations[i++] = new StationItem( m_rep, this, 
            (String) map.get( "station_id" ), (String) map.get( "station_name" ) );
      }

      return stations;
    }
    catch( final Exception e ) // KiWWException and RemoteException
    {
      e.printStackTrace();
      throw new RepositoryException( e );
    }
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
  public Object getAdapter( Class anotherClass )
  {
    // nicht adaptable
    return null;
  }
}
