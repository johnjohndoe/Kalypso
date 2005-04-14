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
 * StationItem
 * 
 * @author schlienger
 */
public class StationItem implements IRepositoryItem
{
  private static final String[] COLUMNS_PARAMETER = { "tsinfo_id",
      "tsinfo_name", "tsinfo_timelevel", "tsinfo_valuetype", "tsinfo_unitname",
      "stationparameter_name", "stationparameter_longname", "station_name",
      "station_no", "parametertype_name", "parametertype_longname",
      "tsinfo_group_name" };

  private final WiskiRepository m_rep;

  private final String m_no;

  private final String m_id;

  private final String m_name;

  private final GroupItem m_group;

  public StationItem( final WiskiRepository rep, final GroupItem group,
      final String no, final String id, final String name )
  {
    m_rep = rep;
    m_group = group;
    m_no = no;
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
    return m_group;
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
    filter.addColumnReference( "station_no" );
    filter.addOperator( "like" );
    filter.addValue( m_no );

    final SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "stationparameter_longname" );

    try
    {
      final HashMap tsinfolist = m_rep.getWiski().getTsInfoList(
          m_rep.getUserData(), COLUMNS_PARAMETER, sort, filter, 15, 0, false,
          null );

      final List resultList = (List) tsinfolist
          .get( KiWWDataProviderInterface.KEY_RESULT_LIST );
      final StationParameter[] params = new StationParameter[resultList.size()];

      int i = 0;
      for( final Iterator it = resultList.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();
        params[i++] = new StationParameter( m_rep, this, (String) map
            .get( "tsinfo_id" ), (String) map.get( "tsinfo_name" ),
            (String) map.get( "tsinfo_unitname" ), (String) map
                .get( "parametertype_name" ) );
      }

      return params;
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
    // not adaptable
    return null;
  }
}
