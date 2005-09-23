package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.List;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * GetSuperGroupList
 * 
 * @author schlienger
 */
public class GetSuperGroupList implements IWiskiCall
{
  /** columns of GROUP */
  public static final String[] COLUMNS =
  { "supergroup_name" };

  private final SimpleRequestFilterTerm filtergroup;

  private final SimpleRequestSortTerm sort;

  private List resultList;

  public GetSuperGroupList( final String superGroupName )
  {
    filtergroup = new SimpleRequestFilterTerm();
    filtergroup.addColumnReference( "supergroup_name" );
    filtergroup.addOperator( "like" );
    filtergroup.addValue( superGroupName );

    sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "supergroup_name" );
  }

  public GetSuperGroupList( final String[] superGroupNames )
  {
    filtergroup = new SimpleRequestFilterTerm();

    for( int i = 0; i < superGroupNames.length; i++ )
    {
      filtergroup.addColumnReference( "supergroup_name" );
      filtergroup.addOperator( "like" );
      filtergroup.addValue( superGroupNames[i] );
    }

    sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "supergroup_name" );
  }

  public void execute( final KiWWDataProviderRMIf wiski, final HashMap userData ) throws NoSuchObjectException,
      KiWWException, RemoteException
  {
    final HashMap superGrouplist = wiski.getGroupList( userData, COLUMNS, KiWWDataProviderInterface.TIMESERIES_GROUP,
        sort, filtergroup, 0, 0, false, null );

    resultList = (List)superGrouplist.get( KiWWDataProviderInterface.KEY_RESULT_LIST );
  }

  public List getResultList()
  {
    return resultList;
  }
}
