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
 * GetGroupList
 * 
 * @author schlienger
 */
public class GetGroupList implements IWiskiCall
{
  /** columns of GROUP */
  public static final String[] COLUMNS =
  {
      "group_id",
      "group_name" };

  private final SimpleRequestFilterTerm filtergroup;

  private final SimpleRequestSortTerm sort;

  private List resultList;

  public GetGroupList( final String superGroupName )
  {
    filtergroup = new SimpleRequestFilterTerm();
    filtergroup.addColumnReference( "supergroup_name" );
    filtergroup.addOperator( "like" );
    filtergroup.addValue( superGroupName );

    sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "group_name" );
  }
  
  public GetGroupList( final String superGroupName, final String groupName )
  {
    filtergroup = new SimpleRequestFilterTerm();
    filtergroup.addColumnReference( "supergroup_name" );
    filtergroup.addOperator( "like" );
    filtergroup.addValue( superGroupName );
    
    filtergroup.addColumnReference( "group_name" );
    filtergroup.addOperator( "like" );
    filtergroup.addValue( groupName );

    sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "group_name" );
  }

  public void execute( final KiWWDataProviderRMIf wiski, final HashMap userData ) throws NoSuchObjectException,
      KiWWException, RemoteException
  {
    final HashMap grouplist = wiski.getGroupList( userData, COLUMNS, KiWWDataProviderInterface.TIMESERIES_GROUP, sort,
        filtergroup, 0, 0, false, null );

    resultList = (List)grouplist.get( KiWWDataProviderInterface.KEY_RESULT_LIST );
  }

  public List getResultList()
  {
    return resultList;
  }
}
