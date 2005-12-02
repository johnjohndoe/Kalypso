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
 * GetTsInfoList
 * 
 * @author schlienger
 */
public class GetTsInfoList implements IWiskiCall
{
  /** Columns of TSINFO */
  public final static String[] COLUMNS =
  {
      "tsinfo_id",
      "tsinfo_name",
      "tsinfo_group_ident",
      "tsinfo_unitname",
      "tsinfo_distunit",
      "tsinfo_distcount",
      "tsinfo_precision",
      "tsinfo_timelevel",
      "tsinfo_valuetype",
      "parametertype_name",
      "parametertype_longname",
      "stationparameter_id",
      "stationparameter_name",
      "stationparameter_longname",
      "station_id",
      "station_no",
      "station_name" };

  private final SimpleRequestFilterTerm filter;

  private final SimpleRequestSortTerm sort;

  private List resultList;

  /**
   * Constructor with groupId. All tsInfoList objects in the group will be fetched in the wiski database.
   */
  public GetTsInfoList( final String groupId )
  {
    filter = new SimpleRequestFilterTerm();
    filter.addColumnReference( "tsinfo_group_ident" );
    filter.addOperator( "like" );
    filter.addValue( groupId );

    sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "tsinfo_name" );
  }

  /**
   * Constructor with a property-name and a value to look for within the given group
   */
  public GetTsInfoList( final String groupId, final String property, final String value )
  {
    filter = new SimpleRequestFilterTerm();

    filter.addColumnReference( "tsinfo_group_ident" );
    filter.addOperator( "like" );
    filter.addValue( groupId );

    filter.addOperator( "and" );
    
    filter.addColumnReference( property );
    filter.addOperator( "like" );
    filter.addValue( value );

    sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "tsinfo_name" );
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    final HashMap tsinfolist = wiski.getTsInfoList( userData, COLUMNS, sort, filter, 0, 0, false, null );

    resultList = (List)tsinfolist.get( KiWWDataProviderInterface.KEY_RESULT_LIST );
  }

  public List getResultList()
  {
    return resultList;
  }
}
