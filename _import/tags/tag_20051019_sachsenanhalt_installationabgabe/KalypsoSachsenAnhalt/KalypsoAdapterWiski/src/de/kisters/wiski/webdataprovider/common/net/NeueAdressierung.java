package de.kisters.wiski.webdataprovider.common.net;

import java.util.HashMap;
import java.util.LinkedList;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * @author butz
 */
public class NeueAdressierung
{
  public static HashMap getTimeseriesByKalypsoID( final String kalypsoAdress, final KiWWDataProviderRMIf myServerObject, final HashMap ud )
  {
    HashMap Groups = null;
    LinkedList resultSet = null;
    HashMap resultMap = new HashMap();
    String ids[] = kalypsoAdress.split( "\\." );
    resultMap.put( "TS-ID", new Long( 0 ) );
    resultMap.put( "TS-NAME", "UNDEF" );
    resultMap.put( "TS-CODE", new Long( 5 ) );
    /* First view on correct Id-Structure */

    if( ids.length != 3 )
    {
      return resultMap;
    }
    /* Split Id-Structure into its parts */
    String supergroupName = ids[0];
    String GroupName = ids[1];
    Long mstNr = new Long( ids[2] );

    Long groupID = new Long( -1 );

    try
    {
      /* Obtain esxactly on Timeseries-Group matching specifications of Kalypso */
      String[] getsl = new String[]
      { "group_id", "group_name" };
      SimpleRequestFilterTerm filter_ts_group = new SimpleRequestFilterTerm();
      filter_ts_group.addColumnReference( "group_name" );
      filter_ts_group.addOperator( "=" );
      filter_ts_group.addValue( GroupName );
      filter_ts_group.addOperator( "and" );
      filter_ts_group.addColumnReference( "supergroup_name" );
      filter_ts_group.addOperator( "=" );
      filter_ts_group.addValue( supergroupName );
      Groups = myServerObject.getGroupList( ud, getsl, KiWWDataProviderInterface.TIMESERIES_GROUP, null,
          filter_ts_group, 5, 0, false, null ); //Nothing returned at all
      if( null == Groups )
      {
        resultMap.put( "TS-CODE", new Long( 1 ) );
        return resultMap;
      }
      resultSet = (LinkedList)Groups.get( "resultList" ); //No resultSet returned
      if( null == resultSet )
      {
        resultMap.put( "TS-CODE", new Long( 1 ) );
        return resultMap;
      }
      //Resultset not exactly matching my expectations
      if( resultSet.size() != 1 )
      {
        resultMap.put( "TS-CODE", new Long( 2 ) );
        return resultMap;
      }
      //Picking out my groupInfos
      for( int i = 0; i < resultSet.size(); i++ )
      {
        HashMap entry = (HashMap)resultSet.get( i );
        System.out.println( (String)entry.get( "group_id" ) + ": " + (String)entry.get( "group_name" ) );
        groupID = new Long( (String)entry.get( "group_id" ) );
        break;
      }
      //Now Getting all Timeseries in Group
      LinkedList TSList = null;
      TSList = myServerObject.getGroupEntryList( ud, KiWWDataProviderInterface.TIMESERIES_GROUP, groupID.longValue(),
          null ); //Didn't get any Returnvalue at all
      if( null == TSList )
      {
        resultMap.put( "TS-CODE", new Long( 4 ) );
        return resultMap;
      }
      //Loop through the Timeseries-Array of actual group
      for( int i = 0; i < TSList.size(); i++ )
      {
        Long vl_id = (Long)TSList.get( i );
        String[] getlist = new String[]
        { "tsinfo_name", "tsinfo_id" };
        SimpleRequestFilterTerm filter_ts = new SimpleRequestFilterTerm();
        filter_ts.addColumnReference( "tsinfo_id" );
        filter_ts.addOperator( "=" );
        filter_ts.addValue( vl_id );
        filter_ts.addOperator( "and" );
        filter_ts.addColumnReference( "station_no" );
        filter_ts.addOperator( "=" );
        filter_ts.addValue( mstNr );
        HashMap res = myServerObject.getTsInfoList( ud, getlist, null, filter_ts, 10, 0, false, null );
        if( null == res )
        {
          //Better luck next time
          continue;
        }
        resultSet = (LinkedList)res.get( "resultList" );
        if( resultSet.size() < 1 )
        {
          //Better luck next time
          continue;
        }
        //More than a single timeseries found
        if( resultSet.size() > 1 )
        {
          resultMap.put( "TS-CODE", new Long( 4 ) );
          return resultMap;
        }
        //That's what we were looking for
        for( int k = 0; k < resultSet.size(); k++ )
        {
          HashMap entry = (HashMap)resultSet.get( k );
          resultMap.put( "TS-ID", new Long( (String)entry.get( "tsinfo_id" ) ) );
          resultMap.put( "TS-NAME", entry.get( "tsinfo_name" ) );

          resultMap.put( "TS-CODE", new Long( 0 ) );
          return resultMap;
        }
      }// Ende TimeseriesLoop, but nothing found obviously
      resultMap.put( "TS-CODE", new Long( 4 ) );
      return resultMap;
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return resultMap;
  }
}
