package de.kisters.wiski.webdataprovider.common.net;

import java.rmi.Naming;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.LinkedList;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * Data provider client example class for none wiskiwj clients.
 * 
 * @author cl, mk(modified)
 * @version $Revision$
 */
public final class KiWWClientSample
{
  /**
   * Creates a new <code>KiWWClientSample</code> instance.
   *  
   */
  private KiWWClientSample()
  {
  // empty
  }

  /**
   * test main.
   * 
   * @param args
   *          a <code>String[]</code> value
   */
  public static void main( String[] args )
  {
    try
    {
      //data range
//      Timestamp from = Timestamp.valueOf( "2003-01-01 00:00:00" );
//      Timestamp to = Timestamp.valueOf( "2003-06-01 00:00:00" );

      //final String rmiUrl =
      // "rmi:/hvz_test_srv.kisters.de:10991/KiWWDataProvider";
      final String rmiUrl = "rmi://193.23.163.115:10991/KiWWDataProvider";

      //create a server object
      KiWWDataProviderRMIf myServerObject = (KiWWDataProviderRMIf)Naming.lookup( rmiUrl );
      System.out.println( "SERVER_OBJECT: " + myServerObject );

      //test connection
      System.out.println( "ABOUT() =" + myServerObject.about() );

      String domain = "-";

      //login to the RMIServer
      HashMap auth = myServerObject.getUserAuthorisation( domain, "em", "123abc", "mydomain.kisters.de", null );
      System.out.println( "LOGIN =" + auth );

      if( ( null != auth ) && "1".equals( auth.get( KiWWDataProviderInterface.AUTHKEY_ALLOWED ) ) )
      {
        System.out.println( " login permitted" );

        //findOutColumnNames( myServerObject );

        //wiskiExample( myServerObject, auth, from, to );

        wqTest( myServerObject, auth );

        //logout
        myServerObject.logout( auth, null );
        System.out.println( "logout" );
      }
      else
      {
        System.out.println( "login denied" );
      }
    }
    catch( Exception e )
    {
      System.err.println( "error=" + e );
      e.printStackTrace();
    }
  }

  private static void wqTest( final KiWWDataProviderRMIf myServerObject, final HashMap auth )
  {
    try
    {
      final SimpleDateFormat sdf = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss" );
      final Timestamp ts = new Timestamp( sdf.parse( "2004-01-01 00:00:00" ).getTime() );
      final HashMap ratingTables = myServerObject.getRatingTables( auth, KiWWDataProviderInterface.OBJECT_TIMESERIES,
          new Long[]
          { new Long( 1024002488 ) }, ts );

      System.out.println( ratingTables );

      final String[] gettsinfo =
      {
          "tsinfo_id",
          "tsinfo_group_ident" };

      final SimpleRequestFilterTerm f1 = new SimpleRequestFilterTerm();
      f1.addColumnReference( "tsinfo_id" );
      f1.addOperator( "=" );
      f1.addValue( new Long( 1024002488 ) );

      final HashMap tsinfolist_group = myServerObject.getTsInfoList( auth, gettsinfo, null, f1, 15, 0, false, null );
      final LinkedList resultListinfo = (LinkedList)tsinfolist_group.get( "resultList" );

      System.out.println( resultListinfo );

      final String groupId = (String)( (HashMap)resultListinfo.getFirst() ).get( "tsinfo_group_ident" );

      final String[] groupinfo =
      {
          "group_id",
          "supergroup_name" };
      final SimpleRequestFilterTerm f3 = new SimpleRequestFilterTerm();
      f3.addColumnReference( "group_id" );
      f3.addOperator( "=" );
      f3.addValue( Long.valueOf( groupId ) );

      final HashMap groupList = myServerObject.getGroupList( auth, groupinfo,
          KiWWDataProviderInterface.TIMESERIES_GROUP, null, f3, 0, 0, false, null );
      System.out.println( groupList );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

//  private static void findOutColumnNames( final KiWWDataProviderRMIf myServerObject ) throws RemoteException,
//      KiWWException
//  {
//    displayColumns( "getGroupList", myServerObject );
//    displayColumns( "getStationList", myServerObject );
//    displayColumns( "getStationDetailList", myServerObject );
//    displayColumns( "getStationGroupList", myServerObject );
//    displayColumns( "getTsInfoList", myServerObject );
//  }

//  private static void displayColumns( final String method, final KiWWDataProviderRMIf myServerObject )
//      throws RemoteException, KiWWException
//  {
//    final String domain = "-";
//
//    final List columnNames = myServerObject.getColumnNames( domain, method );
//
//    System.out.println( "METHOD: " + method );
//
//    for( Iterator it = columnNames.iterator(); it.hasNext(); )
//      System.out.println( it.next() );
//
//    System.out.println( "-------------------------------" );
//  }

//  private static void wiskiExample( KiWWDataProviderRMIf myServerObject, HashMap auth, Timestamp from, Timestamp to )
//      throws RemoteException, KiWWException, InterruptedException
//  {
//    //getStationList filtered by station_name and station_group_name
//    String[] getsl = new String[]
//    {
//        "station_no",
//        "station_name",
//        "station_id",
//        "station_group_name",
//        "station_longname",
//        "station_group_ident",
//        "river_name", };
//    SimpleRequestFilterTerm filtersl = new SimpleRequestFilterTerm();
//    filtersl.addColumnReference( "station_name" );
//    filtersl.addOperator( "like" );
//    filtersl.addValue( "W%" );
//    filtersl.addOperator( "and" );
//    filtersl.addColumnReference( "station_group_name" );
//    filtersl.addOperator( "like" );
//    filtersl.addValue( "Stationen%" );
//
//    SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
//    sort.addColumnAscent( "station_name" );
//
//    HashMap stationlist = myServerObject.getStationList( auth, getsl, sort, filtersl, 15, 0, false, null );
//
//    System.out.println( "STATIONLIST =  " + stationlist );
//
//    //getStationDetailList for specified station_id (or station_id list)
//    String[] getdl = new String[]
//    {
//        "station_id",
//        "station_name",
//        "station_longname",
//        "station_shortname",
//        "station_carteasting",
//        "station_cartnorthing",
//        "station_valid_from",
//        "station_no",
//        "river_name" };
//
//    HashMap detaillist = myServerObject.getStationDetailList( auth, getdl, new Long[]
//    { new Long( 53949 ) }, null );
//
//    System.out.println( "STATIONDETAILLIST = " + detaillist );
//
//    //getTsData for specified timeseries (or ts list)
//    //Daten einer Zeitreihe abfragen
//    Timestamp from1 = Timestamp.valueOf( "2003-02-20 00:00:00" );
//    Timestamp to1 = Timestamp.valueOf( "2003-02-21 00:00:00" );
//
//    HashMap gettsdata = myServerObject.getTsData( auth, new long[]
//    { ( 512062925 ) }, from1, to1, null );
//    System.out.println( " TS_DATA " + gettsdata );
//    System.out.println( " TS_DATA(ts_id->512062925) = " );
//    gettsDatafromHash( gettsdata, "512062925" );
//
//    //  Daten zu einer Zeitreihe zurückschreiben
//    String tsid = "513824046";
//
//    //create essencial objects
//    HashMap timeseries_map = new HashMap();
//
//    HashMap tsID_map = new HashMap();
//
//    HashMap ts_values_map = new HashMap();
//
//    HashMap value_tsinfo_map = new HashMap();
//
//    LinkedList value_tscoldesc_ll = new LinkedList();
//    HashMap value_tscoldesc_map = new HashMap();
//
//    LinkedList value_tsdata_ll = new LinkedList();
//
//    LinkedHashMap value_tstamp_hash_lmap = new LinkedHashMap();
//
//    //set values
//    Timestamp timest1 = Timestamp.valueOf( "2005-01-01 00:00:00" );
//    Timestamp timest2 = Timestamp.valueOf( "2005-01-01 00:15:00" );
//    Timestamp timest3 = Timestamp.valueOf( "2005-01-01 00:30:00" );
//    Timestamp timest4 = Timestamp.valueOf( "2005-01-01 00:45:00" );
//    Timestamp timest5 = Timestamp.valueOf( "2005-01-01 01:00:00" );
//
//    HashMap value_tsdata_map1 = new HashMap();
//    HashMap value_tsdata_map2 = new HashMap();
//    HashMap value_tsdata_map3 = new HashMap();
//    HashMap value_tsdata_map4 = new HashMap();
//    HashMap value_tsdata_map5 = new HashMap();
//    value_tsdata_map1.put( "tsc_value0", new Double( 5.6 ) );
//    value_tsdata_map1.put( "status", new Long( 0 ) );
//    value_tsdata_map1.put( "timestamp", timest1 );
//    value_tsdata_map2.put( "tsc_value0", new Double( 8.2 ) );
//    value_tsdata_map2.put( "status", new Long( 0 ) );
//    value_tsdata_map2.put( "timestamp", timest2 );
//    value_tsdata_map3.put( "tsc_value0", new Double( 13.7 ) );
//    value_tsdata_map3.put( "status", new Long( 0 ) );
//    value_tsdata_map3.put( "timestamp", timest3 );
//    value_tsdata_map4.put( "tsc_value0", new Double( 19.2 ) );
//    value_tsdata_map4.put( "status", new Long( 0 ) );
//    value_tsdata_map4.put( "timestamp", timest4 );
//    value_tsdata_map5.put( "tsc_value0", new Double( 9.6 ) );
//    value_tsdata_map5.put( "status", new Long( 0 ) );
//    value_tsdata_map5.put( "timestamp", timest5 );
//
//    //compose setTsData HashMap
//    ts_values_map.put( KiWWDataProviderInterface.KEY_TSINFO, value_tsinfo_map );
//
//    value_tscoldesc_ll.add( value_tscoldesc_map );
//    ts_values_map.put( KiWWDataProviderInterface.KEY_TSCOLDESC, value_tscoldesc_ll );
//
//    value_tsdata_ll.add( value_tsdata_map1 );
//    value_tsdata_ll.add( value_tsdata_map2 );
//    value_tsdata_ll.add( value_tsdata_map3 );
//    value_tsdata_ll.add( value_tsdata_map4 );
//    value_tsdata_ll.add( value_tsdata_map5 );
//    ts_values_map.put( KiWWDataProviderInterface.KEY_TSDATA, value_tsdata_ll );
//    ts_values_map.put( KiWWDataProviderInterface.KEY_TSTAMP_HASH, value_tstamp_hash_lmap );
//    tsID_map.put( tsid, ts_values_map );
//    timeseries_map.put( KiWWDataProviderInterface.KEY_TIMESERIES, tsID_map );
//    System.out.println( "set TS_DATA = " + timeseries_map );
//
//    //setTsData
//    myServerObject.setTsData( auth, timeseries_map, null );
//
//    //getTsData for created ts object
//    HashMap getts_data_check = myServerObject.getTsData( auth, new long[]
//    { ( 513824046 ) }, Timestamp.valueOf( "2004-12-30 12:00:00" ), Timestamp.valueOf( "2005-01-02 12:00:00" ), null );
//
//    System.out.println( "TS_DATA(ts_id->" + tsid + ") = " );
//    gettsDatafromHash( getts_data_check, tsid );
//
//    //getGroupList filtered by supergroup_mame
//    SimpleRequestFilterTerm filtergroup = new SimpleRequestFilterTerm();
//    filtergroup.addColumnReference( "supergroup_name" );
//    filtergroup.addOperator( "like" );
//    filtergroup.addValue( "HVZ_Bodemodell" );
//
//    HashMap grouplist = myServerObject.getGroupList( auth, new String[]
//    {
//        "group_id",
//        "group_name" }, KiWWDataProviderInterface.TIMESERIES_GROUP, null, filtergroup, 15, 0, false, null );
//
//    LinkedList resultList = (LinkedList)grouplist.get( "resultList" );
//    int size = resultList.size();
//    Long[] ids = new Long[size];
//
//    for( int i = 0; i < size; i++ )
//    {
//      HashMap group = (HashMap)resultList.get( i );
//      ids[i] = new Long( (String)group.get( "group_id" ) );
//      System.out.println( "GROUPID = " + ids[i] );
//    }
//
//    //getTsInfoList filtered by group_ident
//    String[] gettsinfo = new String[]
//    {
//        "tsinfo_name",
//        "tsinfo_group_ident",
//        "tsinfo_group_name",
//        "stationparameter_name",
//        "tsinfo_unitname",
//        "tsinfo_distcount",
//        "tsinfo_distunit", };
//
//    int maxRows = 15;
//
//    SimpleRequestFilterTerm filter_ts_group = new SimpleRequestFilterTerm();
//    filter_ts_group.addColumnReference( "tsinfo_group_ident" );
//    filter_ts_group.addOperator( "in" );
//    filter_ts_group.addValue( ids ); //List of GroupId's from getGroupList
//
//    HashMap tsinfolist_group = myServerObject.getTsInfoList( auth, gettsinfo, null, filter_ts_group, maxRows, 0, false,
//        null );
//    LinkedList resultListinfo = (LinkedList)tsinfolist_group.get( "resultList" );
//    size = resultListinfo.size();
//
//    System.out.println( "tsinfo_group_ident" + "   " + "tsinfo_group_name" + "   " + "tsinfo_name" + "   "
//        + "stationparameter_name" + "   " + "tsinfo_unitname" + "   " + "tsinfo_distcount" + "   " + "tsinfo_distunit" );
//
//    for( int i = 0; i < size; i++ )
//    {
//      HashMap info = (HashMap)resultListinfo.get( i );
//      System.out.println( "TSInfo = " + info.get( "tsinfo_group_ident" ) + "   " + info.get( "tsinfo_group_name" )
//          + "   " + info.get( "tsinfo_name" ) + "   " + info.get( "stationparameter_name" ) + "   "
//          + info.get( "tsinfo_unitname" ) + "   " + info.get( "tsinfo_distcount" ) + "   "
//          + info.get( "tsinfo_distunit" ) );
//    }
//
//    //Zeitreihen-ID (welche für die Prognose relevant ist) in Wiski interne
//    // Nummer umwandeln
//    gettsinfo = new String[]
//    {
//        "tsinfo_name",
//        "tsinfo_id" };
//
//    maxRows = 15;
//
//    filter_ts_group = new SimpleRequestFilterTerm();
//    filter_ts_group.addColumnReference( "tsinfo_name" );
//    filter_ts_group.addOperator( "like" );
//    filter_ts_group.addValue( "Weferlingen.N.S.P.Wert" );
//
//    HashMap tsinfolist = myServerObject.getTsInfoList( auth, gettsinfo, null, filter_ts_group, maxRows, 0, false, null );
//    resultListinfo = (LinkedList)tsinfolist.get( "resultList" );
//    size = resultListinfo.size();
//
//    System.out.println( "tsinfo_name" + "   " + "tsinfo_id" );
//
//    for( int i = 0; i < size; i++ )
//    {
//      HashMap info = (HashMap)resultListinfo.get( i );
//      System.out.println( "TSInfo = " + info.get( "tsinfo_name" ) + "   " + info.get( "tsinfo_id" ) );
//    }
//
//    System.out.println( "Zeitreihen-ID (welche für die Prognose relevant ist) in Wiski interne Nummer umwandeln :" );
//
//    //isTsWritable
//    HashMap writable = myServerObject.isTsWritable( auth, new Long[]
//    {
//        new Long( 512062925 ),
//        new Long( 512203441 ) }, null );
//    System.out.println( "ISWRITABLE = " + writable );
//
//    //getObjectComments
//    HashMap hmMain = new HashMap();
//    HashMap getcomment = new HashMap();
//
//    hmMain.put( new Long( 513695107 ), KiWWDataProviderInterface.OBJECT_TIMESERIES );
//    hmMain.put( new Long( 512810190 ), KiWWDataProviderInterface.OBJECT_TIMESERIES );
//    hmMain.put( new Long( 512062925 ), KiWWDataProviderInterface.OBJECT_TIMESERIES );
//
//    getcomment = myServerObject.getObjectComments( auth, hmMain, from, to, null );
//    System.out.println( "GETCOMMENT = " + getcomment );
//
//    //setObjectComments
//    HashMap hmMainmap = new HashMap();
//    HashMap hmBlock = new HashMap();
//    LinkedList llRems = new LinkedList();
//
//    hmMainmap.put( new Long( 512062925 ), hmBlock );
//
//    hmBlock.put( KiWWDataProviderInterface.KEY_OBJECT_TYPE, KiWWDataProviderInterface.OBJECT_TIMESERIES );
//    hmBlock.put( KiWWDataProviderInterface.KEY_COMMENTS, llRems );
//
//    HashMap hmRem = new HashMap();
//    hmRem.put( "remark", "Test comment one" );
//    hmRem.put( "fromdate", Timestamp.valueOf( "2004-03-10 12:00:00" ) );
//    hmRem.put( "untildate", Timestamp.valueOf( "2004-03-20 12:00:00" ) );
//    llRems.add( hmRem );
//
//    hmRem = new HashMap();
//    hmRem.put( "remark", "Test comment two" );
//    hmRem.put( "fromdate", Timestamp.valueOf( "2003-03-10 12:00:00" ) );
//    hmRem.put( "untildate", Timestamp.valueOf( "2003-03-16 12:00:00" ) );
//    llRems.add( hmRem );
//
//    boolean isset = myServerObject.setObjectComments( auth, hmMainmap, null );
//    System.out.println( "SETCOMMENT = " + isset );
//
//    //check the written comment
//    HashMap hmMain1 = new HashMap();
//    HashMap getcomment1 = new HashMap();
//
//    hmMain1.put( new Long( 512062925 ), KiWWDataProviderInterface.OBJECT_TIMESERIES );
//
//    getcomment1 = myServerObject.getObjectComments( auth, hmMain1, Timestamp.valueOf( "2003-03-10 12:00:00" ),
//        Timestamp.valueOf( "2003-03-16 12:00:00" ), null );
//    System.out.println( "GETCOMMENT = " + getcomment1 );
//
//  }

  public static void gettsDatafromHash( HashMap map, String tsid )
  {
    HashMap timeseries = (HashMap)map.get( KiWWDataProviderInterface.KEY_TIMESERIES );
    HashMap ts_id = (HashMap)timeseries.get( tsid );

    LinkedList tsdata = (LinkedList)ts_id.get( KiWWDataProviderInterface.KEY_TSDATA );
    int i = 0;

    i = tsdata.size();

    if( i > 0 )
    {
      for( int a = 0; a < i; a++ )
      {
        System.out.println( tsdata.get( a ) );
      }
    }
  }
}
