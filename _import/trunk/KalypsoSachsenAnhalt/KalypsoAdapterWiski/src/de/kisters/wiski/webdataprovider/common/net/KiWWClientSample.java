package de.kisters.wiski.webdataprovider.common.net;

import java.rmi.Naming;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * Data provider client example class for none wiskiwj clients.
 * 
 * @author cl, mk(modified)
 * @version $Revision$
 */
public final class KiWWClientSample
{

  static Logwriter log = new Logwriter();

  /**
   * Creates a new <code>KiWWClientSample</code> instance.
   *  
   */
  private KiWWClientSample( )
  {
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

      Timestamp from = null;
      Timestamp to = null;

      //data range
      from = Timestamp.valueOf( "1995-01-01 00:00:00" );
      to = Timestamp.valueOf( "1995-03-28 00:00:00" );

      String rmiUrl = null;

      try
      {
        if( null != args[0] )
          rmiUrl = args[0];
      }
      catch( ArrayIndexOutOfBoundsException arex )
      {
        rmiUrl = "rmi://10.123.123.66:10991/KiWWDataProvider";
      }
      
      BCEHelper.configureProxy( "", "" );

      KiWWDataProviderRMIf myServerObject = (KiWWDataProviderRMIf) Naming
          .lookup( rmiUrl );
      System.out.println( " about()=" + myServerObject.about() );
      log.add_entry( " about()=" + myServerObject.about() );
      String domain = "-";
      HashMap ud = new HashMap();
      ud.put( "domain", domain );
      ud.put( "logonName", "em" );
      ud.put( "password", "123abc" );
      ud.put( "language", "de" );

      HashMap auth = myServerObject.getUserAuthorisation( domain, "em",
          "123abc", "myhost.kisters.de" );
      System.out.println( " login=" + auth );
      log.add_entry( " login=" + auth );

      if( null != auth
          && "1".equals( auth.get( KiWWDataProviderInterface.AUTHKEY_ALLOWED ) ) )
      {

        System.out.println( " login permitted" );
        log.add_entry( " login permitted" );

        String[] getsl = new String[] { "station_no", "station_name",
            "station_id" };
        String[] getdl = new String[] { "station_id", "station_name",
            "station_longname", "station_shortname", "station_carteasting",
            "station_cartnorthing", "station_valid_from", "station_no",
            "river_name" };
        String[] gettsinfo = new String[] { "tsinfo_id", "tsinfo_name",
            "tsinfo_timelevel", "tsinfo_valuetype", "tsinfo_unitname",
            "stationparameter_name", "stationparameter_longname",
            "station_name", "station_no", "parametertype_name" };

        SimpleRequestFilterTerm filterts = new SimpleRequestFilterTerm();
        filterts.addColumnReference( "station_name" );
        filterts.addOperator( "like" );
        filterts.addValue( "C%" );
        filterts.addOperator( "and" );
        filterts.addColumnReference( "station_no" );
        filterts.addOperator( "like" );
        filterts.addValue( "432324" );
        filterts.addOperator( "and" );
        filterts.addColumnReference( "tsinfo_name" );
        filterts.addOperator( "like" );
        filterts.addValue( "432324.SG.DayMean" );

        SimpleRequestFilterTerm filtersl = new SimpleRequestFilterTerm();
        filtersl.addColumnReference( "station_name" );
        filtersl.addOperator( "like" );
        filtersl.addValue( "Cod%" );

        SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
        sort.addColumnAscent( "station_name" );

        HashMap stationlist = myServerObject.getStationList( ud, getsl, sort,
            filtersl, 15, 0, false );

        HashMap detaillist = myServerObject.getStationDetailList( ud, getdl,
            new Long[] { new Long( 447491 ) } );

        HashMap tsinfolist = myServerObject.getTsInfoList( ud, gettsinfo, null,
            filterts, 15, 0, false );

        HashMap gettsdata = myServerObject.getTsData( ud,
            new long[] { (447528) }, from, to );

        //output
        System.out.println( " stationlist= " + stationlist );
        log.add_entry( " stationlist= " + stationlist );
        System.out.println( " stationdetaillist= " + detaillist );
        log.add_entry( " stationdetaillist= " + detaillist );
        System.out.println( " ts_infolist= " + tsinfolist );
        log.add_entry( " ts_infolist= " + tsinfolist );
        System.out.println( " ts_data(ts_id->447528)= " );
        log.add_entry( " ts_data(ts_id->447528)= " );
        gettsDatafromHash( gettsdata );

        // logout
        System.out.println( " logout" );
        log.add_entry( " logout" );

        log.save_log();
        myServerObject.logout( domain, "em" );
      }

    }
    catch( Exception e )
    {
      System.err.println( "error=" + e );
      e.printStackTrace();

      log.save_log();
    }

  }

  /**
   * extract timeseries data from HashMap
   * 
   * @param map
   *          HashMap created by <code>KiWWDataProviderRMIf.getTsData()</code>
   */
  public static void gettsDatafromHash( HashMap map )
  {
    Object returnvalue = null;
    String tmp = "";
    HashMap timeseries = (HashMap) map.get( "timeseries" );
    HashMap ts_id = (HashMap) timeseries.get( "447528" );

    LinkedList tsdata = (LinkedList) ts_id.get( "tsdata" );
    int i = 0;

    i = tsdata.size();

    if( i > 0 )
    {
      for( int a = 0; a < i; a++ )
      {
        System.out.println( tsdata.get( a ) );
        log.add_entry( "" + tsdata.get( a ) );
      }
    }

  }
} // eof
