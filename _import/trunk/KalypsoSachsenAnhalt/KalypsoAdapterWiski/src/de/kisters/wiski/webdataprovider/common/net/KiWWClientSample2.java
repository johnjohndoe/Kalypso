package de.kisters.wiski.webdataprovider.common.net;

import java.rmi.Naming;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
public final class KiWWClientSample2 {
    /**
     * Creates a new <code>KiWWClientSample</code> instance.
     *
     */
    private KiWWClientSample2() {
      // empty
    }

    /**
     * test main.
     * @param args a <code>String[]</code> value
     */
    public static void main(String[] args) {
        try {
            //data range
            Timestamp from = Timestamp.valueOf("2002-01-01 00:00:00");
            Timestamp to = Timestamp.valueOf("2002-01-01 12:00:00");
            Timestamp curvets = Timestamp.valueOf("2005-01-01 00:00:00");

            String rmiUrl = null;

            //set Server URL
            try {
                if (null != args[0]) {
                    rmiUrl = args[0];
                }
            } catch (ArrayIndexOutOfBoundsException arex) {
                rmiUrl = "rmi://guiness:10991/KiWWDataProvider";
            }

            //create a server object
            KiWWDataProviderRMIf myServerObject = (KiWWDataProviderRMIf) Naming.lookup(rmiUrl);
            System.out.println("SERVER_OBJECT: " + myServerObject);

            //test connection
            System.out.println("ABOUT() =" + myServerObject.about());

            //create UserData HashMap
            String domain = "-";
            HashMap ud = new HashMap();
            ud.put("domain", domain);
            ud.put("logonName", "emu");
            ud.put("password", "123abc");
            ud.put("language", "de");

            //login to the RMIServer
            HashMap auth = myServerObject.getUserAuthorisation(domain, "emu",
                    "123abc", "myhost.kisters.de", null);
            System.out.println("LOGIN =" + auth);

            if ((null != auth) &&
                    "1".equals(auth.get(
                            KiWWDataProviderInterface.AUTHKEY_ALLOWED))) {
                System.out.println(" login permitted");

                //get supported column names for a method
                String method = "getGroupList";
                LinkedList columnnames = myServerObject.getColumnNames(domain,
                        method);

                int listsize = columnnames.size();
                int a = 0;
                System.out.println("COLUMNS for " + method + " : ");

                if (listsize > 0) {
                    for (a = 0; a < listsize; a++) {
                        HashMap map = (HashMap) columnnames.get(a);
                        System.out.println(columnnames.get(a));
                    }
                }

                //getStationList filtered by station_name and station_group_name
                String[] getsl = new String[] {
                        "station_no", "station_name", "station_id",
                        "station_group_name", "station_longname",
                        "station_group_ident", "river_name",
                    };
                SimpleRequestFilterTerm filtersl = new SimpleRequestFilterTerm();
                filtersl.addColumnReference("station_name");
                filtersl.addOperator("like");
                filtersl.addValue("W%");
                filtersl.addOperator("and");
                filtersl.addColumnReference("station_group_name");
                filtersl.addOperator("like");
                filtersl.addValue("Stationen%");

                SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
                sort.addColumnAscent("station_name");

                HashMap stationlist = myServerObject.getStationList(ud, getsl,
                        sort, filtersl, 15, 0, false, null);

                System.out.println("STATIONLIST =  " + stationlist);

                //getStationDetailList for specified station_id (or station_id list)
                String[] getdl = new String[] {
                        "station_id", "station_name", "station_longname",
                        "station_shortname", "station_carteasting",
                        "station_cartnorthing", "station_valid_from",
                        "station_no", "river_name"
                    };

                HashMap detaillist = myServerObject.getStationDetailList(ud,
                        getdl, new Long[] { new Long(53949) }, null);

                System.out.println("STATIONDETAILLIST = " + detaillist);

                //getTsInfoList filtered by station_name and ts_info_name
                String[] gettsinfo = new String[] {
                        "tsinfo_id", "tsinfo_name", "tsinfo_timelevel",
                        "tsinfo_valuetype", "tsinfo_unitname",
                        "stationparameter_name", "stationparameter_longname",
                        "station_name", "station_no", "parametertype_name",
                        "parametertype_longname", "tsinfo_group_name"
                    };

                SimpleRequestFilterTerm filter_ts_station = new SimpleRequestFilterTerm();
                filter_ts_station.addColumnReference("station_name");
                filter_ts_station.addOperator("like");
                filter_ts_station.addValue("W%");
                filter_ts_station.addOperator("and");
                filter_ts_station.addColumnReference("tsinfo_name");
                filter_ts_station.addOperator("like");
                filter_ts_station.addValue("Wangen.W.15");

                HashMap tsinfolist_name = myServerObject.getTsInfoList(ud,
                        gettsinfo, null, filter_ts_station, 15, 0, false, null);

                System.out.println(
                    "TS_INFOLIST(station_name and tsinfo_name filter) = " +
                    tsinfolist_name);

                //getTsInfoList filtered by ts_group_name
                SimpleRequestFilterTerm filter_ts_group = new SimpleRequestFilterTerm();
                filter_ts_group.addColumnReference("tsinfo_group_name");
                filter_ts_group.addOperator("like");
                filter_ts_group.addValue("Zeit%");

                HashMap tsinfolist_group = myServerObject.getTsInfoList(ud,
                        gettsinfo, null, filter_ts_group, 15, 0, false, null);

                System.out.println("TS_INFOLIST(group filter) = " +
                    tsinfolist_group);

                //getTsData for specified timeseries (or ts list)
                HashMap gettsdata = myServerObject.getTsData(ud,
                        new long[] { (512062925) }, from, to, null);

                System.out.println(" TS_DATA(ts_id->512062925) = ");
                gettsDatafromHash(gettsdata, "512062925");

                //getAlarmLevelList for specified timeseries (or ts list)
                HashMap alarmlevel = myServerObject.getAlarmLevelList(ud,
                        new Long[] {
                            new Long(216698), new Long(53177), new Long(53755)
                        }, null);

                System.out.println("ALARMLEVEL = " + alarmlevel);

                //getGroupList filtered by group_name and supergroup_mame
                SimpleRequestFilterTerm filtergroup = new SimpleRequestFilterTerm();
                filtergroup.addColumnReference("group_name");
                filtergroup.addOperator("like");
                filtergroup.addValue("Zeit%");
                filtergroup.addOperator("and");
                filtergroup.addColumnReference("supergroup_name");
                filtergroup.addOperator("like");
                filtergroup.addValue("WWW%");

                HashMap grouplist = myServerObject.getGroupList(ud,
                        new String[] { "group_id", "group_name", "supergroup_name" },
                        KiWWDataProviderInterface.TIMESERIES_GROUP, null,
                        filtergroup, 15, 0, false, null);

                System.out.println("GROUPLIST = " + grouplist);

                //getRatingTables for a specified OBJECT_TYPE and ID
                HashMap getratingtables = myServerObject.getRatingTables(ud,
                        KiWWDataProviderInterface.OBJECT_STATION,
                        new Long[] { new Long(1024009663) }, curvets);

                System.out.println("RATINGCURVES = " + getratingtables);

                //______________________________________________________________
                // create the TsData map structure
                String tsid = "513824046";

                //create essencial objects
                HashMap timeseries_map = new HashMap();

                HashMap tsID_map = new HashMap();

                HashMap ts_values_map = new HashMap();

                HashMap value_tsinfo_map = new HashMap();

                LinkedList value_tscoldesc_ll = new LinkedList();
                HashMap value_tscoldesc_map = new HashMap();

                LinkedList value_tsdata_ll = new LinkedList();

                LinkedHashMap value_tstamp_hash_lmap = new LinkedHashMap();

                //set values
                Timestamp timest1 = Timestamp.valueOf("2005-01-01 00:00:00");
                Timestamp timest2 = Timestamp.valueOf("2005-01-01 00:15:00");

                HashMap value_tsdata_map1 = new HashMap();
                HashMap value_tsdata_map2 = new HashMap();
                value_tsdata_map1.put("tsc_value0", new Double(44.6));
                value_tsdata_map1.put("status", new Long(0));
                value_tsdata_map1.put("timestamp", timest1);
                value_tsdata_map2.put("tsc_value0", new Double(46.2));
                value_tsdata_map2.put("status", new Long(0));
                value_tsdata_map2.put("timestamp", timest2);

                //compose setTsData HashMap
                ts_values_map.put(KiWWDataProviderInterface.KEY_TSINFO,
                    value_tsinfo_map);

                value_tscoldesc_ll.add(value_tscoldesc_map);
                ts_values_map.put(KiWWDataProviderInterface.KEY_TSCOLDESC,
                    value_tscoldesc_ll);

                value_tsdata_ll.add(value_tsdata_map1);
                value_tsdata_ll.add(value_tsdata_map2);
                ts_values_map.put(KiWWDataProviderInterface.KEY_TSDATA,
                    value_tsdata_ll);
                ts_values_map.put(KiWWDataProviderInterface.KEY_TSTAMP_HASH,
                    value_tstamp_hash_lmap);
                tsID_map.put(tsid, ts_values_map);
                timeseries_map.put(KiWWDataProviderInterface.KEY_TIMESERIES,
                    tsID_map);


                //setTsData
                boolean setdata = myServerObject.setTsData(ud, timeseries_map,
                        null);

                //getTsData for created ts object
                HashMap getts_data_check = myServerObject.getTsData(ud,
                        new long[] { (513824046) },
                        Timestamp.valueOf("2004-12-30 12:00:00"),
                        Timestamp.valueOf("2005-01-02 12:00:00"), null);

                System.out.println("TS_DATA(ts_id->" + tsid + ") = ");
                gettsDatafromHash(getts_data_check, tsid);


				// set Comments
			   HashMap map_objectType_as_key = new HashMap();

				LinkedList list_of_commentmaps = new LinkedList();

				//create comment hashmaps
				HashMap map_comments1 = new HashMap();
				HashMap map_comments2 = new HashMap();

				map_comments1.put("id", new Long(12345));
				map_comments1.put("fromdate", Timestamp.valueOf("2004-12-10 12:00:00"));
				map_comments1.put("untildate", Timestamp.valueOf("2004-12-25 12:00:00"));
				map_comments1.put("comment", "Testkommentar erstellt am 25.02.2005");

				map_comments2.put("id", new Long(456798));
				map_comments2.put("fromdate", Timestamp.valueOf("2000-01-10 12:00:00"));
				map_comments2.put("untildate", Timestamp.valueOf("2000-10-25 12:00:00"));
				map_comments2.put("comment", "Testkommentar erstellt am 25.02.2005");

				//add elements to list
				list_of_commentmaps.add(map_comments1);
				list_of_commentmaps.add(map_comments2);

				//put list into hashmap
				map_objectType_as_key.put(KiWWDataProviderInterface.OBJECT_TIMESERIES,
					list_of_commentmaps);
					
				boolean returnvalue = myServerObject.setComments(ud, map_objectType_as_key, null);


                //______________________________________________________________
                //logout
                myServerObject.logout(ud, null);
                System.out.println("logout");
            } else {
                System.out.println("login denied");
            }
        } catch (Exception e) {
            System.err.println("error=" + e);
            e.printStackTrace();
        }
    }

    /**
     * extract timeseries data from HashMap
     * @param map HashMap created by <code>KiWWDataProviderRMIf.getTsData()</code>
     * @param tsid time series ID to extract
     */
    public static void gettsDatafromHash(HashMap map, String tsid) {
        HashMap timeseries = (HashMap) map.get(KiWWDataProviderInterface.KEY_TIMESERIES);
        HashMap ts_id = (HashMap) timeseries.get(tsid);
        
        LinkedList tsdata = (LinkedList) ts_id.get(KiWWDataProviderInterface.KEY_TSDATA);
        int i = 0;

        i = tsdata.size();

        if (i > 0) {
            for (int a = 0; a < i; a++) {
                System.out.println(tsdata.get(a));
            }
        }
    }
} // eof
