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
 * getTsInfoList Wiski Call
 * <p>
 * Liefert Metainformation über WISKI-Zeitreihen
 * <p>
 * <b>2006-06-06 Ergänzung für Tageswerte </b>
 * <ul>
 * <li>tsinfo_begin_of als timestamp, dessen Time-Anteil den Beginn der Integrationszeit des Tageswertes Beschreibt
 * (z.B. 07:30 oder ähnlich)
 * <li>tsinfo_offset_of als long, welcher beschreibt, ob die Quellwerte eines Tageswertes zum Datum x vom Tag x bis x+1
 * einfliessen (offset 0) oder z.B. vom Tag x-1 bis zum Tag x (offset -1).
 * </ul>
 * Der Zeitstempel dieser Tageswerte selber ist immer 00:00 Uhr.
 * <p>
 * Extrakt aus der WDP-Dokumentation
 * <p>
 * <li>tsinfo_id: internal ID
 * <li>tsinfo_name: external name
 * <li>tsinfo_timelevel: time series time step type
 * <br>0: high resolution
 * <br>1: daily
 * <br>2: monthly
 * <br>3: year
 * <br>4: week
 * <br>5: half year
 * <br>255: other
 * 
 * <li>tsinfo_valuetype: type of data values:
 * <br>0: instantaneous
 * <br>1: mean
 * <br>2: sum
 * <br>3: min
 * <br>4: max
 * <br>255: other
 * 
 * <li>tsinfo_presicion: can be SIGNIDECI,a,b,c or DEC,a,b (and some other non standard) where a: is the number of
 * precision digits
 * 
 * <li>tsinfo_distvalue: amount of unit according to tsinfo_distunit (sec/minutes....)
 * <li>tsinfo_distunit: units to take for tsinfo_distvalue
 * <br>1: seconds
 * <br>2: minutes
 * <br>3: hours
 * <br>4: days
 * 
 * <li>tsinfo_unitname: shortname of unit
 * 
 * @author schlienger
 */
public class GetTsInfoList implements IWiskiCall
{
  /** Columns of TSINFO */
  public final static String[] COLUMNS =
  { "tsinfo_id", "tsinfo_name", "tsinfo_group_ident", "tsinfo_unitname", "tsinfo_distunit", "tsinfo_distcount", "tsinfo_precision", "tsinfo_timelevel", "tsinfo_valuetype", "tsinfo_begin_of", "tsinfo_offset_of", "parametertype_name", "parametertype_longname", "stationparameter_id", "stationparameter_name", "stationparameter_longname", "station_id", "station_no", "station_name" };

  private final SimpleRequestFilterTerm m_filter;

  private final SimpleRequestSortTerm m_sort;

  private List m_resultList;

  /**
   * Constructor with groupId. All tsInfoList objects in the group will be fetched in the wiski database.
   */
  public GetTsInfoList( final String groupId )
  {
    m_filter = new SimpleRequestFilterTerm();
    m_filter.addColumnReference( "tsinfo_group_ident" );
    m_filter.addOperator( "like" );
    m_filter.addValue( groupId );

    m_sort = new SimpleRequestSortTerm();
    m_sort.addColumnAscent( "tsinfo_name" );
  }

  /**
   * Constructor with a property-name and a value to look for within the given group
   */
  public GetTsInfoList( final String groupId, final String property, final String value )
  {
    m_filter = new SimpleRequestFilterTerm();

    m_filter.addColumnReference( "tsinfo_group_ident" );
    m_filter.addOperator( "like" );
    m_filter.addValue( groupId );

    m_filter.addOperator( "and" );

    m_filter.addColumnReference( property );
    m_filter.addOperator( "like" );
    m_filter.addValue( value );

    m_sort = new SimpleRequestSortTerm();
    m_sort.addColumnAscent( "tsinfo_name" );
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    final HashMap tsinfolist = wiski.getTsInfoList( userData, COLUMNS, m_sort, m_filter, 0, 0, false, null );

    m_resultList = (List)tsinfolist.get( KiWWDataProviderInterface.KEY_RESULT_LIST );
  }

  public List getResultList()
  {
    return m_resultList;
  }
}
