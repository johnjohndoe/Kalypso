package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.TimeZone;

import org.kalypso.ogc.sensor.DateRange;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * GetTsData
 * 
 * @author schlienger
 */
public class GetTsData implements IWiskiCall
{
  private final DateRange m_dr;

  private final Long m_id;

  private LinkedList data = null;

  private Integer utcOffset = null;

  public GetTsData( final Long id, final DateRange dr )
  {
    m_id = id;
    m_dr = dr;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    //  date range
    final Timestamp from = new Timestamp( m_dr.getFrom().getTime() );
    final Timestamp to = new Timestamp( m_dr.getTo().getTime() );

    //getTsData for specified timeseries (or ts list)
    final HashMap gettsdata = wiski.getTsData( userData, new long[]
    { m_id.longValue() }, from, to, null );

    final HashMap series = (HashMap)gettsdata.get( KiWWDataProviderInterface.KEY_TIMESERIES );

    final HashMap serie = (HashMap)series.get( String.valueOf( m_id ) );
    if( serie != null )
    {
      data = (LinkedList)serie.get( KiWWDataProviderInterface.KEY_TSDATA );

      utcOffset = (Integer)( (HashMap)serie.get( KiWWDataProviderInterface.KEY_TSINFO ) ).get( "utcoffset" );
    }
  }

  public LinkedList getData()
  {
    return data;
  }

  public TimeZone getTimeZone()
  {
    if( utcOffset != null )
    {
      final int offset = utcOffset.intValue();
      final String sign = offset >= 0 ? "+" : "-";
      return TimeZone.getTimeZone( "UTC" + sign + Math.abs( offset ) );
    }

    return null;
  }
}
