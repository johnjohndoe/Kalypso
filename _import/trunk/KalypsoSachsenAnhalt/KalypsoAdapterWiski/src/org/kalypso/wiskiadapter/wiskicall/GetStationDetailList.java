package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.LinkedList;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * GetStationDetailList
 * 
 * @author schlienger
 */
public class GetStationDetailList implements IWiskiCall
{
  private final static String[] COLUMNS = { "station_carteasting",
      "station_cartnorthing", "river_name", "river_longname", "catchment_name" };

  private final Long m_id;

  private HashMap details;

  public GetStationDetailList( final Long id )
  {
    m_id = id;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData )
      throws NoSuchObjectException, KiWWException, RemoteException
  {
    final HashMap map = wiski.getStationDetailList( userData, COLUMNS,
        new Long[] { m_id }, null );

    details = (HashMap) ((LinkedList) map.get( "resultList" )).getFirst();
  }

  public HashMap getDetails( )
  {
    return details;
  }
}
