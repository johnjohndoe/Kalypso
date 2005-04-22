package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.List;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * GetAlarmLevelList
 * 
 * @author schlienger
 */
public class GetAlarmLevelList implements IWiskiCall
{
  private List alarmList;
  private final Long m_id;

  public GetAlarmLevelList( final Long id )
  {
    m_id = id;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException, RemoteException
  {
    final HashMap alarmlevel = wiski.getAlarmLevelList(
        userData, new Long[] { m_id }, null );

    alarmList = (List) alarmlevel.get( m_id );
  }
  
  public List getAlarmList( )
  {
    return alarmList;
  }
}
