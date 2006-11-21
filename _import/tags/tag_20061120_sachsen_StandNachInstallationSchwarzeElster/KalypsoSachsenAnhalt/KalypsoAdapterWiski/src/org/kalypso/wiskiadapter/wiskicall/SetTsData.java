package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * SetTsData
 * 
 * @author schlienger
 */
public class SetTsData implements IWiskiCall
{
  private final HashMap m_timeseries_map;

  private boolean m_success = false;

  public SetTsData( final HashMap timeseries_map )
  {
    m_timeseries_map = timeseries_map;
  }

  public void execute( KiWWDataProviderRMIf wiski, HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException
  {
    try
    {
      m_success = wiski.setTsData( userData, m_timeseries_map, null );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();

      throw new KiWWException( e.getLocalizedMessage() );
    }
  }

  public boolean isSuccess()
  {
    return m_success;
  }
}
