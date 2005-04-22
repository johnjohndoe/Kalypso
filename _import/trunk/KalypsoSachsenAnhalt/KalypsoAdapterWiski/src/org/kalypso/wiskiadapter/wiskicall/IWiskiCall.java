package org.kalypso.wiskiadapter.wiskicall;

import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.util.HashMap;

import de.kisters.wiski.webdataprovider.common.util.KiWWException;
import de.kisters.wiski.webdataprovider.server.KiWWDataProviderRMIf;

/**
 * IWiskiCall
 * 
 * @author schlienger
 */
public interface IWiskiCall
{
  /**
   * Performs a wiski call.
   * 
   * @throws NoSuchObjectException
   * @throws KiWWException
   * @throws RemoteException
   */
  public void execute( final KiWWDataProviderRMIf wiski,
      final HashMap userData ) throws NoSuchObjectException, KiWWException,
      RemoteException;
}
