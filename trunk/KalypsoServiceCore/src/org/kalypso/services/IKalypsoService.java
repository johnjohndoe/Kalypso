package org.kalypso.services;

import java.rmi.RemoteException;

/**
 * Dieses Interface müssen alle Kalypso-Services implementieren
 * 
 * @author belger
 */
public interface IKalypsoService
{
  /**
   * Returns the service version
   */
  public int getServiceVersion() throws RemoteException;
}
