package org.kalypso.services;

import java.rmi.RemoteException;

/**
 * Dieses Interface m�ssen alle Kalypso-Services implementieren
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
