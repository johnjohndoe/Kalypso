package org.kalypso.services.user;

import java.rmi.Remote;
import java.rmi.RemoteException;

import org.kalypso.services.IKalypsoService;

/**
 * IUserService
 * 
 * @author belger
 */
public interface IUserService extends Remote, IKalypsoService
{
  /**
   * Returns the rights of the given user.
   */
  public String[] getRights( final String username ) throws RemoteException;
}
