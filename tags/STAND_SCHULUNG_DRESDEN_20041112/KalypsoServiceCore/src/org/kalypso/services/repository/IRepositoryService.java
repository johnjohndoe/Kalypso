package org.kalypso.services.repository;

import java.rmi.Remote;
import java.rmi.RemoteException;

import org.kalypso.repository.beans.ItemBean;


/**
 * General service base interface for repositories.
 * <p>
 * <b>IMPORTANT NOTE</b>: this interface is primary not intended to be directly
 * used as a webservice. It should be extended by some specific interfaces
 * that, in turn, are eligible to be real web services.
 * <p>
 * For an example see <code>IObservationService</code>.
 * 
 * @author schlienger
 */
public interface IRepositoryService extends Remote
{
  /**
   * @param parent
   * @return true if the given parent has children.
   * @throws RemoteException
   */
  public boolean hasChildren( final ItemBean parent ) throws RemoteException;
  
  /**
   * @param parent
   * @return the children of the given item (parent node). Returns an empty array
   * when the parent has no children.
   * @throws RemoteException
   */
  public ItemBean[] getChildren( final ItemBean parent ) throws RemoteException;
  
  /**
   * @param id
   * @return ItemBean if found, else null.
   * @throws RemoteException
   */
  public ItemBean findItem( final String id ) throws RemoteException;
  
  /**
   * Forces the refresh of the remote repository.
   * @throws RemoteException
   */
  public void reload() throws RemoteException;
}
