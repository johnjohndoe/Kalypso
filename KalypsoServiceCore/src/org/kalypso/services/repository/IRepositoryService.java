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
   * Returns true if the given parent has children.
   */
  public boolean hasChildren( final ItemBean parent ) throws RemoteException;
  
  /**
   * Returns the children of the given item (parent node). Returns an empty array
   * when the parent has no children.
   */
  public ItemBean[] getChildren( final ItemBean parent ) throws RemoteException;
}
