package org.kalypso.services.metadoc;

import java.rmi.Remote;
import java.rmi.RemoteException;

import org.kalypso.metadoc.beans.DocBean;
import org.kalypso.services.IKalypsoService;

/**
 * IMetaDocService
 * 
 * @author schlienger
 */
public interface IMetaDocService extends Remote, IKalypsoService
{
  /**
   * Prepares a DocBean for a new document with the given extension.
   * 
   * @param extension
   * @param username The user who send the request. Can be used for authentifikation and default values
   * @return the DocBean to fill and give back with a call to commitNewDocument( DocBean )
   * @throws RemoteException
   */
  public DocBean prepareNewDocument( final String extension, final String username ) throws RemoteException;
  
  /**
   * Commits the new document described by the given DocBean.
   * 
   * @param mdb
   * @throws RemoteException
   */
  public void commitNewDocument( final DocBean mdb ) throws RemoteException;
  
  /**
   * Rolls back (cancels) the creation of the document.
   *  
   * @param mdb the bean describing the document to roll back
   * @throws RemoteException
   */
  public void rollbackNewDocument( final DocBean mdb ) throws RemoteException;
}
