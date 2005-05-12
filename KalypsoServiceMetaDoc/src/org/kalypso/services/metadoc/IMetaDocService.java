package org.kalypso.services.metadoc;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

import javax.activation.DataHandler;

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
   * @param username
   *          The user who send the request. Can be used for authentifikation
   *          and default values
   * @return the Metadata to fill and give back with a call to
   *         commitNewDocument( Map )
   * @throws RemoteException
   */
  public Map prepareNewDocument( final String username ) throws RemoteException;

  /**
   * Commits the new document described by the given DocBean.
   * 
   * @throws RemoteException
   */
  public void commitNewDocument( final Map metadata, final DataHandler data,
      final String fileExtension ) throws RemoteException;
}