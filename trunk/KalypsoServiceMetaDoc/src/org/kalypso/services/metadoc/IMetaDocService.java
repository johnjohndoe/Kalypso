package org.kalypso.services.metadoc;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * IMetaDocService
 * 
 * @author schlienger
 */
public interface IMetaDocService extends Remote
{
  public DocBean prepareNewDocument( final String extension ) throws RemoteException;
  
  public void commitNewDocument( final DocBean mdb ) throws RemoteException;
}
