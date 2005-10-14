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
   * Prepares the list of metadata expected by this service for a new document.
   * 
   * @param username
   *          The user who send the request. Can be used for authentification and default values
   * @return the Metadata to fill and give back when calling commitNewDocument( Map, ... )
   * 
   * @throws RemoteException
   */
  public Map prepareNewDocument( final String username ) throws RemoteException;

  /**
   * Commits the document described by the metadata
   * 
   * @param metadata
   *          describes the document, simple mapping between keys and values
   * @param data
   *          contains the document
   * @param preferredFilename
   *          the preferred file name for the underlying file. Kalypso will try to preserve it, but no guarantee is
   *          made. It is possible that extra characters are appended to it so that its uniqueness is guaranteed.
   * @param metadataExtensions
   *          [optional, can be null] additional metadata information that might be used by the document commiter. It
   *          should be a string that is loadable by a {@link org.apache.commons.configuration.PropertiesConfiguration}.
   * 
   * @throws RemoteException
   */
  public void commitNewDocument( final Map metadata, final DataHandler data, final String preferredFilename,
      final String metadataExtensions ) throws RemoteException;
}