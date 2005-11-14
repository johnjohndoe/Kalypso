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
   * @param documentIdentifier
   *          identifies the document uniquely amongst the documents that can be exported. This information might be
   *          used by IMetaDocCommiters by including it in the metadata. Third party software (such as an Information
   *          Management System) which stores the document can use this identifier along with other metadata to
   *          overwrite documents which are already present for the same forecast for instance
   * @param metadataExtensions
   *          [optional, can be null] additional metadata information that might be used by the document commiter. It
   *          can be wrapped by a {@link org.apache.commons.configuration.MapConfiguration}to read the properties from
   *          it.
   * 
   * @throws RemoteException
   */
  public void commitNewDocument( final Map metadata, final DataHandler data, final String preferredFilename,
      final String documentIdentifier, final Map metadataExtensions ) throws RemoteException;
}