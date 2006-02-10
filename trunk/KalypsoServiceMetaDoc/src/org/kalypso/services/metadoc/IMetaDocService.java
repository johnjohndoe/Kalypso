package org.kalypso.services.metadoc;

import javax.activation.DataHandler;

import org.kalypso.metadoc.impl.MetaDocException;
import org.kalypso.services.IKalypsoService;

/**
 * IMetaDocService
 * 
 * @author schlienger
 */
public interface IMetaDocService extends IKalypsoService
{
  /**
   * Prepares the list of metadata expected by this service for a new document.
   * 
   * @param username
   *          The user who send the request. Can be used for authentification and default values
   * @return the bean that contains the Metadata to fill and give back when calling commitNewDocument( )
   */
  public PrepareBean prepareNewDocument( final String username ) throws MetaDocException;

  /**
   * Commits the document described by the metadata
   * 
   * @param docBean
   *          document descriptor
   * @param data
   *          contains the document data
   */
  public void commitNewDocument( final DocumentBean docBean, final DataHandler data ) throws MetaDocException;
}
