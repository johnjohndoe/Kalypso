package org.kalypso.metadoc;

import java.util.Properties;

import org.kalypso.metadoc.beans.DocBean;


/**
 * IMetaDocCommiter. Commits documents with their meta information.
 * 
 * @author schlienger
 */
public interface IMetaDocCommiter
{
  /**
   * Prepares the DocBean with the required Metainformation.
   * 
   * @param serviceProps
   *          Properties of the MetaDoc service. Can be used to get additional
   *          properties relevant to the commiter
   * 
   * @param docBean
   * @throws MetaDocException
   */
  public void prepareMetainf( final Properties serviceProps,
      final DocBean docBean ) throws MetaDocException;

  /**
   * Commits the document described by the given bean.
   * 
   * @param serviceProps
   *          Properties of the MetaDoc service. Can be used to get additional
   *          properties relevant to the commiter
   * @param docBean
   * @throws MetaDocException
   */
  public void commitDocument( final Properties serviceProps,
      final DocBean docBean ) throws MetaDocException;
}