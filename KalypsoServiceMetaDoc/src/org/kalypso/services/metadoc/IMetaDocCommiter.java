package org.kalypso.services.metadoc;

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
   * @param docBean
   * @throws MetaDocException
   */
  public void prepareMetainf( final DocBean docBean ) throws MetaDocException;
  
  /**
   * Commits the document described by the given bean.
   * 
   * @param docBean
   * @throws MetaDocException
   */
  public void commitDocument( final DocBean docBean ) throws MetaDocException;
}
