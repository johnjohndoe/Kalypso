package org.kalypso.repository;

/**
 * Configurator for a Repository.
 * 
 * @author schlienger
 */
public interface IRepositoryFactory
{
  /**
   * Sets the configuration string for this factory.
   * @param conf
   */
  public void setConfiguration( final String conf );
  
  public String getConfiguration();

  /**
   * Sets the readOnly flag. When true, the repository is forced in read only
   * mode.
   * @param ro
   */
  public void setReadOnly( final boolean ro );
  
  public boolean isReadOnly();

  /**
   * Vorbereitet die Konfiguration für das erzeugen des Repository.
   * 
   * @return true wenn Benutzer die Konfiguration bestätigt hat.
   * @throws RepositoryException
   */
  public boolean configureRepository() throws RepositoryException;

  /**
   * @return new repository based on the configuration
   * @throws RepositoryException
   */
  public IRepository createRepository() throws RepositoryException;
}