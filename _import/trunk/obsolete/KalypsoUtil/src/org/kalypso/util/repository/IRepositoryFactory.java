package org.kalypso.util.repository;


/**
 * Configurator for a Repository.
 * 
 * @author schlienger
 */
public interface IRepositoryFactory
{
  /**
   * Konfiguriert das angegebene Repository oder vorbereitet die Konfiguration 
   * für das erzeugen des Repository.
   * 
   * @param rep [optional] wenn null, die Factory soll sich die Konfiguration merken um
   *  eventuell später bei der Erzeugung des Repository es konfigurieren zu können.
   * 
   * @return true wenn Benutzer die Konfiguration bestätigt hat.
   */
  public boolean configureRepository( final IRepository rep );
  
  public IRepository createRepository( ) throws RepositoryException;
}
