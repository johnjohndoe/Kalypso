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
   * f�r das erzeugen des Repository.
   * 
   * @param rep [optional] wenn null, die Factory soll sich die Konfiguration merken um
   *  eventuell sp�ter bei der Erzeugung des Repository es konfigurieren zu k�nnen.
   * 
   * @return true wenn Benutzer die Konfiguration best�tigt hat.
   */
  public boolean configureRepository( final IRepository rep );
  
  public IRepository createRepository( ) throws RepositoryException;
}
