package org.kalypso.util.repository;

import org.eclipse.swt.widgets.Shell;

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
   * @param shell
   * @param rep [optional] wenn null, die Factory soll sich die Konfiguration merken um
   *  eventuell sp�ter bei der Erzeugung des Repository es konfigurieren zu k�nnen.
   * 
   * @return true wenn Benutzer die Konfiguration best�tigt hat.
   */
  public boolean configureRepository( final Shell shell, final IRepository rep );
  
  public IRepository createRepository( ) throws RepositoryException;
}
