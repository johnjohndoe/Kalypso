package org.kalypso.loader;

import org.kalypso.util.factory.FactoryException;

/**
 * <p>Diese Factory erzeugt Objekte vom Typ {@link ILoader} anhand eines �bergebenen Typs (ein String)</p>
 * <p>Zus�tzlich liefert sie alle Typen, die verwendet werden k�nnen.</p>
 * @author Schlienger
 *
 */
public interface ILoaderFactory
{
  public ILoader getLoaderInstance( final String type ) throws FactoryException;

  public String[] getTypes();
}