package org.kalypso.util.loader;

import org.kalypso.util.factory.FactoryException;

/**
 * <p>Diese Factory erzeugt Objekte vom Typ {@link ILoader} anhand eines übergebenen Typs (ein String)</p>
 * <p>Zusätzlich liefert sie alle Typen, die verwendet werden können.</p>
 * @author Schlienger
 *
 */
public interface ILoaderFactory
{
  public ILoader getLoaderInstance( final String type ) throws FactoryException;

  public String[] getTypes();
}