package org.kalypso.repository;

/**
 * Ein Listener auf ein Repository. Wenn sich die Struktur des Repository �ndert,
 * dann wird die Methode onRepositoryStructureChanged aufgerufen.
 * 
 * @author schlienger
 */
public interface IRepositoryListener
{
  public void onRepositoryStructureChanged();
}
