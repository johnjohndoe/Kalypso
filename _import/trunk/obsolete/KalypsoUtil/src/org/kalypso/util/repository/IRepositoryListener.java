package org.kalypso.util.repository;

/**
 * Ein Listener auf ein Repository. Wenn sich die Struktur des Repository ändert,
 * dann wird die Methode onRepositoryStructureChanged aufgerifen.
 * 
 * @author schlienger
 */
public interface IRepositoryListener
{
  public void onRepositoryStructureChanged();
}
