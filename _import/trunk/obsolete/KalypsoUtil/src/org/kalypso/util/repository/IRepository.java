package org.kalypso.util.repository;

/**
 * Eingangspunkt zu einem Repository. Es liefert z.B. die ersten Items. Damit kann eine Struktur
 * aufgebaut werden.
 * 
 * @author schlienger
 */
public interface IRepository extends IRepositoryItem
{
  public String getLocation();
  
  public void addRepositoryListener( IRepositoryListener l );
  public void removeRepositoryListener( IRepositoryListener l );
  public void fireRepositoryStructureChanged();
}
