package org.kalypso.repository;

/**
 * Eingangspunkt zu einem Repository. Es liefert z.B. die ersten Items. Damit kann eine Struktur
 * aufgebaut werden.
 * 
 * @author schlienger
 */
public interface IRepository extends IRepositoryItem
{
  public String getLocation();
  
  public void addRepositoryListener( final IRepositoryListener l );
  public void removeRepositoryListener( final IRepositoryListener l );
  public void fireRepositoryStructureChanged();
}
