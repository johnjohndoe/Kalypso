package org.kalypso.util.repository;

/**
 * Eingangspunkt zu einem Repository. Es liefert z.B. die ersten Items. Damit kann eine Struktur
 * aufgebaut werden.
 * 
 * @author schlienger
 */
public interface IRepository extends IRepositoryItem
{
  public String getIdentifier();
  
  public String getLocation();
}
