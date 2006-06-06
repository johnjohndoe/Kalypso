package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;

public interface IProfilEventManager
{
  public IProfil getProfil( );

  public void addProfilListener( final IProfilListener pl );

  public void fireProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes );

  public void removeProfilListener( final IProfilListener pl );
}