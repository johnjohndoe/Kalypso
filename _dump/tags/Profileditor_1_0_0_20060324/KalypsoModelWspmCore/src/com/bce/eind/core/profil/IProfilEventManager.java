package com.bce.eind.core.profil;

import com.bce.eind.core.profil.changes.ProfilChangeHint;

public interface IProfilEventManager
{
  public IProfil getProfil( );

  public void addProfilListener( final IProfilListener pl );

  public void fireProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes );

  public void removeProfilListener( final IProfilListener pl );
}