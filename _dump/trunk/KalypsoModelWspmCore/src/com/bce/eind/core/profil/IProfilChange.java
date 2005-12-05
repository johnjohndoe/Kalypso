package com.bce.eind.core.profil;

import com.bce.eind.core.profil.changes.ProfilChangeHint;

public interface IProfilChange
{
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException;
}
