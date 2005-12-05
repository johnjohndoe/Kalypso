package com.bce.eind.core.profil;

import com.bce.eind.core.profil.changes.ProfilChangeHint;

/**
 * @author Belger
 */
public interface IProfilListener
{
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes );
}