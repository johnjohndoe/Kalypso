package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;

/**
 * @author Belger
 */
public interface IProfilListener
{
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes );
}