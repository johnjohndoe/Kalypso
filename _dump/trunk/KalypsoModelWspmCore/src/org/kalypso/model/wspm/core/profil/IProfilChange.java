package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;


public interface IProfilChange
{
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException;

  public Object getObject( );
  public POINT_PROPERTY getPointProperty();
  public Double getValue();
}
