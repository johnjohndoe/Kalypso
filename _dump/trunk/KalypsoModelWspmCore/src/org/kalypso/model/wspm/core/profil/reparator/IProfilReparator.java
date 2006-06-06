package org.kalypso.model.wspm.core.profil.reparator;

import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypso.model.wspm.core.profil.IProfil;


/** A profile reparator is intended to repair a profile, which has inconsistent data. */
public interface IProfilReparator extends IExecutableExtension
{
  public String getID();
  
  public String getName( );
  
  public String getDescription();
  
  public boolean hasChanges( final IProfil profil );

  public boolean doChanges( final IProfil profil );
}
