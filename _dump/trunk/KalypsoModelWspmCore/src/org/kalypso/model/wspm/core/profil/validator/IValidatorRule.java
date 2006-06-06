package org.kalypso.model.wspm.core.profil.validator;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypso.model.wspm.core.profil.IProfil;


public interface IValidatorRule extends IExecutableExtension
{
  /**
   * Validiere das Profil und erzeuge einen Marker an dieser resource
   * 
   * @throws CoreException
   */
  public void validate( final IProfil profil, final IValidatorMarkerCollector helper )
      throws CoreException;
  
  public String getID();
  
  public String getDescription();
  
  public String getType();
}
