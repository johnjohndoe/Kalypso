package com.bce.eind.core.profil.validator;

import org.eclipse.core.runtime.CoreException;

import com.bce.eind.core.profil.IProfil;

public interface IValidatorRule
{
  /** Validiere das Profil und erzeuge einen Marker an dieser resource  
   * @throws CoreException */
  public void validate( final IProfil profil, final IValidatorMarkerCollector helper ) throws CoreException;
}
