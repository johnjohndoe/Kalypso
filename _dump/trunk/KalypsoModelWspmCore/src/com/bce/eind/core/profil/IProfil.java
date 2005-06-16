package com.bce.eind.core.profil;

import java.util.List;

/**
 * Das eigentliche Profil. Wie {@link com.bce.eind.core.profil.IPlainProfil} nur mit Event-Handling.
 * 
 * @author kimwerner
 */
public interface IProfil extends IPlainProfil
{
  //////////////////////
  // Listener support //
  //////////////////////
  public void addProfilListener( final IProfilListener pl );

  public void removeProfilListener( final IProfilListener pl );
  
  /////////////////////////
  // Convenience methods //
  /////////////////////////
  public void setValueFor( final IProfilPoint point, final ProfilPointProperty pointProperty,
      final double value ) throws ProfilDataException;

  public void setValuesFor( final List<IProfilPoint> pointList,
      final ProfilPointProperty pointProperty, final double value ) throws ProfilDataException;

}