package com.bce.eind.core.profil;

import java.util.List;

import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

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
  public void setValueFor( final IProfilPoint point, final POINT_PROPERTY pointProperty,
      final double value ) throws ProfilDataException;
  
  public void setProperty( final Object key, final Object value );

  public void setValueFor(IProfilDevider devider,Object property,Object value);
  
  public void setValueFor(final IProfilBuilding building,final BUILDING_PROPERTY property,final Object value) throws ProfilBuildingException;

  public void setValuesFor( final List<IProfilPoint> pointList,
      final POINT_PROPERTY pointProperty, final double value ) throws ProfilDataException;

}