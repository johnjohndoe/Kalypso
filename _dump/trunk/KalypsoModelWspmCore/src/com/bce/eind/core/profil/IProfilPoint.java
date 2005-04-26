/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil;

import java.util.Collection;

import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public interface IProfilPoint
{
  
  public double getValueFor( final POINT_PROPERTY pointProperty ) throws ProfilDataException;

  //public boolean setValueFor( final IProfilPointProperty pointProperty, final double value )
  //    throws ProfilDataException;

  public boolean hasProperty( final POINT_PROPERTY pointProperty );

  public IProfilPoint clonePoint( );

  public Collection<POINT_PROPERTY> getProperties( );
  
  public  boolean isEqualPosition( final double breite,final double hoehe) throws ProfilDataException; 
  
}  

